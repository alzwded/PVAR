unit CoreUtils;

{$mode objfpc}{$H+}

(*$UNDEF DEBUG_AILOOP*)

interface

uses
  Classes, SysUtils, GfxUtils, ExtCtrls, Graphics, fgl;

type
  IWorldEntity = class(TObject)
    procedure Render(engine: PJakRandrEngine); virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    (* Movable *)
    procedure MoveTo(p: TPoint3D); virtual;
    procedure Translate(dp: TPoint3D); virtual;
    procedure Rotate(rx, ry, rz: real); virtual;
    procedure RotateAround(c: TPoint3D; rx, ry, rz: real); virtual;
  end;

  TListOfWorldEntities = specialize TFPGObjectList<IWorldEntity>;

  (* undrawable set of points floating around to act as support for skin *)
  TSupport = class(IWorldEntity)
    constructor Support(centre: TPoint3D);
    destructor Destroy; override;
    procedure AddNode(p: TPoint3D);
    (* implementation of IWorldEntity *)
    procedure Render(engine: PJakRandrEngine); override;
    procedure Start; override;
    procedure Stop; override;
    (* implementation of IMovable *)
    procedure MoveTo(p: TPoint3D);
    procedure Translate(dp: TPoint3D);
    procedure Rotate(rx, ry, rz: real);
    procedure RotateAround(c: TPoint3D; rx, ry, rz: real);
  private
    m_nodes: array of TRealPoint3D;
    m_c: TRealPoint3D;
    m_n: integer;

    function GetPNode(i: integer): PRealPoint3D;
  public
    property Location: TRealPoint3D read m_c write m_c;
    property Nodes[i: integer]: PRealPoint3D read GetPNode;
  end;

  AWorldEntity = class(IWorldEntity)
    constructor AWorldEntity;
    constructor AWorldEntity(location: TPoint3D);
    destructor Destroy; override;
    (* implementation of IWorldEntity *)
    procedure Render(engine: PJakRandrEngine); override;
    procedure Start; override;
    procedure Stop; override;
  protected (* because TranslateVector needs var access *)
    m_geometry: TEntity3DList;
    m_c: TRealPoint3D;
  public
    property Geometry: TEntity3DList read m_geometry write m_geometry;
    property Location: TRealPoint3D read m_c write m_c;
  end;

  ACompound = class(IWorldEntity)
    constructor Compound(interval: cardinal);
    destructor Destroy; override;
    procedure Init; virtual;
    procedure AddEntity(e: IWorldEntity);
    procedure Loop; virtual;
    (* implementation of IWorldEntity *)
    procedure Render(engine: PJakRandrEngine); override;
    procedure Start; override;
    procedure Stop; override;
  protected
    m_entities: TListOfWorldEntities;
  private
    m_clock: TTimer;

    procedure OnClock(Sender: TObject);
  end;

  (* flexible skin *)
  TSkin = class(AWorldEntity)
    constructor Skin;
    destructor Destroy; override;
    procedure BindTria(p1, p2, p3: PRealPoint3D; contourColour, fillColour: TColor);
    procedure BindQuad(p1, p2, p3, p4: PRealPoint3D; contourColour, fillColour: TColor);
  end;

  (* rigid entity *)
  TPart = class(AWorldEntity)
    constructor Part(c: TPoint3D);
    procedure InitMesh; virtual; (* called right before returning from ctor*)
    (* implementation of IMovable *)
    procedure MoveTo(p: TPoint3D); override;
    procedure Translate(dp: TPoint3D); override;
    procedure Rotate(rx, ry, rz: real); override;
    procedure RotateAround(c: TPoint3D; rx, ry, rz: real); override;
  end;

  (* rigid entity with AI *)
  TSentientEntity = class(TPart)
    constructor SentientEntity(c: TPoint3D; interval: cardinal);
    destructor Destroy; override;
    procedure InitAI; virtual; (* called right before returning from constructor
                                  but after InitMesh *)
    procedure Loop; virtual; (* called OnClock *)
    (* implementation of IWorldEntity *)
    procedure Start; override;
    procedure Stop; override;
  private
    m_clock: TTimer;
    procedure OnClock(Sender: TObject);
  end;

implementation

(* buggery *)

procedure IWorldEntity.MoveTo(p: TPoint3D); begin end;
procedure IWorldEntity.Translate(dp: TPoint3D); begin end;
procedure IWorldEntity.Rotate(rx, ry, rz: real); begin end;
procedure IWorldEntity.RotateAround(c: TPoint3D; rx, ry, rz: real); begin end;

(* TSupport *)

constructor TSupport.Support(centre: TPoint3D);
begin
  m_c := RealPoint3DFromPoint(centre);
  m_n := 0;
end;

destructor TSupport.Destroy;
begin
  SetLength(m_nodes, 0);
end;

procedure TSupport.AddNode(p: TPoint3D);
begin
  inc(m_n);
  if m_n > High(m_nodes) then
    SetLength(m_nodes, m_n * 2);
  m_nodes[m_n - 1] := RealPoint3DFromPoint(p);
end;

procedure TSupport.Render(engine: PJakRandrEngine); begin end;
procedure TSupport.Start; begin end;
procedure TSupport.Stop; begin end;

procedure TSupport.MoveTo(p: TPoint3D);
var
  reverse: TPoint3D;
  rp: TPoint3D;
  i: integer;
begin
  rp := GetRotatedPoint(m_c);
  reverse := Point3DFromCoords(-rp.x, -rp.y, -rp.z);
  for i := 0 to m_n - 1 do begin
    TranslateVector(m_nodes[i].p, reverse);
    TranslateVector(m_nodes[i].rotationCentre, reverse);
    TranslateVector(m_nodes[i].p, p);
    TranslateVector(m_nodes[i].rotationCentre, p);
  end;
  TranslateVector(m_c.rotationCentre, reverse);
  TranslateVector(m_c.rotationCentre, p);
  m_c.p := p;
end;

procedure TSupport.Translate(dp: TPoint3D);
var
  i: integer;
begin
  for i := 0 to m_n - 1 do begin
    TranslateVector(m_nodes[i].p, dp);
    TranslateVector(m_nodes[i].rotationCentre, dp);
  end;
  TranslateVector(m_c.rotationCentre, dp);
  TranslateVector(m_c.p, dp);
end;

procedure TSupport.Rotate(rx, ry, rz: real);
var
  i: integer;
begin
  for i := 0 to m_n - 1do
    ApplyRotationToPoint(m_nodes[i], m_c, rx, ry, rz);
end;

procedure TSupport.RotateAround(c: TPoint3D; rx, ry, rz: real);
var
  i: integer;
  rc: TRealPoint3D;
  dv, p: TPoint3D;
begin
  dv := GetRotatedPoint(m_c);
  rc := RealPoint3DFromPoint(c);

  ApplyRotationToPoint(m_c, rc, rx, ry, rz);
  p := GetRotatedPoint(m_c);
  dv.x := -dv.x + p.x;
  dv.y := -dv.y + p.y;
  dv.z := -dv.z + p.z;

  for i := 0 to m_n - 1 do begin
    TranslateVector(m_nodes[i].p, dv);
    TranslateVector(m_nodes[i].rotationCentre, dv);
  end;
    //ApplyRotationToPoint(m_nodes[i], rc, rx, ry, rz);
end;

function TSupport.GetPNode(i: integer): PRealPoint3D;
begin
  if (i < 0) or (i > m_n) then
    Raise Exception.CreatE('out of range');
  GetPNode := @m_nodes[i];
end;

(* ACompound *)

constructor ACompound.Compound(interval: cardinal);
begin
  m_clock := TTimer.Create(Nil);
  m_clock.Interval := interval;
  if interval > 0 then
    m_clock.Enabled := True
  else
    m_clock.Enabled := False;
  m_clock.OnTimer := @OnClock;

  m_entities := TListOfWorldEntities.Create;

  Init;
end;

destructor ACompound.Destroy;
begin
  m_entities.Clear;
  m_entities.Free;
  m_clock.Free;

  inherited;
end;

procedure ACompound.Init; begin end;

procedure ACompound.AddEntity(e: IWorldEntity);
begin
  m_entities.Add(e);
end;

procedure ACompound.Render(engine: PJakRandrEngine);
var
  i: integer;
begin
  for i := 0 to m_entities.Count - 1 do
    m_entities[i].Render(engine);
end;

procedure ACompound.Start;
var
  i: integer;
begin
  for i := 0 to m_entities.Count - 1 do
    m_entities[i].Start;
  m_clock.Enabled := true;
end;

procedure ACompound.Stop;
var
  i: integer;
begin
  for i := 0 to m_entities.Count - 1 do
    m_entities[i].Stop;
  m_clock.Enabled := false;
end;

procedure ACompound.Loop;
var
  i: integer;
begin
  for i := 0 to m_entities.Count - 1 do
    if (m_entities[i] is TSentientEntity)
       and not (m_entities[i] as TSentientEntity).m_clock.Enabled then
      (m_entities[i] as TSentientEntity).Loop;
end;

procedure ACompound.OnClock(Sender: TObject);
begin
  Loop;
end;

(* AWorldEntity *)

constructor AWorldEntity.AWorldEntity;
begin
 m_c := RealPoint3DFromCoords(0, 0, 0);
 m_geometry := TEntity3DList.Create;
end;

constructor AWorldEntity.AWorldEntity(location: TPoint3D);
begin
 m_c := RealPoint3DFromPoint(location);
 m_geometry := TEntity3DList.Create;
end;

destructor AWorldEntity.Destroy;
begin
 m_geometry.Clear;
 m_geometry.Free;

 inherited;
end;

procedure AWorldEntity.Render(engine: PJakRandrEngine);
var
  i: integer;
begin
  if engine = Nil then
    Raise Exception.Create('NULL engine parameter provided!');

  for i := 0 to m_geometry.Count - 1 do
    engine^.AddEntity(m_geometry.Items[i]);
end;

procedure AWorldEntity.Start; begin end;

procedure AWorldEntity.Stop; begin end;

(* TSkin *)

constructor TSkin.Skin;
begin
  inherited AWorldEntity;
end;

destructor TSkin.Destroy;
begin
  inherited Destroy;
end;

procedure TSkin.BindTria(p1, p2, p3: PRealPoint3D; contourColour, fillColour: TColor);
var
  e: IEntity3D;
begin
  e := TPointerPolygon.Triangle(p1, p2, p3);
  (e as TPointerPolygon).ContourColour := contourColour;
  (e as TPointerPolygon).FillColour := fillColour;

  Geometry.Add(e);
end;

procedure TSkin.BindQuad(p1, p2, p3, p4: PRealPoint3D; contourColour, fillColour: TColor);
var
  e: IEntity3D;
begin
  e := TPointerPolygon.Quad(p1, p2, p3, p4);
  (e as TPointerPolygon).ContourColour := contourColour;
  (e as TPointerPolygon).FillColour := fillColour;

  Geometry.Add(e);
end;

(* TPart *)

constructor TPart.Part(c: TPoint3D);
begin
  inherited AWorldEntity(c); (* note to self -- can be called at the end
                                as well :-D *)
  InitMesh;
end;

procedure TPart.InitMesh; begin end;

procedure TPart.MoveTo(p: TPoint3D);
var
  i: integer;
  e: IEntity3D;
  reverse: TPoint3D;
  rp: TPoint3D;
begin
  rp := GetRotatedPoint(Location);
  reverse := Point3DFromCoords(-rp.x, -rp.y, -rp.z);
  for i := 0 to Geometry.Count - 1 do begin
    Geometry.Items[i].Translate(reverse);
    Geometry.Items[i].Translate(p);
  end;
  incr(m_c.rotationCentre.x, p.x - Location.p.x);
  incr(m_c.rotationCentre.y, p.y - Location.p.y);
  incr(m_c.rotationCentre.z, p.z - Location.p.z);
  m_c.p.x := p.x;
  m_c.p.y := p.x;
  m_c.p.z := p.x;
end;

procedure TPart.Translate(dp: TPoint3D);
var
  i: integer;
begin
  TranslateVector(m_c.p, dp);
  TranslateVector(m_c.rotationCentre, dp);
  for i := 0 to Geometry.Count - 1 do
    Geometry.Items[i].Translate(dp);
end;

procedure TPart.Rotate(rx, ry, rz: real);
var
  i: integer;
begin
  for i := 0 to Geometry.Count - 1 do
    Geometry.Items[i].Rotate(GetRotatedPoint(Location), rx, ry, rz);
end;

procedure TPart.RotateAround(c: TPoint3D; rx, ry, rz: real);
var
  i: integer;
begin
  ApplyRotationToPoint(m_c, RealPoint3DFromPoint(c), rx, ry, rz);
  for i := 0 to Geometry.Count - 1 do
    Geometry.Items[i].Rotate(c, rx, ry, rz);
end;

(* TSentientEntity *)

constructor TSentientEntity.SentientEntity(c: TPoint3D; interval: cardinal);
begin
  inherited Part(c);

  m_clock := TTimer.Create(Nil);
  m_clock.Interval := interval;
  m_clock.OnTimer := @OnClock;
  m_clock.Enabled := true;

  InitAI;
end;

destructor TSentientEntity.Destroy;
begin
  m_clock.Free;
  inherited;
end;

procedure TSentientEntity.InitAI; begin end;

procedure TSentientEntity.Start;
begin
  m_clock.Enabled := true;
end;

procedure TSentientEntity.Stop;
begin
  m_clock.Enabled := false;
end;

procedure TSentientEntity.Loop; begin end;

procedure TSentientEntity.OnClock(Sender: TObject);
begin
  Loop;
end;

end.

