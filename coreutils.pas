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
  end;

  TListOfWorldEntities = specialize TFPGObjectList<IWorldEntity>;

  IMovable = class
    procedure MoveTo(p: TPoint3D); virtual; abstract;
    procedure Translate(dp: TPoint3D); virtual; abstract;
    procedure Rotate(rx, ry, rz: real); virtual; abstract;
    procedure RotateAround(c: TPoint3D; rx, ry, rz: real); virtual; abstract;
  end;

  (* undrawable set of points floating around to act as support for skin *)
  TSupport = class(IWorldEntity, IMovable)
    constructor Support(centre: TPoint3D);
    procedure AddNode(p: TPoint3D);
    (* implementation of IWorldEntity *)
    procedure Render(engine: PJakRandrEngine); override;
    procedure Start; override;
    procedure Stop; override;
    (* implementation of IMovable *)
    procedure MoveTo(p: TPoint3D); override;
    procedure Translate(dp: TPoint3D); override;
    procedure Rotate(rx, ry, rz: real); override;
    procedure RotateAround(c: TPoint3D; rx, ry, rz: real); override;
  private
    m_nodes: array of TRealPoint3D;
    m_c: TRealPoint3D;
    m_n: integer;
  end;

  AWorldEntity = class(IWorldEntity)
    constructor AWorldEntity(location: TPoint3D);
    destructor Destroy; override;
    (* implementation of IWorldEntity *)
    procedure Render(engine: PJakRandrEngine); override;
    procedure Start; override;
    procedure Stop; override;
  private
    m_geometry: TEntity3DList;
    m_c: TRealPoint3D;
  public
    property Geometry: TEntity3DList read m_geometry write m_geometry;
    property Location: TRealPoitn3D read m_c write m_c;
  end;

  (* flexible skin *)
  TSkin = class(AWorldEntity)
    procedure BindTria(p1, p2, p3: PRealPoint3D; contourColour, fillColour: TColor);
    procedure BindQuad(p1, p2, p3, p4: PRealPoint3D; contourColour, fillColour: TColor);
  end;

  (* rigid entity *)
  TPart = class(AWorldEntity, IMovable)
    constructor Part(location: TPoint3D);
    procedure InitMesh; virtual; (* called right before returning from ctor*)
    (* implementation of IMovable *)
    procedure MoveTo(p: TPoint3D); override;
    procedure Translate(dp: TPoint3D); override;
    procedure Rotate(rx, ry, rz: real); override;
    procedure RotateAround(c: TPoint3D; rx, ry, rz: real); override;
  end;

  (* rigid entity with AI *)
  TSentientEntity = class(TPart)
    constructor SentientEntity(location: TPoint3D; interval: integer);
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

  TTestAxis = class(IWorldEntity)
    constructor Create;
    destructor Destroy; override;
    (* implementation of IWorldEntity *)
    procedure Render(engine: PJakRandrEngine); override;
    procedure Start; override;
    procedure Stop; override;
  private
    m_geometry: TEntity3DList;
  end;

  TTestWE = class(IWorldEntity)
    constructor Create(location: TPoint3D; state, phase: integer);
    destructor Destroy; override;
    (* implementation of IWorldEntity *)
    procedure Render(engine: PJakRandrEngine); override;
    procedure Start; override;
    procedure Stop; override;
  protected
    (* called on clock tick *)
    procedure Loop;
  private
    (* handler for m_clock *)
    procedure OnTimer(Sender: TObject);
  protected
    m_location: TPoint3D;
    m_geometry: TEntity3DList;
    m_state, m_phase: integer;
  private
    m_clock: TTimer;

    function GetInterval: Cardinal;
    procedure SetInterval(value: Cardinal);
  public
    property Interval: Cardinal read GetInterval write SetInterval;
  end;

implementation

destructor TTestAxis.Destroy;
begin
 m_geometry.Clear;
 m_geometry.Free;
end;

constructor TTestAxis.Create;
var
  e: IEntity3D;
begin
 m_geometry := TEntity3DList.Create;

 e := TLine.Line(
        Point3DFromCoords(0.0, 0.0, 0.0),
        Point3DFromCoords(1000.0, 0.0, 0.0));
 (e as TLine).ContourColour := clWhite;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(0.0, 0.0, 0.0),
        Point3DFromCoords(0.0, 1000.0, 0.0));
 (e as TLine).ContourColour := clWhite;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(0.0, 0.0, 0.0),
        Point3DFromCoords(0.0, 0.0, 1000.0));
 (e as TLine).ContourColour := clWhite;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(0.0, 0.0, 1000.0),
        Point3DFromCoords(0.0, 180.0, 1000.0));
 (e as TLine).ContourColour := clAqua;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(0.0, 180.0, 1000.0),
        Point3DFromCoords(1000.0, 180.0, 1000.0));
 (e as TLine).ContourColour := clAqua;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(1000.0, 180.0, 1000.0),
        Point3DFromCoords(1000.0, 0.0, 1000.0));
 (e as TLine).ContourColour := clAqua;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(1000.0, 0.0, 1000.0),
        Point3DFromCoords(0.0, 0.0, 1000.0));
 (e as TLine).ContourColour := clAqua;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(1000.0, 0.0, 1000.0),
        Point3DFromCoords(1000.0, 0.0, 0.0));
 (e as TLine).ContourColour := clAqua;
 m_geometry.Add(e);




 e := TLine.Line(
        Point3DFromCoords(0.0, 0.0, 0.0),
        Point3DFromCoords(0.0, 0.0, -1000.0));
 (e as TLine).ContourColour := clRed;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(1000.0, 0.0, -1000.0),
        Point3DFromCoords(0.0, 0.0, -1000.0));
 (e as TLine).ContourColour := clRed;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(1000.0, 0.0, 0.0),
        Point3DFromCoords(1000.0, 0.0, -1000.0));
 (e as TLine).ContourColour := clRed;
 m_geometry.Add(e);



 e := TLine.Line(
        Point3DFromCoords(1000.0, 180.0, -1000.0),
        Point3DFromCoords(1000.0, 0.0, -1000.0));
 (e as TLine).ContourColour := clRed;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(1000.0, 180.0, -1000.0),
        Point3DFromCoords(0.0, 180.0, -1000.0));
 (e as TLine).ContourColour := clRed;
 m_geometry.Add(e);

 e := TLine.Line(
        Point3DFromCoords(0.0, 0.0, -1000.0),
        Point3DFromCoords(0.0, 180.0, -1000.0));
 (e as TLine).ContourColour := clRed;
 m_geometry.Add(e);
end;

procedure TTestAxis.Start;
begin
end;

procedure TTestAxis.Stop;
begin
end;

procedure TTestAxis.Render(engine: PJakRandrEngine);
var
  i: integer;
begin
  if engine = Nil then
    Raise Exception.Create('NULL engine parameter provided!');

  for i := 0 to m_geometry.Count - 1 do begin
    engine^.AddEntity(m_geometry[i]);
  end;
end;

constructor TTestWE.Create(location: TPoint3D; state, phase: integer);
var
  e: IEntity3D;
begin
  m_location := location;
  m_state := state;
  m_phase := phase;

  m_clock := TTimer.Create(Nil);
  m_clock.Enabled := true;
  m_clock.OnTimer := @OnTimer;
  m_clock.Interval := 20; (*20;*)

  m_geometry := TEntity3DList.Create;

  (*
  // test2
  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x + 100.0,
                m_location.y,
                m_location.z + 600.0),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y,
                m_location.z + 600.0),
        Point3DFromCoords(
                m_location.x + 100.0,
                m_location.y + 100,
                m_location.z + 600.0));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clBlue;
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x - 100.0,
                m_location.y,
                m_location.z - 600.0),
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y,
                m_location.z - 600.0),
        Point3DFromCoords(
                m_location.x - 100.0,
                m_location.y + 100,
                m_location.z - 600.0));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clRed;
  m_geometry.Add(e);

  exit;
            *)

  (*
  //test
  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x,
                m_location.y + 50.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z - 200.0),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 150.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clBlue;
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clRed;
  m_geometry.Add(e);

  exit;
  *)

  (*
    /\
   / |\
  /__|_\
  \  | /
   \ |/
    \/
  *)
  (* top side *)
  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z - 200.0));
  (e as TPolygon).ContourColour := clPurple;
  //(e as TPolygon).FillColour := RGBToColor(64, 32, 32);
  (e as TPolygon).FillColour := RGBToColor(128, 64, 64);
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z - 200.0),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  //(e as TPolygon).FillColour := RGBToColor(96, 48, 48);
  (e as TPolygon).FillColour := RGBToColor(128, 64, 64);
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z + 200.0));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := RGBToColor(128, 64, 64);
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z + 200.0),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  //(e as TPolygon).FillColour := RGBToColor(196, 96, 96);
  (e as TPolygon).FillColour := RGBToColor(128, 64, 64);
  m_geometry.Add(e);

  (* bottom side *)
  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z - 200.0),
        Point3DFromCoords(
                m_location.x,
                m_location.y - 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  //(e as TPolygon).FillColour := RGBToColor(32, 32, 64);
  (e as TPolygon).FillColour := RGBToColor(128, 64, 64);
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z - 200.0),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y - 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  //(e as TPolygon).FillColour := RGBToColor(48, 48, 96);
  (e as TPolygon).FillColour := RGBToColor(128, 64, 64);
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x,
                m_location.y - 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z + 200.0));
  (e as TPolygon).ContourColour := clPurple;
  //(e as TPolygon).FillColour := RGBToColor(64, 64, 128);
  (e as TPolygon).FillColour := RGBToColor(128, 64, 64);
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x,
                m_location.y - 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z + 200.0),
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  //(e as TPolygon).FillColour := RGBToColor(96, 96, 196);
  (e as TPolygon).FillColour := RGBToColor(128, 64, 64);
  m_geometry.Add(e);

  // flaps
  e := TPolygon.Quad(
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y + 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y + 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y + 400.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y + 400.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := RGBToColor(96, 96, 200);
  m_geometry.Add(e);
  e := TPolygon.Quad(
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y + 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y + 400.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y + 400.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y + 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := RGBToColor(96, 96, 200);
  m_geometry.Add(e);

  e := TPolygon.Quad(
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y - 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y - 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y - 400.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y - 400.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := RGBToColor(96, 96, 200);
  m_geometry.Add(e);
  e := TPolygon.Quad(
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y - 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y - 400.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y - 400.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y - 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := RGBToColor(96, 96, 200);
  m_geometry.Add(e);

end;

destructor TTestWE.Destroy;
begin
  m_clock.Free;
  m_geometry.Clear;
  m_geometry.Free;
end;

function TTestWE.GetInterval: Cardinal;
begin
  GetInterval := m_clock.Interval;
end;

procedure TTestWE.SetInterval(value: Cardinal);
begin
  m_clock.Interval := value;
end;

procedure TTestWE.OnTimer(Sender: TObject);
begin
  Loop;
end;

procedure TTestWE.Render(engine: PJakRandrEngine);
var
  i: integer;
begin
  if engine = Nil then
    Raise Exception.Create('NULL engine parameter provided!');

  for i := 0 to m_geometry.Count - 1 do begin
    engine^.AddEntity(m_geometry.Items[i]);
  end;
end;

procedure TTestWE.Start;
begin
  m_clock.Enabled := true;
end;

procedure TTestWE.Stop;
begin
  m_clock.Enabled := false;
end;

procedure TTestWE.Loop;
var
  i: integer;
  iface: IEntity3D;
  dz: real;
begin
  inc(m_phase);
  if m_phase >= 400 then begin
    m_state := 1 - m_state;
    m_phase := 0;
  end;
  if m_state = 0 then
    dz := 3.0
  else
    dz := -3.0;
(*$IFDEF DEBUG_AILOOP*)
  writeln('------');
  writeln('Entities before rotation: ');
  writeln('------');
  for i := 0 to m_geometry.Count - 1 do begin
    write('  ');
    iface := m_geometry.Items[i];
    iface.Dump;
  end;
  writeln('======');
 (*$ENDIF*)
  incr(m_location.z, dz);
  for i := 0 to m_geometry.Count - 1 do begin
    iface := m_geometry.Items[i];
    iface.Rotate(m_location, pi / 200.0, pi / 43.0, pi / 173.0);
    iface.Translate(Point3DFromCoords(0.0, 0.0, dz));
    (* N.B. there exists the DegToRad function *)
  end;
(*$IFDEF DEBUG_AILOOP*)
  writeln('------');
  writeln('Entities after rotation: ');
  writeln('------');
  for i := 0 to m_geometry.Count - 1 do begin
    write('  ');
    iface := m_geometry.Items[i];
    iface.Dump;
  end;
  writeln('======');
 (*$ENDIF*)
end;

end.

