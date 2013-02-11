unit TestUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GfxUtils, ExtCtrls, Graphics, fgl, CoreUtils;

const
  clWhite = $FFFFFF;
  // 255 105 180
  clHotPink = (195 << 16) + (145 << 8) + 255;
  NICE_RED = $404080;

  HEART_SCALE_FACTOR = 2.8;

type

  PSupport = ^TSupport;

  THeart = class(ACompound)
    procedure Init; override;
    procedure Loop; override;
  end;

  TArm = class(ACompound)
    procedure Init; override;
    procedure Loop; override;
  private
    pSup1, pSup2: TSupport;
    state, phase: integer;
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

(* THeart *)

procedure THeart.Init;
var
  sup: TSupport;
  skin: TSkin;
  c: TPoint3D;
begin
  c := GetRotatedPoint(Centre);

  sup := TSupport.Support(c);
  skin := TSkin.Skin;
  Entities.Add(sup);
  Entities.Add(skin);

  // left, top
  sup.AddNode(Point3DFromCoords(c.x - HEART_SCALE_FACTOR * 400, c.y, c.z)); // 0
  sup.AddNode(Point3DFromCoords(c.x - HEART_SCALE_FACTOR * 200, c.y, c.z - HEART_SCALE_FACTOR * 200)); // 1
  sup.AddNode(Point3DFromCoords(c.x - HEART_SCALE_FACTOR * 200, c.y + HEART_SCALE_FACTOR * 200, c.z)); // 2
  sup.AddNode(Point3DFromCoords(c.x - HEART_SCALE_FACTOR * 200, c.y, c.z + HEART_SCALE_FACTOR * 200)); // 3

  sup.AddNode(Point3DFromCoords(c.x, c.y - HEART_SCALE_FACTOR * 600, c.z)); // 4 unique

  sup.AddNode(Point3DFromCoords(c.x, c.y + HEART_SCALE_FACTOR * 100, c.z - HEART_SCALE_FACTOR * 80)); // 5
  sup.AddNode(Point3DFromCoords(c.x, c.y + HEART_SCALE_FACTOR * 100, c.z + HEART_SCALE_FACTOR * 80)); // 6

  sup.AddNode(Point3DFromCoords(c.x, c.y - HEART_SCALE_FACTOR * 80, c.z - HEART_SCALE_FACTOR * 200)); // 7
  sup.AddNode(Point3DFromCoords(c.x, c.y - HEART_SCALE_FACTOR * 80, c.z + HEART_SCALE_FACTOR * 200)); // 8

  sup.AddNode(Point3DFromCoords(c.x, c.y - HEART_SCALE_FACTOR * 280, c.z - HEART_SCALE_FACTOR * 200)); // 9
  sup.AddNode(Point3DFromCoords(c.x, c.y - HEART_SCALE_FACTOR * 280, c.z + HEART_SCALE_FACTOR * 200)); // 10

  sup.AddNode(Point3DFromCoords(c.x + HEART_SCALE_FACTOR * 400, c.y, c.z)); // 11
  sup.AddNode(Point3DFromCoords(c.x + HEART_SCALE_FACTOR * 200, c.y, c.z - HEART_SCALE_FACTOR * 200)); // 12
  sup.AddNode(Point3DFromCoords(c.x + HEART_SCALE_FACTOR * 200, c.y + HEART_SCALE_FACTOR * 200, c.z)); // 13
  sup.AddNode(Point3DFromCoords(c.x + HEART_SCALE_FACTOR * 200, c.y, c.z + HEART_SCALE_FACTOR * 200)); // 14

  skin.BindTria(sup.Nodes[2], sup.Nodes[1], sup.Nodes[0], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[0], sup.Nodes[3], sup.Nodes[2], clWhite, clHotPink);

  skin.BindTria(sup.Nodes[2], sup.Nodes[5], sup.Nodes[1], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[2], sup.Nodes[6], sup.Nodes[5], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[2], sup.Nodes[3], sup.Nodes[6], clWhite, clHotPink);

  // left, bottom
  skin.BindTria(sup.Nodes[0], sup.Nodes[1], sup.Nodes[4], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[0], sup.Nodes[4], sup.Nodes[3], clWhite, clHotPink);

  skin.BindTria(sup.Nodes[1], sup.Nodes[5], sup.Nodes[7], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[3], sup.Nodes[8], sup.Nodes[6], clWhite, clHotPink);

  skin.BindTria(sup.Nodes[1], sup.Nodes[7], sup.Nodes[9], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[3], sup.Nodes[10], sup.Nodes[8], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[1], sup.Nodes[9], sup.Nodes[4], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[3], sup.Nodes[4], sup.Nodes[10], clWhite, clHotPink);

  // right, top
  skin.BindTria(sup.Nodes[11], sup.Nodes[13], sup.Nodes[14], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[11], sup.Nodes[12], sup.Nodes[13], clWhite, clHotPink);

  skin.BindTria(sup.Nodes[13], sup.Nodes[12], sup.Nodes[5], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[13], sup.Nodes[5], sup.Nodes[6], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[13], sup.Nodes[6], sup.Nodes[14], clWhite, clHotPink);

  // right, bottom
  skin.BindTria(sup.Nodes[4], sup.Nodes[11], sup.Nodes[14], clWhite, clHotPink);
  skin.BindTria(Sup.Nodes[4], sup.Nodes[12], sup.Nodes[11], clWhite, clHotPink);

  skin.BindTria(sup.Nodes[5], sup.Nodes[12], sup.Nodes[7], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[6], sup.Nodes[8], sup.Nodes[14], clWhite, clHotPink);

  skin.BindTria(sup.Nodes[4], sup.Nodes[9], sup.Nodes[12], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[4], sup.Nodes[14], sup.Nodes[10], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[9], sup.Nodes[7], sup.Nodes[12], clWhite, clHotPink);
  skin.BindTria(sup.Nodes[10], sup.Nodes[14], sup.Nodes[8], clWhite, clHotPink);
end;

procedure THeart.Loop;
begin
  Rotate(0, pi / 24 / 5.01, 0);
end;

(* test entities *)

(* TArm *)

procedure TArm.Init;
var
  sup1, sup2: TSupport;
  e: TSkin;
begin
  state := 0;
  phase := 0;

  sup1 := TSupport.Support(Point3DFromCoords(m_c.p.x, m_c.p.y - 200, m_c.p.z - 200));
  sup1.AddNode(Point3DFromCoords(m_c.p.x, m_c.p.y - 250, m_c.p.z - 200));
  sup1.AddNode(Point3DFromCoords(m_c.p.x, m_c.p.y - 150, m_c.p.z - 200));
  sup1.AddNode(Point3DFromCoords(m_c.p.x - 50, m_c.p.y - 200, m_c.p.z - 200));
  AddEntity(sup1);
  pSup1 := sup1;

  sup2 := TSupport.Support(Point3DFromCoords(m_c.p.x, m_c.p.y + 100, m_c.p.z + 200));
  sup2.AddNode(Point3DFromCoords(m_c.p.x, m_c.p.y + 150, m_c.p.z + 200));
  sup2.AddNode(Point3DFromCoords(m_c.p.x, m_c.p.y + 250, m_c.p.z + 200));
  sup2.AddNode(Point3DFromCoords(m_c.p.x - 50, m_c.p.y + 200, m_c.p.z + 200));
  AddEntity(sup2);
  pSup2 := sup2;

  e := TSkin.Skin;
  e.BindQuad(
          sup1.Nodes[0],
          sup1.Nodes[1],
          sup2.Nodes[1],
          sup2.Nodes[0],
          clRed,
          clRed);
  e.BindQuad(
          sup2.Nodes[2],
          sup2.Nodes[1],
          sup1.Nodes[1],
          sup1.Nodes[2],
          clRed,
          clRed);
  e.BindQuad(
          sup1.Nodes[2],
          sup1.Nodes[0],
          sup2.Nodes[0],
          sup2.Nodes[2],
          clRed,
          clRed);
  e.BindTria(
          sup1.Nodes[2],
          sup1.Nodes[1],
          sup1.Nodes[0],
          clRed,
          clRed);
  e.BindTria(
          sup2.Nodes[0],
          sup2.Nodes[1],
          sup2.Nodes[2],
          clRed,
          clRed);
  AddEntity(e);
end;

procedure TArm.Loop;
var
  p: TPoint3D;
begin
  p := GetRotatedPoint(m_c);
  p.z := p.z - 200.0;
  case state of
  0: begin
    if phase < 34 then begin
      pSup1.RotateAround(p, 0.0, 0.0, -pi / 34.0);
      inc(phase);
    end else begin
      inc(state);
      phase := 0;
    end;
    end;
  1: begin
    if phase < 30 then begin
      pSup1.Translate(Point3DFromCoords(0.0, -400.0 / 30, 0.0));
      inc(phase);
    end else begin
      state := 0;
      phase := 0;
    end;
    end;
  end;
end;

(* TTestAxis *)

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

(* TTestWE *)

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
  for i := 0 to m_geometry.Count - 1 do begin
    iface := m_geometry.Items[i];
    iface.Rotate(m_location, pi / 200.0, pi / 43.0, pi / 173.0);
    iface.Translate(Point3DFromCoords(0.0, 0.0, dz));
    (* N.B. there exists the DegToRad function *)
  end;
  incr(m_location.z, dz);
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

