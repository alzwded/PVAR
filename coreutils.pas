unit CoreUtils;

{$mode objfpc}{$H+}

(*$UNDEF DEBUG_AILOOP*)

interface

uses
  Classes, SysUtils, GfxUtils, ExtCtrls, Graphics, fgl;

type
  IWorldEntity = interface(IInterface)
    procedure Render(engine: PJakRandrEngine);
    procedure Start;
    procedure Stop;
  end;

  TListOfWorldEntities = specialize TFPGInterfacedObjectList<IWorldEntity>;

  TTestAxis = class(TInterfacedObject, IWorldEntity)
    constructor Create;
    destructor Destroy; override;
    (* implementation of IWorldEntity *)
    procedure Render(engine: PJakRandrEngine);
    procedure Start;
    procedure Stop;
  private
    m_geometry: TEntity3DList;
  end;

  TTestWE = class(TInterfacedObject, IWorldEntity)
    constructor Create(location: TPoint3D; state, phase: integer);
    destructor Destroy; override;
    (* implementation of IWorldEntity *)
    procedure Render(engine: PJakRandrEngine);
    procedure Start;
    procedure Stop;
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
var
  i: integer;
begin
  for i := 0 to m_geometry.Count - 1 do
    m_geometry[i].Free;

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
                m_location.y,
                m_location.z - 200.0),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  //(e as TPolygon).FillColour := RGBToColor(64, 32, 32);
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
  (e as TPolygon).FillColour := RGBToColor(64, 64, 128);
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
  (e as TPolygon).FillColour := RGBToColor(64, 64, 128);
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x + 200.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y - 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z + 200.0));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := RGBToColor(64, 64, 128);
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z + 200.0),
        Point3DFromCoords(
                m_location.x,
                m_location.y - 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x - 200.0,
                m_location.y,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  //(e as TPolygon).FillColour := RGBToColor(96, 96, 196);
  (e as TPolygon).FillColour := RGBToColor(64, 64, 128);
  m_geometry.Add(e);
end;

destructor TTestWE.Destroy;
var
  i: integer;
begin
  m_clock.Free;
  for i := 0 to m_geometry.Count - 1 do
    m_geometry[i].Free;
  m_geometry.Clear;
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

