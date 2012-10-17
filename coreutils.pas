unit CoreUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GfxUtils, ExtCtrls, Graphics;

type
  IWorldEntity = interface(IInterface)
    procedure Render(engine: TJakRandrEngine);
    procedure Start;
    procedure Stop;
  end;

  TTestWE = class(TInterfacedObject, IInterface)
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
                m_location.x - 100.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z - 100.0),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clRed;
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z - 100.0),
        Point3DFromCoords(
                m_location.x + 100.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clLtGray;
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x + 100.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z + 100.0));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clYellow;
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z + 100.0),
        Point3DFromCoords(
                m_location.x,
                m_location.y + 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x - 100.0,
                m_location.y,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clGreen;
  m_geometry.Add(e);

  (* bottom side *)
  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x - 100.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z - 100.0),
        Point3DFromCoords(
                m_location.x,
                m_location.y - 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clBlue;
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z - 100.0),
        Point3DFromCoords(
                m_location.x + 100.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y - 200.0,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clTeal;
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x + 100.0,
                m_location.y,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y - 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z + 100.0));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clFuchsia;
  m_geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(
                m_location.x,
                m_location.y,
                m_location.z + 100.0),
        Point3DFromCoords(
                m_location.x,
                m_location.y - 200.0,
                m_location.z),
        Point3DFromCoords(
                m_location.x - 100.0,
                m_location.y,
                m_location.z));
  (e as TPolygon).ContourColour := clPurple;
  (e as TPolygon).FillColour := clAqua;
  m_geometry.Add(e);
end;

destructor TTestWE.Destroy;
begin
  m_clock.Free;
  m_geometry.Clear; (* _in theory_ this should clean up all items since they
                       were declared as IEntity3D *)
  (* TODO check if above comment is true *)
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
begin
(*IFDEF DEBUG_AILOOP*)
  writeln('------');
  writeln('Entities before rotation: ');
  writeln('------');
  for i := 0 to m_geometry.Count - 1 do begin
    write('  ');
    iface := m_geometry.Items[i];
    iface.Dump;
  end;
  writeln('======');
 (*ENDIF*)
  for i := 0 to m_geometry.Count - 1 do begin
    iface := m_geometry.Items[i];
    iface.Rotate(m_location, 0.0, pi / 100.0, 0.0);
    (* N.B. there exists the DegToRad function *)
  end;
(*IFDEF DEBUG_AILOOP*)
  writeln('------');
  writeln('Entities after rotation: ');
  writeln('------');
  for i := 0 to m_geometry.Count - 1 do begin
    write('  ');
    iface := m_geometry.Items[i];
    iface.Dump;
  end;
  writeln('======');
 (*ENDIF*)
end;

end.

