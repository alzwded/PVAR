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
  m_clock.Enabled := false;
  m_clock.OnTimer := @OnTimer;
  m_clock.Interval := 20;

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
  (e as TPolygon).ContourColour := clBlack;
  (e as TPolygon).FillColour := clYellow;
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
  (e as TPolygon).ContourColour := clBlack;
  (e as TPolygon).FillColour := clYellow;
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
  (e as TPolygon).ContourColour := clBlack;
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
  (e as TPolygon).ContourColour := clBlack;
  (e as TPolygon).FillColour := clYellow;
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
  (e as TPolygon).ContourColour := clBlack;
  (e as TPolygon).FillColour := clYellow;
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
  (e as TPolygon).ContourColour := clBlack;
  (e as TPolygon).FillColour := clYellow;
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
  (e as TPolygon).ContourColour := clBlack;
  (e as TPolygon).FillColour := clYellow;
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
  (e as TPolygon).ContourColour := clBlack;
  (e as TPolygon).FillColour := clYellow;
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

  (* engine^.BeginScene; NO -- called one level above *)

  for i := 1 to m_geometry.Count do begin
    engine^.AddEntity(m_geometry.Items[i]);
  end;

  (* engine^.CommitScene; NO called up one level *)
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
  for i := 1 to m_geometry.Count do begin
    iface := m_geometry.Items[i];
    iface.Rotate(m_location, 0.0, pi / 10.0, 0.0);
    (* N.B. there exists the DegToRad function *)
  end;
end;

end.

