unit LegPutOnner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GfxUtils, CoreUtils, BuildableRobot;

const
  NICE_YELLOW = 10351863; // 9D F4 F7
  NICE_GREEN = 5959770; // 5A F0 5A

  LPO_HALF_WAIT = 35;

type
  TLegPutOnner = class(ASticker)
    procedure Init; override;
    procedure Loop; override;
    function GetBoundingBox: PBoundingBox; override;
  private
    m_phase: integer;
    m_bbox: TBoundingBox;
    m_lspawnPoint: TPoint3D;
    m_rspawnPoint: TPoint3D;
  end;

implementation

procedure TLegPutOnner.Init;
var
  part: TPart;
  e: TPolygon;
  rp: TPoint3D;
begin
  m_phase := -1;

  rp := GetRotatedPoint(Centre);

  m_bbox.p1 := Point3DFromCoords(rp.x, rp.y - 100, rp.z - 100 - 200);
  m_bbox.p2 := Point3DFromCoords(rp.x + 30, rp.y + 100, rp.z + 100 - 200);

  part := TPart.Part(rp);
  Entities.Add(part);

  m_rspawnPoint := Point3DFromCoords(rp.x - 30, rp.y, rp.z - 200);
  m_lspawnPoint := Point3DFromCoords(rp.x - 30, rp.y, rp.z - 200 + 50);

  (* top *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x - 10, rp.y + 10, rp.z + 10),
        Point3DFromCoords(rp.x + 10, rp.y + 10, rp.z + 10),
        Point3DFromCoords(rp.x + 10, rp.y + 10, rp.z - 200),
        Point3DFromCoords(rp.x - 10, rp.y + 10, rp.z - 200));
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  part.Geometry.Add(e);
  (* bottom *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z + 10),
        Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z - 200),
        Point3DFromCoords(rp.x + 10, rp.y - 10, rp.z - 200),
        Point3DFromCoords(rp.x + 10, rp.y - 10, rp.z + 10));
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  part.Geometry.Add(e);
  (* back *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x - 10, rp.y + 10, rp.z - 200),
        Point3DFromCoords(rp.x + 10, rp.y + 10, rp.z - 200),
        Point3DFromCoords(rp.x + 10, rp.y - 10, rp.z - 200),
        Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z - 200));
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  part.Geometry.Add(e);
  (* front *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x - 10, rp.y + 10, rp.z + 10),
        Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z + 10),
        Point3DFromCoords(rp.x + 10, rp.y - 10, rp.z + 10),
        Point3DFromCoords(rp.x + 10, rp.y + 10, rp.z + 10));
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  part.Geometry.Add(e);
  (* left *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x - 10, rp.y + 10, rp.z - 200),
        Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z - 200),
        Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z + 10),
        Point3DFromCoords(rp.x - 10, rp.y + 10, rp.z + 10));
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  part.Geometry.Add(e);
  (* right *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x + 10, rp.y + 10, rp.z - 200),
        Point3DFromCoords(rp.x + 10, rp.y + 10, rp.z + 10),
        Point3DFromCoords(rp.x + 10, rp.y - 10, rp.z + 10),
        Point3DFromCoords(rp.x + 10, rp.y - 10, rp.z - 200));
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  part.Geometry.Add(e);

  (* kernel *)
  // left
  e := TPolygon.Triangle(
        Point3DFromCoords(rp.x, rp.y - 300, rp.z),
        Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z + 10),
        Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z - 10));
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  part.Geometry.Add(e);
  // right
  e := TPolygon.Triangle(
        Point3DFromCoords(rp.x, rp.y - 300, rp.z),
        Point3DFromCoords(rp.x + 10, rp.y - 10, rp.z - 10),
        Point3DFromCoords(rp.x + 10, rp.y - 10, rp.z + 10));
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  part.Geometry.Add(e);
  // back
  e := TPolygon.Triangle(
        Point3DFromCoords(rp.x + 10, rp.y - 10, rp.z - 10),
        Point3DFromCoords(rp.x, rp.y - 300, rp.z),
        Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z - 10));
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  part.Geometry.Add(e);
  // front
  e := TPolygon.Triangle(
        Point3DFromCoords(rp.x, rp.y - 300, rp.z),
        Point3DFromCoords(rp.x + 10, rp.y - 10, rp.z + 10),
        Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z + 10));
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  part.Geometry.Add(e);
end;

procedure TLegPutOnner.Loop;
var
  rleg, lleg: TRobotPart;
  e: IWorldEntity;
begin
  if m_phase < 0 then begin
    // check for new trigger
    if TryGrab(GetBoundingBox, False, e) then begin
      if e is TBuildableRobot then begin
        rleg := TRobotPart.RobotPart(m_rspawnPoint, rptRightLeg);
        rleg.Rotate(-pi/3, 0, 0);
        lleg := TRobotPart.RobotPart(m_lspawnPoint, rptLeftLeg);
        lleg.Rotate(pi/3, 0, 0);
        if not TryStick(GetBoundingBox, True, rleg) then
          rleg.Free;
        if not TryStick(GetBoundingBox, True, lleg) then
          lleg.Free;
        m_phase := 0;
      end;
    end;
  end else begin
    // retreat and return
    if m_phase < LPO_HALF_WAIT then begin
      Entities[0].Rotate(0, (-pi/2)/LPO_HALF_WAIT, 0);
      inc(m_phase);
    end else if m_phase < 3 * LPO_HALF_WAIT then begin
      Entities[0].Rotate(0, (pi/2)/(2*LPO_HALF_WAIT), 0);
      inc(m_phase);
    end else
      m_phase := -1;
  end;
end;

function TLegPutOnner.GetBoundingBox: PBoundingBox;
begin
  GetBoundingBox := @m_bbox;
end;

end.

