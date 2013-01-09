unit BuildableRobot;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils, GrimReaper;

const
  NICE_YELLOW = 10351863; // 9D F4 F7
  NICE_GREEN = 5959770; // 5A F0 5A

  BR_LA_S = 0;
  BR_RA_S = 1;
  BR_L_S = 2;
  BR_H_S = 3;

  BR_ARM_TO_WAIST_OFFSET = 57;
  BR_LEG_TO_WAIST_OFFSET = 0;
  BR_HEAD_TO_WAIST_OFFSET = 110;
  BR_HALF_WIDTH = 42;

type
  TRobotPartType = (
        rptLeftArm,
        rptRightArm,
        rptLeftLeg,
        rptRightLeg,
        rptHead
  );

  TRobotPart = class(TPart)
    constructor RobotPart(cntr: TPoint3D; leType: TRobotPartType);
    procedure InitMesh; override;
    function GetBoundingBox: PBoundingBox; override;
  private
    m_type: TRobotPartType;
    m_bbox: TBoundingBox;
  public
    property MyType: TRobotPartType read m_type;
  private
    procedure GetAArm;
    procedure GetALeg;
    procedure GetAHead;
  end;

  TBuildableRobot = class(ACompound)
    procedure Stick(e: IWorldEntity);
    procedure Init; override;
    procedure Loop; override;
    function GetBoundingBox: PBoundingBox; override;
  private
    m_leftArm, m_rightArm: TRobotPart;
    m_leftLeg, m_rightLeg: TRobotPart;
    m_head: TRobotPart;
    m_bbox: TBoundingBox;
    m_disabled: boolean;
    m_phase: integer;
  end;

  TRobotLifeGiver = class(AGrabber)
    constructor LifeGiver(cntr: TPoint3D; ntrvl: cardinal);
    procedure Loop; override;
    procedure Init; override;
  private
    m_bbox: TBoundingBox;
  end;

implementation

(* TRobotLifeGiver *)

constructor TRobotLifeGiver.LifeGiver(cntr: TPoint3D; ntrvl: cardinal);
begin
  m_bbox.p1 := Point3DFromCoords(cntr.x - 50, cntr.y - 50, cntr.z - 50);
  m_bbox.p2 := Point3DFromCoords(cntr.x + 50, cntr.y + 50, cntr.z + 50);
  inherited Grabber(cntr, ntrvl);
end;

procedure TRobotLifeGiver.Loop;
var
  e: IWorldEntity;
begin
  if TryGrab(@m_bbox, False, e) then
    if e is TBuildableRobot then
      (e as TBuildableRobot).m_disabled := false;
end;

procedure TRobotLifeGiver.Init;
var
  part: TPart;
  e: TPolygon;
  rp: TPoint3D;
begin
  rp := GetRotatedPoint(Centre);
  decr(rp.x, 100);

  part := TPart.Part(rp);
  Entities.Add(part);

  e := TPolygon.Triangle(
        Point3DFromCoords(rp.x, rp.y + 100, rp.z),
        Point3DFromCoords(rp.x + 30 * sqrt(2), rp.y + 100, rp.z - 15),
        Point3DFromCoords(rp.x + 30 * sqrt(2), rp.y + 100, rp.z + 15));
  e.ContourColour := 0;
  e.FillColour := $F7F49D;
  part.Geometry.Add(e);

  e := TPolygon.Triangle(
        Point3DFromCoords(rp.x, rp.y + 100, rp.z),
        Point3DFromCoords(rp.x + 30 * sqrt(2), rp.y + 100, rp.z + 15),
        Point3DFromCoords(rp.x + 30 * sqrt(2), rp.y + 100, rp.z - 15));
  e.ContourColour := 0;
  e.FillColour := $F7F49D;
  part.Geometry.Add(e);
end;

(* TRobotPart *)

constructor TRobotPart.RobotPart(cntr: TPoint3D; leType: TRobotPartType);
begin
  m_type := leType;
  inherited Part(cntr);
end;

procedure TRobotPart.GetAArm;
var
  e: TPolygon;
  rp: TPoint3D;
begin
  rp := GetRotatedPoint(Location);

  e := TPolygon.Quad(
        Point3DFromCoords(rp.x, rp.y + 20, rp.z),
        Point3DFromCoords(rp.x, rp.y, rp.z),
        Point3DFromCoords(rp.x + 100, rp.y, rp.z),
        Point3DFromCoords(rp.x + 100, rp.y + 20, rp.z)
  );
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  Geometry.Add(e);

  e := TPolygon.Quad(
        Point3DFromCoords(rp.x, rp.y + 20, rp.z),
        Point3DFromCoords(rp.x + 100, rp.y + 20, rp.z),
        Point3DFromCoords(rp.x + 100, rp.y, rp.z),
        Point3DFromCoords(rp.x, rp.y, rp.z)
  );
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  Geometry.Add(e);
end;

procedure TRobotPart.GetALeg;
var
  rp: TPoint3D;
  e: TPolygon;
begin
  rp := GetRotatedPoint(Location);

  e := TPolygon.Quad(
        Point3DFromCoords(rp.x, rp.y, rp.z - 20),
        Point3DFromCoords(rp.x, rp.y, rp.z + 20),
        Point3DFromCoords(rp.x + 100, rp.y, rp.z + 20),
        Point3DFromCoords(rp.x + 100, rp.y, rp.z - 20));
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  Geometry.Add(e);

  e := TPolygon.Quad(
        Point3DFromCoords(rp.x, rp.y, rp.z - 20),
        Point3DFromCoords(rp.x + 100, rp.y, rp.z - 20),
        Point3DFromCoords(rp.x + 100, rp.y, rp.z + 20),
        Point3DFromCoords(rp.x, rp.y, rp.z + 20));
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  Geometry.Add(e);
end;

function TRobotPart.GetBoundingBox: PBoundingBox;
var
  rp: TPoint3D;
begin
  rp := GetRotatedPoint(Location);
  if m_type = rptHead then begin
    m_bbox.p1 := Point3DFromCoords(rp.x, rp.y, rp.z -40);
    m_bbox.p2 := Point3DFromCoords(rp.x + 80, rp.y + 80, rp.z + 40);
  end else begin
    m_bbox.p1 := Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z - 10);
    m_bbox.p2 := Point3DFromCoords(rp.x + 10, rp.y + 10, rp.z + 10);
  end;
  GetBoundingBox := @m_bbox;
end;

procedure TRobotPart.GetAHead;
var
  rp: TPoint3D;
  e: TPolygon;
begin
  rp := GetRotatedPoint(Location);
  (*          ______           ____
  |\  side    |\  /| front     \  /
  | \         | \/ |            \/
  |  |        | || |              top
  ----        ------
  *)

  (* bottom *)
  e := TPolygon.Triangle(
        Point3DFromCoords(rp.x, rp.y, rp.z + 40),
        Point3DFromCoords(rp.x, rp.y, rp.z - 40),
        Point3DFromCoords(rp.x + 70, rp.y, rp.z));
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  Geometry.Add(e);

  (* top *)
  e := TPolygon.Triangle(
        Point3DFromCoords(rp.x, rp.y + 80, rp.z - 40),
        Point3DFromCoords(rp.x, rp.y + 80, rp.z + 40),
        Point3DFromCoords(rp.x + 70, rp.y + 40, rp.z));
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  Geometry.Add(e);

  (* back *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x, rp.y, rp.z - 40),
        Point3DFromCoords(rp.x, rp.y, rp.z + 40),
        Point3DFromCoords(rp.x, rp.y + 80, rp.z + 40),
        Point3DFromCoords(rp.x, rp.y + 80, rp.z - 40));
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  Geometry.Add(e);

  (* left *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x, rp.y, rp.z + 40),
        Point3DFromCoords(rp.x + 70, rp.y, rp.z),
        Point3DFromCoords(rp.x + 70, rp.y + 40, rp.z),
        Point3DFromCoords(rp.x, rp.y + 80, rp.z + 40));
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  Geometry.Add(e);

  (* right *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x, rp.y, rp.z - 40),
        Point3DFromCoords(rp.x, rp.y + 80, rp.z - 40),
        Point3DFromCoords(rp.x + 70, rp.y + 40, rp.z),
        Point3DFromCoords(rp.x + 70, rp.y, rp.z));
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  Geometry.Add(e);

  (* done. *)
end;

procedure TRobotPart.InitMesh;
begin
  case m_type of
  rptLeftArm: GetAArm;
  rptRightArm: GetAArm;
  rptLeftLeg: GetALeg;
  rptRightleg: GetALeg;
  rptHead: GetAHead;
  end;
end;

(* TBuildableRobot *)

procedure TBuildableRobot.Stick(e: IWorldEntity);
begin
  if e is TRobotPart then begin
    Entities.Add(e);

    case (e as TRobotPart).MyType of
    rptLeftArm: begin
      m_leftArm := e as TRobotPart;
      (Entities[BR_LA_S] as TSplosion).Splode;
      end;
    rptRightArm: begin
      m_rightArm := e as TRobotPart;
      (Entities[BR_RA_S] as TSplosion).Splode;
      end;
    rptLeftLeg: begin
      m_leftLeg := e as TRobotPart;
      (Entities[BR_L_S] as TSplosion).Splode;
      end;
    rptRightLeg: begin
      m_rightLeg := e as TRobotPart;
      (Entities[BR_L_S] as TSplosion).Splode;
      end;
    rptHead: begin
      m_head := e as TRobotPart;
      (Entities[BR_H_S] as TSplosion).Splode;
      end;
    end;
  end else
    Raise Exception.Create('Not a TRobotPart!');
end;

procedure TBuildableRobot.Init;
var
  sp: TSplosion;
  rp: TPoint3D;
  body: TPart;
  e: TPolygon;
begin
  m_disabled := true;
  m_phase := 0;

  rp := GetRotatedPoint(Centre);

  (* splojunz *)
  sp := TSplosion.Splosion(Point3DFromCoords(
        rp.x, rp.y + BR_ARM_TO_WAIST_OFFSET, rp.z + BR_HALF_WIDTH));
  Entities.Add(sp);
  sp := TSplosion.Splosion(Point3DFromCoords(
        rp.x, rp.y + BR_ARM_TO_WAIST_OFFSET, rp.z - BR_HALF_WIDTH));
  Entities.Add(sp);
  sp := TSplosion.Splosion(Point3DFromCoords(
        rp.x, rp.y + BR_LEG_TO_WAIST_OFFSET, rp.z));
  Entities.Add(sp);
  sp := TSplosion.Splosion(Point3DFromCoords(
        rp.x, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z));
  Entities.Add(sp);

  (* body *)

  body := TPart.Part(rp);
  Entities.Add(body);

  (* back *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x, rp.y, rp.z - BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y, rp.z + BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z + BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z - BR_HALF_WIDTH)
  );
  e.ContourColour := 0;
  e.FillColour := NICE_GREEN;
  body.Geometry.Add(e);
  (* front *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y, rp.z + BR_HALF_WIDTH),
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y, rp.z - BR_HALF_WIDTH),
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z - BR_HALF_WIDTH),
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z + BR_HALF_WIDTH)
  );
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  body.Geometry.Add(e);
  (* left *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y, rp.z - BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y, rp.z - BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z - BR_HALF_WIDTH),
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z - BR_HALF_WIDTH)
  );
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  body.Geometry.Add(e);
  (* right *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y, rp.z + BR_HALF_WIDTH),
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z + BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z + BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y, rp.z + BR_HALF_WIDTH)
  );
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  body.Geometry.Add(e);
  (* top *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z - BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z - BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z + BR_HALF_WIDTH),
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z + BR_HALF_WIDTH)
  );
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  body.Geometry.Add(e);
  (* bottom *)
  e := TPolygon.Quad(
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y, rp.z - BR_HALF_WIDTH),
        Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y, rp.z + BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y, rp.z + BR_HALF_WIDTH),
        Point3DFromCoords(rp.x, rp.y, rp.z - BR_HALF_WIDTH)
  );
  e.ContourColour := 0;
  e.FillColour := NICE_YELLOW;
  body.Geometry.Add(e);
end;

function TBuildableRobot.GetBoundingBox: PBoundingBox;
var
  rp: TPoint3D;
begin
  rp := GetRotatedPoint(Centre);
  m_bbox.p1 := Point3DFromCoords(rp.x, rp.y, rp.z - BR_HALF_WIDTH - 1);
  m_bbox.p2 := Point3DFromCoords(rp.x + BR_HALF_WIDTH, rp.y + BR_HEAD_TO_WAIST_OFFSET, rp.z + BR_HALF_WIDTH + 1);
  GetBoundingBox := @m_bbox;
end;

procedure TBuildableRobot.Loop;
begin
  if m_disabled then exit;

  if m_phase >= 20 then m_phase := 0;

  if m_phase < 5 then begin
    // flail larm
    if m_leftArm <> Nil then
      m_leftArm.Rotate(-pi/60, pi/60, 0);
    // flail rarm
    if m_rightArm <> Nil then
      m_rightArm.Rotate(pi/60, -pi/60, 0);
    // flail lleg
    if m_leftLeg <> Nil then
      m_leftLeg.Rotate(0, -pi/60, pi/30);
    // flail rleg
    if m_leftLeg <> Nil then
      m_leftLeg.Rotate(0, pi/60, -pi/30);
    // flail head
    if m_head <> Nil then
      m_head.Rotate(pi/30, 0, 0);
  end else if m_phase < 15 then begin
    // flail larm
    if m_leftArm <> Nil then
      m_leftArm.Rotate(pi/60, -pi/60, 0);
    // flail rarm
    if m_rightArm <> Nil then
      m_rightArm.Rotate(-pi/60, pi/60, 0);
    // flail lleg
    if m_leftLeg <> Nil then
      m_leftLeg.Rotate(0, pi/60, -pi/30);
    // flail rleg
    if m_leftLeg <> Nil then
      m_leftLeg.Rotate(0, -pi/60, pi/30);
    // flail head
    if m_head <> Nil then
      m_head.Rotate(-pi/30, 0, 0);
  end else if m_phase < 20 then begin
    // flail larm
    if m_leftArm <> Nil then
      m_leftArm.Rotate(-pi/60, pi/60, 0);
    // flail rarm
    if m_rightArm <> Nil then
      m_rightArm.Rotate(pi/60, -pi/60, 0);
    // flail lleg
    if m_leftLeg <> Nil then
      m_leftLeg.Rotate(0, -pi/60, pi/30);
    // flail rleg
    if m_leftLeg <> Nil then
      m_leftLeg.Rotate(0, pi/60, -pi/30);
    // flail head
    if m_head <> Nil then
      m_head.Rotate(pi/30, 0, 0);
  end;

  inc(m_phase);
end;

end.

