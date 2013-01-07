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
  BR_HALF_WIDTH = 50;

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
  end;

implementation

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
  e.ContourColour := NICE_YELLOW;
  e.FillColour := NICE_GREEN;
  Geometry.Add(e);

  e := TPolygon.Quad(
        Point3DFromCoords(rp.x, rp.y, rp.z - 20),
        Point3DFromCoords(rp.x + 100, rp.y, rp.z - 20),
        Point3DFromCoords(rp.x + 100, rp.y, rp.z + 20),
        Point3DFromCoords(rp.x, rp.y, rp.z + 20));
  e.ContourColour := NICE_YELLOW;
  e.FillColour := NICE_GREEN;
  Geometry.Add(e);
end;

function TRobotPart.GetBoundingBox: PBoundingBox;
var
  rp: TPoint3D;
begin
  rp := GetRotatedPoint(Location);
  m_bbox.p1 := Point3DFromCoords(rp.x - 10, rp.y - 10, rp.z - 10);
  m_bbox.p2 := Point3DFromCoords(rp.x + 10, rp.y + 10, rp.z + 10);
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
  rp := GetRotatedPoint(Centre);

  (* splojunz *)
  sp := TSplosion.Splosion(Point3DFromCoords(
        rp.x, rp.y + BR_ARM_TO_WAIST_OFFSET, rp.z - BR_HALF_WIDTH));
  Entities.Add(sp);
  sp := TSplosion.Splosion(Point3DFromCoords(
        rp.x, rp.y + BR_ARM_TO_WAIST_OFFSET, rp.z + BR_HALF_WIDTH));
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
  e.FillColour := NICE_YELLOW;
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
  inherited Loop;
end;

end.

