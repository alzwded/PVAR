unit RoboArm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils, Graphics;

const
  NICE_YELLOW = 10351863; // 9D F4 F7
  NICE_GREEN = 5959770; // 5A F0 5A

  RoboArm_Kernel = 0;
  RoboArm_Pivot = 1;
  RoboArm_Median = 2;
  RoboArm_Grapple = 3;
  RoboArm_Skin = 4;

  ROBA_PITCH_ANGLE = -pi/4;

  ROBA_DIST_KERN_MED_X = 256;
  ROBA_DIST_KERN_MED_Y = -200;
  ROBA_DIST_KERN_MED_Z = 0;
  ROBA_DIST_MED_GRAP_X = 128;
  ROBA_DIST_MED_GRAP_Y = +100;
  ROBA_DIST_MED_GRAP_Z = 0;

  ROBA_KERN_STEM_LEN = 100;
  ROBA_KERN_HALF_WIDTH = 20;
  ROBA_MEDIAN_HALF_WIDTH = 10;
  ROBA_GRAPPLE_HALF_WIDTH = 20;
  ROBA_GRAPPLE_HEIGHT = 5;

type
  TRoboArm = class(ASticker)
    constructor RoboArm(c: TPoint3D;
      ntrvl: cardinal;
      waitSteps, moveSteps, returnSteps: integer);
    procedure Init; override;
    procedure Loop; override;
    function GetBoundingBox: PBoundingBox; override;
  private
    m_waitSteps, m_moveSteps, m_returnSteps: integer;
    m_phase: integer;
    m_bbox: TBoundingBox;

    (* rotate the crank arm thingy *)
    procedure RotateThings(fwd: boolean);
  end;

implementation

constructor TRoboArm.RoboArm(
  c: TPoint3D;
  ntrvl: cardinal;
  waitSteps, moveSteps, returnSteps: integer);
begin
  m_waitSteps := waitSteps;
  m_moveSteps := moveSteps;
  m_returnSteps := returnSteps;
  inherited Sticker(c, ntrvl);
end;

procedure TRoboArm.Init;
var
  grappleSup, medianSup, kernelSup, pivotSup: TSupport;
  grappleC, medianC, kernelC, pivotC: TPoint3D;
  skin: TSkin;
  rp: TPoint3D;
begin
  rp := GetRotatedPoint(Centre);

  kernelC := rp;

  kernelSup := TSupport.Support(rp);
  Entities.Add(kernelSup);
  kernelSup.AddNode(Point3DFromCoords(rp.x - ROBA_KERN_HALF_WIDTH, rp.y, rp.z - ROBA_KERN_HALF_WIDTH));
  kernelSup.AddNode(Point3DFromCoords(rp.x + ROBA_KERN_HALF_WIDTH, rp.y, rp.z - ROBA_KERN_HALF_WIDTH));
  kernelSup.AddNode(Point3DFromCoords(rp.x + ROBA_KERN_HALF_WIDTH, rp.y, rp.z + ROBA_KERN_HALF_WIDTH));
  kernelSup.AddNode(Point3DFromCoords(rp.x - ROBA_KERN_HALF_WIDTH, rp.y, rp.z + ROBA_KERN_HALF_WIDTH));
  kernelSup.AddNode(Point3DFromCoords(rp.x - ROBA_KERN_HALF_WIDTH, rp.y - ROBA_KERN_STEM_LEN, rp.z - ROBA_KERN_HALF_WIDTH));
  kernelSup.AddNode(Point3DFromCoords(rp.x + ROBA_KERN_HALF_WIDTH, rp.y - ROBA_KERN_STEM_LEN, rp.z - ROBA_KERN_HALF_WIDTH));
  kernelSup.AddNode(Point3DFromCoords(rp.x + ROBA_KERN_HALF_WIDTH, rp.y - ROBA_KERN_STEM_LEN, rp.z + ROBA_KERN_HALF_WIDTH));
  kernelSup.AddNode(Point3DFromCoords(rp.x - ROBA_KERN_HALF_WIDTH, rp.y - ROBA_KERN_STEM_LEN, rp.z + ROBA_KERN_HALF_WIDTH));

  pivotC := rp;

  pivotSup := TSupport.Support(Point3DFromCoords(pivotC.x, pivotC.y, pivotC.z));
  Entities.Add(pivotSup);
  pivotSup.AddNode(Point3DFromCoords(
        pivotC.x - ROBA_MEDIAN_HALF_WIDTH,
        pivotC.y,
        pivotC.z - ROBA_MEDIAN_HALF_WIDTH));
  pivotSup.AddNode(Point3DFromCoords(
        pivotC.x + ROBA_MEDIAN_HALF_WIDTH,
        pivotC.y,
        pivotC.z - ROBA_MEDIAN_HALF_WIDTH));
  pivotSup.AddNode(Point3DFromCoords(
        pivotC.x + ROBA_MEDIAN_HALF_WIDTH,
        pivotC.y,
        pivotC.z + ROBA_MEDIAN_HALF_WIDTH));
  pivotSup.AddNode(Point3DFromCoords(
        pivotC.x - ROBA_MEDIAN_HALF_WIDTH,
        pivotC.y,
        pivotC.z + ROBA_MEDIAN_HALF_WIDTH));

  medianC := Point3DFromCoords(
        pivotC.x - ROBA_DIST_KERN_MED_X,
        pivotC.y - ROBA_DIST_KERN_MED_Y,
        pivotC.z - ROBA_DIST_KERN_MED_Z);

  medianSup := TSupport.Support(medianC);
  Entities.Add(medianSup);
  medianSup.AddNode(Point3DFromCoords(
        medianC.x,
        medianC.y - ROBA_MEDIAN_HALF_WIDTH,
        medianC.z - ROBA_MEDIAN_HALF_WIDTH));
  medianSup.AddNode(Point3DFromCoords(
        medianC.x,
        medianC.y + ROBA_MEDIAN_HALF_WIDTH,
        medianC.z - ROBA_MEDIAN_HALF_WIDTH));
  medianSup.AddNode(Point3DFromCoords(
        medianC.x,
        medianC.y + ROBA_MEDIAN_HALF_WIDTH,
        medianC.z + ROBA_MEDIAN_HALF_WIDTH));
  medianSup.AddNode(Point3DFromCoords(
        medianC.x,
        medianC.y - ROBA_MEDIAN_HALF_WIDTH,
        medianC.z + ROBA_MEDIAN_HALF_WIDTH));

  grappleC := Point3DFromCoords(
        medianC.x - ROBA_DIST_MED_GRAP_X,
        medianC.y - ROBA_DIST_MED_GRAP_Y,
        medianC.z - ROBA_DIST_MED_GRAP_Z);

  grappleSup := TSupport.Support(grappleC);
  Entities.Add(grappleSup);
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x - ROBA_MEDIAN_HALF_WIDTH,
        grappleC.y,
        grappleC.z - ROBA_MEDIAN_HALF_WIDTH));
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x + ROBA_MEDIAN_HALF_WIDTH,
        grappleC.y,
        grappleC.z - ROBA_MEDIAN_HALF_WIDTH));
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x + ROBA_MEDIAN_HALF_WIDTH,
        grappleC.y,
        grappleC.z + ROBA_MEDIAN_HALF_WIDTH));
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x - ROBA_MEDIAN_HALF_WIDTH,
        grappleC.y,
        grappleC.z + ROBA_MEDIAN_HALF_WIDTH));

  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x - ROBA_GRAPPLE_HALF_WIDTH,
        grappleC.y,
        grappleC.z - ROBA_GRAPPLE_HALF_WIDTH));
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x + ROBA_GRAPPLE_HALF_WIDTH,
        grappleC.y,
        grappleC.z - ROBA_GRAPPLE_HALF_WIDTH));
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x + ROBA_GRAPPLE_HALF_WIDTH,
        grappleC.y,
        grappleC.z + ROBA_GRAPPLE_HALF_WIDTH));
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x - ROBA_GRAPPLE_HALF_WIDTH,
        grappleC.y,
        grappleC.z + ROBA_GRAPPLE_HALF_WIDTH));
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x - ROBA_GRAPPLE_HALF_WIDTH,
        grappleC.y - ROBA_GRAPPLE_HEIGHT,
        grappleC.z - ROBA_GRAPPLE_HALF_WIDTH));
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x + ROBA_GRAPPLE_HALF_WIDTH,
        grappleC.y - ROBA_GRAPPLE_HEIGHT,
        grappleC.z - ROBA_GRAPPLE_HALF_WIDTH));
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x + ROBA_GRAPPLE_HALF_WIDTH,
        grappleC.y - ROBA_GRAPPLE_HEIGHT,
        grappleC.z + ROBA_GRAPPLE_HALF_WIDTH));
  grappleSup.AddNode(Point3DFromCoords(
        grappleC.x - ROBA_GRAPPLE_HALF_WIDTH,
        grappleC.y - ROBA_GRAPPLE_HEIGHT,
        grappleC.z + ROBA_GRAPPLE_HALF_WIDTH));

  grappleSup.RotateAround(GetRotatedPoint(kernelSup.Location), 0, -pi/4, 0);
  medianSup.RotateAround(GetRotatedPoint(kernelSup.Location), 0, -pi/4, 0);
  pivotSup.RotateAround(GetRotatedPoint(kernelSup.Location), 0, -pi/4, 0);
  grappleSup.Rotate(0, -pi/4, 0);
  medianSup.Rotate(0, -pi/4, 0);
  pivotSup.Rotate(0, -pi/4, 0);

  skin := TSkin.Skin;
  Entities.Add(skin);

  (* kernel *)
  // top
  skin.BindQuad(kernelSup.Nodes[3], kernelSup.Nodes[2],
        kernelSup.Nodes[1], kernelSup.Nodes[0],
        0, NICE_GREEN);
  // bottom
  skin.BindQuad(kernelSup.Nodes[4], kernelSup.Nodes[5],
        kernelSup.Nodes[6], kernelSup.Nodes[7],
        0, NICE_GREEN);
  // front
  skin.BindQuad(kernelSup.Nodes[2], kernelSup.Nodes[3],
        kernelSup.Nodes[7], kernelSup.Nodes[6],
        0, NICE_GREEN);
  // back
  skin.BindQuad(kernelSup.Nodes[0], kernelSup.Nodes[1],
        kernelSup.Nodes[5], kernelSup.Nodes[4],
        0, NICE_GREEN);
  // left
  skin.BindQuad(kernelSup.Nodes[3], kernelSup.Nodes[0],
        kernelSup.Nodes[4], kernelSup.Nodes[7],
        0, NICE_GREEN);
  // right
  skin.BindQuad(kernelSup.Nodes[1], kernelSup.Nodes[2],
        kernelSup.Nodes[6], kernelSup.Nodes[5],
        0, NICE_GREEN);

  (* kernel-median *)
  // bottom
  skin.BindQuad(pivotSup.Nodes[0], pivotSup.Nodes[3],
        medianSup.Nodes[3], medianSup.Nodes[0],
        0, NICE_YELLOW);
  // top
  skin.BindQuad(pivotSup.Nodes[2], pivotSup.Nodes[1],
        medianSup.Nodes[1], medianSup.Nodes[2],
        0, NICE_YELLOW);
  // front //9
  skin.BindQuad(pivotSup.Nodes[3], pivotSup.Nodes[2],
        medianSup.Nodes[2], medianSup.Nodes[3],
        0, NICE_YELLOW);
  // back
  skin.BindQuad(pivotSup.Nodes[1], pivotSup.Nodes[0],
        medianSup.Nodes[0], medianSup.Nodes[1],
        0, NICE_YELLOW);

  (* median-grapple *)
  // bottom
  skin.BindQuad(medianSup.Nodes[0], medianSup.Nodes[3],
        grappleSup.Nodes[2], grappleSup.Nodes[1],
        0, NICE_YELLOW);
  // top
  skin.BindQuad(medianSup.Nodes[2], medianSup.Nodes[1],
        grappleSup.Nodes[0], grappleSup.Nodes[3],
        0, NICE_YELLOW);
  // front
  skin.BindTria(medianSup.Nodes[3], medianSup.Nodes[2],
        grappleSup.Nodes[3], 0, NICE_YELLOW);
  skin.BindTria(grappleSup.Nodes[3], grappleSup.Nodes[2],
        medianSup.Nodes[3], 0, NICE_YELLOW);
  // back
  skin.BindTria(medianSup.Nodes[1], medianSup.Nodes[0],
        grappleSup.Nodes[1], 0, NICE_YELLOW);
  skin.BindTria(grappleSup.Nodes[1], grappleSup.Nodes[0],
        medianSup.Nodes[1], 0, NICE_YELLOW);

  (* grapple *)
  // top
  skin.BindQuad(grappleSup.Nodes[5], grappleSup.Nodes[4],
        grappleSup.Nodes[7], grappleSup.Nodes[6],
        0, NICE_GREEN);
  // bottom
  skin.BindQuad(grappleSup.Nodes[8], grappleSup.Nodes[9],
        grappleSup.Nodes[10], grappleSup.Nodes[11],
        0, clSilver);
  // front
  skin.BindQuad(grappleSup.Nodes[6], grappleSup.Nodes[7],
        grappleSup.Nodes[11], grappleSup.Nodes[10],
        0, NICE_GREEN);
  // back
  skin.BindQuad(grappleSup.Nodes[4], grappleSup.Nodes[5],
        grappleSup.Nodes[9], grappleSup.Nodes[8],
        0, NICE_GREEN);
  // left
  skin.BindQuad(grappleSup.Nodes[7], grappleSup.Nodes[4],
        grappleSup.Nodes[8], grappleSup.Nodes[11],
        0, NICE_GREEN);
  // right
  skin.BindQuad(grappleSup.Nodes[5], grappleSup.Nodes[6],
        grappleSup.Nodes[10], grappleSup.Nodes[9],
        0, NICE_GREEN);
end;

procedure TRoboArm.RotateThings(fwd: boolean);
var
  rp: TPoint3D;
  direction, ratio: real;
  specialAngle: real;
begin
  rp := GetRotatedPoint(Centre);

  if fwd then begin direction := 1.0; ratio := m_moveSteps; end
  else begin direction := -1.0; ratio := m_returnSteps; end;

  Entities[RoboArm_Grapple].RotateAround(rp, 0, direction * (pi/2.0) / ratio, 0);
  Entities[RoboArm_Grapple].Rotate(0, direction * (pi/2.0) / ratio, 0);
  Entities[RoboArm_Median].RotateAround(rp, 0, direction * (pi/2.0) / ratio, 0);
  Entities[RoboArm_Median].Rotate(0, direction * (pi/2.0) / ratio, 0);
  Entities[RoboArm_Pivot].RotateAround(rp, 0, direction * (pi/2.0) / ratio, 0);
  Entities[RoboArm_Pivot].Rotate(0, direction * (pi/2.0) / ratio, 0);

  if fwd then begin
    specialAngle := (ROBA_PITCH_ANGLE) / m_moveSteps;
    if (m_phase - m_waitSteps) >= (m_moveSteps div 2) then
      specialAngle := (-ROBA_PITCH_ANGLE) / (m_moveSteps + (m_moveSteps mod 2));
  end else begin
    specialAngle := (ROBA_PITCH_ANGLE) / m_returnSteps;
    if (m_phase - m_waitSteps - m_moveSteps) >= (m_returnSteps div 2) then
      specialAngle := (-ROBA_PITCH_ANGLE) / (m_returnSteps + (m_returnSteps mod 2));
  end;

  rp := GetRotatedPoint((Entities[RoboArm_Median] as TSupport).Location);

  Entities[RoboArm_Grapple].RotateAround(rp, 0, 0, specialAngle);
end;

procedure TRoboArm.Loop;
var
  e: IWorldEntity;
  i: integer;
  p0, p1, dv: TPoint3D;
begin
  m_phase := (m_phase + 1) mod (m_waitSteps + m_moveSteps + m_returnSteps);

  if m_phase < m_waitSteps then
    exit
  else if m_phase = m_waitSteps then begin
    if TryGrab(GetBoundingBox, True, e) then
      InanimateObjects.Add(e);
    p0 := GetRotatedPoint((Entities[RoboArm_Grapple] as TSupport).Nodes[0]^);
    RotateThings(true);
    p1 := GetRotatedPoint((Entities[RoboArm_Grapple] as TSupport).Nodes[0]^);
    dv := Point3DFromCoords(p1.x - p0.x, p1.y - p0.y, p1.z - p0.z);

    for i := 0 to InanimateObjects.Count - 1 do
      InanimateObjects[i].Translate(dv);
  end else if m_phase < m_waitSteps + m_moveSteps then begin
    p0 := GetRotatedPoint((Entities[RoboArm_Grapple] as TSupport).Nodes[0]^);
    RotateThings(true);
    p1 := GetRotatedPoint((Entities[RoboArm_Grapple] as TSupport).Nodes[0]^);
    dv := Point3DFromCoords(p1.x - p0.x, p1.y - p0.y, p1.z - p0.z);
    for i := 0 to InanimateObjects.Count - 1 do
      InanimateObjects[i].Translate(dv);
  end else if m_phase = m_waitSteps + m_moveSteps then begin
    if InanimateObjects.Count > 0 then begin
      TryStick(GetBoundingBox, True, InanimateObjects[0]);
    end;
    RotateThings(false);
  end else begin
    RotateThings(false);
  end;
end;

function TRoboArm.GetBoundingBox: PBoundingBox;
var
  sup: TSupport;
  rp: TPoint3D;
begin
  sup := Entities[RoboArm_Grapple] as TSupport;
  rp := GetRotatedPoint(sup.Nodes[8]^);
  m_bbox.p1 := Point3DFromCoords(rp.x, rp.y - 100, rp.z);
  m_bbox.p2 := GetRotatedPoint(sup.Nodes[10]^);
  GetBoundingBox := @m_bbox;
end;

end.

