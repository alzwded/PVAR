unit RoboArm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils;

const
  RoboArm_Kernel = 0;
  RoboArm_Pivot = 1
  RoboArm_Median = 2;
  RoboArm_Grapple = 3;
  RoboArm_Skin = 4;

  ROBA_DIST_KERN_MED_X = 256;
  ROBA_DIST_KERN_MED_Y = 200;
  ROBA_DIST_KERN_MED_Z = 0;
  ROBA_DIST_MED_GRAP_X = 128;
  ROBA_DIST_MED_GRAP_Y = -100;
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
    m_orientation: TFlipArmOrientation;
    m_phase: integer;
    m_bbox: TBoundingBox;
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

  pivotSup := TSupport.Support(pivotC.x, pivotC.y, pivotC.z);
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
  medianSup.AddNode(
        medianC.x
        medianC.y - ROBA_MEDIAN_HALF_WIDTH,
        medianC.z - ROBA_MEDIAN_HALF_WIDTH);
  medianSup.AddNode(
        medianC.x
        medianC.y + ROBA_MEDIAN_HALF_WIDTH,
        medianC.z - ROBA_MEDIAN_HALF_WIDTH);
  medianSup.AddNode(
        medianC.x
        medianC.y + ROBA_MEDIAN_HALF_WIDTH,
        medianC.z + ROBA_MEDIAN_HALF_WIDTH);
  medianSup.AddNode(
        medianC.x
        medianC.y - ROBA_MEDIAN_HALF_WIDTH,
        medianC.z + ROBA_MEDIAN_HALF_WIDTH);

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

  grappleSup.RotateAround(kernelSup.Location, 0, -pi/4, 0);
  medianSup.RotateAround(kernelSup.Location, 0, -pi/4, 0);
  pivotSup.RotateAround(kernelSup.Location, 0, -pi/4, 0);

  skin := TSkin.Skin;
  Entities.Add(skin);

  (* kernel *)
  skin.BindQuad(kernelSup.Nodes[]

  (* kernel-median *)
  (* median-grapple *)
  (* grapple *)
end;

end.

