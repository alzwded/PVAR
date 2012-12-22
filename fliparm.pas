unit FlipArm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils, Graphics;

const
  FlipArm_Kernel = 0;
  FlipArm_Tip = 1;
  FlipArm_Skin = 2;

type
  TFlipArmOrientation = (
        faoRight,
        faoLeft
  );

  TFlipArm = class(ASticker)
    constructor FlipArm(c: TPoint3D;
      ntrvl: cardinal;
      orientation: TFlipArmOrientation;
      waitSteps, moveSteps, returnSteps: integer);
    procedure Init; override;
    procedure Loop; override;
  private
    m_waitSteps, m_moveSteps, m_returnSteps: integer;
    m_orientation: TFlipArmOrientation;
    m_phase: integer;
  end;

implementation

(* TFlipArm *)

constructor TFlipArm.FlipArm(
  c: TPoint3D;
  ntrvl: cardinal;
  orientation: TFlipArmOrientation;
  waitSteps, moveSteps, returnSteps: integer);
begin
  if (m_waitSteps + m_moveSteps + m_returnSteps <= 0) then
    Raise Exception.Create('Sum of wait and move steps must be greater than 0!');

  inherited Sticker(c, ntrvl);
  m_orientation := orientation;
  m_waitSteps := waitSteps;
  m_moveSteps := moveSteps;
  m_returnSteps := returnSteps;
end;

procedure TFlipArm.Init;
var
  supKrnl, supTip: TSupport;
  skin: TSkin;
  rp: TPoint3D;
  offsetOri: real;
begin
  case m_orientation of
  faoLeft: offsetOri := -20.0;
  faoRight: offsetOri := 20.0;
  end;

  rp := GetRotatedPoint(Centre);

  supKrnl := TSupport.Support(rp);
  supKrnl.AddNode(Point3DFromCoords(rp.x, rp.y - 10, rp.z));
  supKrnl.AddNode(Point3DFromCoords(rp.x, rp.y + 10, rp.z));
  supKrnl.AddNode(Point3DFromCoords(rp.x, rp.y, rp.z + offsetOri));
  Entities.Add(supKrnl);

  supTip := TSupport.Support(Point3DFromCoords(
                rp.x - 100.0,
                rp.y, rp.z));
  supTip.AddNode(Point3DFromCoords(rp.x - 100.0, rp.y - 10, rp.z));
  supTip.AddNode(Point3DFromCoords(rp.x - 100.0, rp.y + 10, rp.z));
  supTip.AddNode(Point3DFromCoords(rp.x - 100.0, rp.y, rp.z + offsetOri));
  supTip.AddNode(Point3DFromCoords(rp.x - 150.0, rp.y - 10, rp.z));
  supTip.AddNode(Point3DFromCoords(rp.x - 150.0, rp.y + 10, rp.z));
  supTip.AddNode(Point3DFromCoords(rp.x - 150.0, rp.y, rp.z + offsetOri));
  if m_orientation = faoLeft then
    supTip.RotateAround(rp, 0, -pi/4, 0)
  else if m_orientation = faoRight then
    supTip.RotateAround(rp, 0, pi/4, 0);
  Entities.Add(supTip);

  skin := TSkin.Skin;
  case m_orientation of
  faoLeft: begin
    skin.BindQuad(
        supKrnl.Nodes[0], supKrnl.Nodes[1],
        supTip.Nodes[1], supTip.Nodes[0],
        0, RGBToColor(80, 230, 30));
    );
    end;
    (* TODO *)
  faoRight: begin
    end;
  end;
  Entities.Add(skin);

  AddReceivingBox(Point3DFromCoords(rp.x - 125, rp.y, rp.z + 8), 10);
end;

procedure TFlipArm.Loop;
var
  direction: real;
  e: IWorldEntity;
begin
  m_phase := (m_phase + 1) mod (m_waitSteps + m_moveSteps + m_returnSteps);

  case m_orientation of
  faoLeft: direction := 1.0;
  faoRight: direction := -1.0;
  else
    direction := 0.0;
  end;

  if m_phase < m_waitSteps then
    exit
  else if m_phase = m_waitSteps then
    if TryGrab(Nil(*TODO*), True, e) then
      InanimateObjects.Add(e)
  else if m_phase < m_waitSteps + m_moveSteps then
    RotateAround(GetRotatedPoint(m_c), 0, direction * (pi/2.0) / m_moveSteps, 0)
  else if m_phase = m_waitSteps + m_moveSteps then begin
    if InanimateObjects.Count > 0 then
      TryStick(Nil(*TODO*), True, InanimateObjects[0]);
    RotateAround(GetRotatedPoint(m_c), 0, direction * (-pi/2.0) / m_returnSteps, 0);
  end else
    RotateAround(GetRotatedPoint(m_c), 0, direction * (-pi/2.0) / m_returnSteps, 0);
end;

end.

