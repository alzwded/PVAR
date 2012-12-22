unit FlipArm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils, Graphics;

const
  FlipArm_Kernel = 0;
  FlipArm_Tip = 1;
  FlipArm_Skin = 2;

  FLIPARM_HEIGHT = 50;
  FLIPARM_TIPDICKNESS = 30;
  FLIPARM_TIPLENGTH = 100;
  FLIPARM_TIPKRNLLENGTH = 365;

  FLIPARM_KRNLCOLOUR = 10351863; // 9D F4 F7
  FLIPARM_TIPCOLOUR = 5959770; // 5A F0 5A

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
  if (waitSteps + moveSteps + returnSteps <= 0) then
    Raise Exception.Create('Sum of wait and move steps must be greater than 0!');

  m_orientation := orientation;
  m_waitSteps := waitSteps;
  m_moveSteps := moveSteps;
  m_returnSteps := returnSteps;
  inherited Sticker(c, ntrvl);
end;

procedure TFlipArm.Init;
var
  supKrnl, supTip: TSupport;
  skin: TSkin;
  rp: TPoint3D;
  offsetOri: real;
begin
  case m_orientation of
  faoLeft: offsetOri := -FLIPARM_TIPDICKNESS;
  faoRight: offsetOri := FLIPARM_TIPDICKNESS;
  end;

  rp := GetRotatedPoint(Centre);

  supKrnl := TSupport.Support(rp);
  supKrnl.AddNode(Point3DFromCoords(rp.x, rp.y - FLIPARM_HEIGHT, rp.z));
  supKrnl.AddNode(Point3DFromCoords(rp.x, rp.y + FLIPARM_HEIGHT, rp.z));
  supKrnl.AddNode(Point3DFromCoords(rp.x, rp.y, rp.z + offsetOri));
  Entities.Add(supKrnl);

  supTip := TSupport.Support(Point3DFromCoords(
                rp.x - FLIPARM_TIPKRNLLENGTH,
                rp.y, rp.z));
  supTip.AddNode(Point3DFromCoords(rp.x - FLIPARM_TIPKRNLLENGTH, rp.y - FLIPARM_HEIGHT, rp.z));
  supTip.AddNode(Point3DFromCoords(rp.x - FLIPARM_TIPKRNLLENGTH, rp.y + FLIPARM_HEIGHT, rp.z));
  supTip.AddNode(Point3DFromCoords(rp.x - FLIPARM_TIPKRNLLENGTH, rp.y, rp.z + offsetOri));
  supTip.AddNode(Point3DFromCoords(rp.x - FLIPARM_TIPKRNLLENGTH - FLIPARM_TIPLENGTH, rp.y - FLIPARM_HEIGHT, rp.z));
  supTip.AddNode(Point3DFromCoords(rp.x - FLIPARM_TIPKRNLLENGTH - FLIPARM_TIPLENGTH, rp.y + FLIPARM_HEIGHT, rp.z));
  supTip.AddNode(Point3DFromCoords(rp.x - FLIPARM_TIPKRNLLENGTH - FLIPARM_TIPLENGTH, rp.y, rp.z + offsetOri));
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
        0, FLIPARM_KRNLCOLOUR
    );
    skin.BindQuad(
        supTip.Nodes[1], supKrnl.Nodes[1],
        supKrnl.Nodes[2], supTip.Nodes[2],
        0, FLIPARM_KRNLCOLOUR
    );
    skin.BindQuad(
        supTip.Nodes[2], supKrnl.Nodes[2],
        supKrnl.Nodes[0], supTip.Nodes[0],
        0, FLIPARM_KRNLCOLOUR
    );
    skin.BindQuad(
        supTip.Nodes[3], supTip.Nodes[0],
        supTip.Nodes[1], supTip.Nodes[4],
        0, FLIPARM_TIPCOLOUR
    );
    skin.BindQuad(
        supTip.Nodes[5], supTip.Nodes[4],
        supTip.Nodes[1], supTip.Nodes[2],
        0, FLIPARM_TIPCOLOUR
    );
    skin.BindQuad(
        supTip.Nodes[3], supTip.Nodes[5],
        supTip.Nodes[2], supTip.Nodes[0],
        0, FLIPARM_TIPCOLOUR
    );
    skin.BindTria(supTip.Nodes[4], supTip.Nodes[5], supTip.Nodes[3],
        0, FLIPARM_TIPCOLOUR);
    skin.BindTria(supKrnl.Nodes[0], supKrnl.Nodes[2], supKrnl.Nodes[1],
        0, FLIPARM_TIPCOLOUR);
    end;
  faoRight: begin
    skin.BindQuad(
        supKrnl.Nodes[1], supKrnl.Nodes[0],
        supTip.Nodes[0], supTip.Nodes[1],
        0, FLIPARM_KRNLCOLOUR
    );
    skin.BindQuad(
        supKrnl.Nodes[2], supTip.Nodes[2],
        supTip.Nodes[0], supKrnl.Nodes[0],
        0, FLIPARM_KRNLCOLOUR
    );
    skin.BindQuad(
        supTip.Nodes[2], supKrnl.Nodes[2],
        supKrnl.Nodes[1], supTip.Nodes[1],
        0, FLIPARM_KRNLCOLOUR
    );
    skin.BindQuad(
        supTip.Nodes[2], supTip.Nodes[1],
        supTip.Nodes[4], supTip.Nodes[5],
        0, FLIPARM_TIPCOLOUR
    );
    skin.BindQuad(
        supTip.Nodes[5], supTip.Nodes[3],
        supTip.Nodes[0], supTip.Nodes[2],
        0, FLIPARM_TIPCOLOUR
    );
    skin.BindQuad(
        supTip.Nodes[0], supTip.Nodes[3],
        supTip.Nodes[4], supTip.Nodes[1],
        0, FLIPARM_TIPCOLOUR
    );
    skin.BindTria(supTip.Nodes[4], supTip.Nodes[3], supTip.Nodes[5],
        0, FLIPARM_TIPCOLOUR);
    skin.BindTria(supKrnl.Nodes[0], supKrnl.Nodes[1], supKrnl.Nodes[2],
        0, FLIPARM_TIPCOLOUR);
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
  else if m_phase = m_waitSteps then begin
    if TryGrab(Nil(*TODO*), True, e) then
      InanimateObjects.Add(e);
    Entities[FlipArm_Tip].RotateAround(GetRotatedPoint(m_c), 0, direction * (pi/2.0) / m_moveSteps, 0)
  end else if m_phase < m_waitSteps + m_moveSteps then
    Entities[FlipArm_Tip].RotateAround(GetRotatedPoint(m_c), 0, direction * (pi/2.0) / m_moveSteps, 0)
  else if m_phase = m_waitSteps + m_moveSteps then begin
    if InanimateObjects.Count > 0 then begin
      TryStick(Nil(*TODO*), True, InanimateObjects[0]);
    end;
    Entities[FlipArm_Tip].RotateAround(GetRotatedPoint(m_c), 0, direction * (-pi/2.0) / m_returnSteps, 0);
  end else
    Entities[FlipArm_Tip].RotateAround(GetRotatedPoint(m_c), 0, direction * (-pi/2.0) / m_returnSteps, 0);
end;

end.

