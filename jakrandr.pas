unit JakRandr;

{$mode objfpc}{$H+}

(*
   moving the camera:
   * LMB X      -- move camera along X axis
   * LMB Y      -- move camera along Z axis
   * RMB X      -- rotate camera around Y axis
   * RMB Y      -- rotate camera around X axis
   * CTRL+LMB Y -- move camera along Y axis
   * CTRL+RMB X -- rotate camera around Z axis
   * SCROLL     -- change focal
*)

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  GfxUtils, CoreUtils, LCLType, TestUtils, TestConveyor, Cartof,
  (*$IFDEF UNIX*)
  LCLIntf,
  (*$ELSE*)
  Windows,
  (*$ENDIF*)
  Math,
  Provider, GrimReaper, Rotator, FlipArm, BuildableRobot, RoboArm, LegPutOnner;

const
  DEFAULT_CAPTION = 'JakRandr - F1 for help';
  DYNAMIC_FRAMERATE_LOW = 0.5;
  DYNAMIC_FRAMERATE_HIGH = 0.9;

  CAMERA_DEFAULT_X = 666;
  CAMERA_DEFAULT_Y = -1273;
  CAMERA_DEFAULT_Z = -762;
  CAMERA_DEFAULT_RX = 5.34;
  CAMERA_DEFAULT_RY = 0.44;
  CAMERA_DEFAULT_RZ = 0.55;

  ROOM_X_LOW = -2000;
  ROOM_X_HIGH = 2000;
  ROOM_Y_LOW = -1098;
  ROOM_Y_HIGH = 200;
  ROOM_Z_LOW = 800;
  ROOM_Z_HIGH = 3000;

  CONVEYOR_CLOCK = 120;
  SIDE_CONVEYORS_OFFSET = BR_ARM_TO_WAIST_OFFSET;
  PARTS_STOCK = 20;
  FLIPARM_OFFSET = 310;
  FLIPARM_COMPENSATION = 38;

  FLIPARMS_WAIT = 120; // speed of conveyor / distance
  FLIPARMS_MOVE = 10;
  FLIPARMS_RETURN = 20;

  ROBOARM_WAIT = 63;
  ROBOARM_MOVE = 63;
  ROBOARM_RETURN = 24;

  PROVIDER_CLOCK = CONVEYOR_CLOCK;
  PROVIDERS_MAX = FLIPARMS_WAIT + FLIPARMS_RETURN + FLIPARMS_MOVE; // sum of FLIPARMS frames
  PROVIDERS_MAIN = FLIPARMS_MOVE + 3;  // = FLIPARMS_MOVE
  PROVIDERS_SIDE = 0;
  PROVIDERS_HEAD = PROVIDERS_MAIN + 10;

  ROTATOR_X_OFFSET = 1600;

  HEAD_CORRECTION_Z = -14;
  HEAD_CORRECTION_Y = 23;

  HEAD_ARM_Z_OFFSET = -272 + HEAD_CORRECTION_Z;
  HEAD_ARM_X_OFFSET = 1600;
  HEAD_ARM_Y_OFFSET = 110 + HEAD_CORRECTION_Y;

  HEAD_CONV_X = HEAD_ARM_X_OFFSET - 290;
  HEAD_CONV_Y = 120 - 35 + HEAD_CORRECTION_Y;
  HEAD_CONV_Z = -20 * PLATE_LENGTH + HEAD_ARM_Z_OFFSET * 2 + 70 + HEAD_CORRECTION_Z;

type

  (* TCameraManip *)
  TCameraManip = ( cmNone, cmPanUV, cmPanH, cmRot, cmRotZ, cmZoom );

  { TJakRandr }

  TJakRandr = class(TForm)
    DisplaySurface: TImage;
    RenderClock: TTimer;
    procedure DisplaySurfaceClick(Sender: TObject);
    procedure DisplaySurfaceMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DisplaySurfaceMouseEnter(Sender: TObject);
    procedure DisplaySurfaceMouseLeave(Sender: TObject);
    procedure DisplaySurfaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DisplaySurfaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DisplaySurfaceResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure RenderClockTimer(Sender: TObject);
  private
    { private declarations }
    m_disp: TJakRandrEngine;
    m_worldEntities: TListOfWorldEntities;
    m_move: boolean;

    m_cameraManip: TCameraManip;
    m_lastClicked: TPoint;
  private
    procedure ToggleMotion;
    procedure CentreCamera;
    procedure ScreenShot;
    procedure ShowCameraInfo;
    procedure ShowAbout;

    procedure AddTestEntities;
    procedure AddEntities; virtual;
  public
    { public declarations }
  end; 

var
  randr: TJakRandr;

implementation

{$R *.lfm}

{ TJakRandr }

procedure TJakRandr.AddEntities;
var
  producer: TProvider;
  conveyor: TTestConveyor;
  mainConveyor: TTestConveyor;
  gravity: TTestConveyor;
  reaper: TGrimReaper;
  rotator: TRotator;
  fliparm: TFlipArm;
  roboarm: TRoboArm;
  lifeGiver: TRobotLifeGiver;
  legPutOnner: TLegPutOnner;
  e: IWorldEntity;
  t: TPolygon;
  i: integer;
begin
  (* main *)
  producer := TProvider.Grabber(
        Point3DFromCoords(0, 35, 0), PROVIDER_CLOCK);
  producer.ProvideOnFrame := PROVIDERS_MAIN;
  producer.MaxFrame := PROVIDERS_MAX;
  m_worldEntities.Add(producer);

  for i := 1 to PARTS_STOCK do begin
    e := TBuildableRobot.Compound(Point3DFromCoords(0, 0, 0), CONVEYOR_CLOCK);
    producer.AddStock(e);
  end;

  conveyor := TTestConveyor.Conveyor(
        Point3DFromCoords(0, 0, 0),
        CONVEYOR_CLOCK, 50);
  conveyor.Rotate(0, pi / 2, 0);
  conveyor.InputSource(producer);
  m_worldEntities.Add(conveyor);
  mainConveyor := conveyor;

  rotator := TRotator.Rotator(
        Point3DFromCoords(ROTATOR_X_OFFSET, 100, 0),
        CONVEYOR_CLOCK,
        0, 0, pi / 2);
  rotator.InputSource(conveyor);
  m_worldEntities.Add(rotator);

  gravity := TTestConveyor.Ghost(
        Point3DFromCoords(2700, 0, 0), CONVEYOR_CLOCK);
  gravity.InputSource(conveyor);
  m_worldEntities.Add(gravity);

  lifeGiver := TRobotLifeGiver.LifeGiver(
        Point3DFromCoords(2700 - 500, 0, 0), CONVEYOR_CLOCK); // have it
  lifeGiver.InputSource(conveyor);                            // closer
  m_worldEntities.Add(lifeGiver);

  legPutOnner := TLegPutOnner.Sticker(
        Point3DFromCoords(2700 - 300 - 500, 65, 180), CONVEYOR_CLOCK);
  legPutOnner.InputSource(conveyor);
  legPutOnner.OutputSource(conveyor);
  m_worldEntities.Add(legPutOnner);

  reaper := TGrimReaper.GrimReaper(
        Point3DFromCoords(2600, -800, 0));
  reaper.InputSource(gravity);
  m_worldEntities.Add(reaper);


  (* right *)
  producer := TProvider.Grabber(
        Point3DFromCoords(0, 35 + SIDE_CONVEYORS_OFFSET + 4, -560), PROVIDER_CLOCK);
  producer.ProvideOnFrame := PROVIDERS_SIDE;
  producer.MaxFrame := PROVIDERS_MAX;
  m_worldEntities.Add(producer);

  for i := 1 to PARTS_STOCK do begin
    e := TRobotPart.RobotPart(Point3DFromCoords(0, 0, 0), rptRightArm);
    producer.AddStock(e);
  end;

  conveyor := TTestConveyor.Conveyor(
        Point3DFromCoords(0, SIDE_CONVEYORS_OFFSET, -560),
        CONVEYOR_CLOCK, 20);
  conveyor.Rotate(0, pi / 2, 0);
  conveyor.InputSource(producer);
  m_worldEntities.Add(conveyor);

  gravity := TTestConveyor.Ghost(
        Point3DFromCoords(1100, SIDE_CONVEYORS_OFFSET, -560), CONVEYOR_CLOCK);
  gravity.InputSource(conveyor);
  m_worldEntities.Add(gravity);

  reaper := TGrimReaper.GrimReaper(
        Point3DFromCoords(1050, -800, -560));
  reaper.InputSource(gravity);
  m_worldEntities.Add(reaper);

  fliparm := TFlipArm.FlipArm(
        Point3DFromCoords(950, SIDE_CONVEYORS_OFFSET + FLIPARM_HEIGHT + FLIPARM_COMPENSATION, -FLIPARM_OFFSET),
        CONVEYOR_CLOCK,
        faoLeft,
        FLIPARMS_WAIT, FLIPARMS_MOVE, FLIPARMS_RETURN);
  fliparm.InputSource(conveyor);
  fliparm.OutputSource(mainConveyor);
  m_worldEntities.Add(fliparm);

  (* left *)
  producer := TProvider.Grabber(
        Point3DFromCoords(0, 35 + SIDE_CONVEYORS_OFFSET, 560), PROVIDER_CLOCK);
  producer.ProvideOnFrame := PROVIDERS_SIDE;
  producer.MaxFrame := PROVIDERS_MAX;
  m_worldEntities.Add(producer);

  for i := 1 to PARTS_STOCK do begin
    e := TRobotPart.RobotPart(Point3DFromCoords(0, 0, 0), rptLeftArm);
    producer.AddStock(e);
  end;

  conveyor := TTestConveyor.Conveyor(
        Point3DFromCoords(0, SIDE_CONVEYORS_OFFSET, 560),
        CONVEYOR_CLOCK, 20);
  conveyor.Rotate(0, pi / 2, 0);
  conveyor.InputSource(producer);
  m_worldEntities.Add(conveyor);

  gravity := TTestConveyor.Ghost(
        Point3DFromCoords(1100, SIDE_CONVEYORS_OFFSET, 560), CONVEYOR_CLOCK);
  gravity.InputSource(conveyor);
  m_worldEntities.Add(gravity);

  reaper := TGrimReaper.GrimReaper(
        Point3DFromCoords(1050, -800, 560));
  reaper.InputSource(gravity);
  m_worldEntities.Add(reaper);

  fliparm := TFlipArm.FlipArm(
        Point3DFromCoords(950, SIDE_CONVEYORS_OFFSET + FLIPARM_HEIGHT + FLIPARM_COMPENSATION, FLIPARM_OFFSET),
        CONVEYOR_CLOCK,
        faoRight,
        FLIPARMS_WAIT, FLIPARMS_MOVE, FLIPARMS_RETURN);
  fliparm.InputSource(conveyor);
  fliparm.OutputSource(mainConveyor);
  m_worldEntities.Add(fliparm);

  (* head stuff *)
  roboarm := TRoboArm.RoboArm(
        Point3DFromCoords(HEAD_ARM_X_OFFSET, HEAD_ARM_Y_OFFSET, HEAD_ARM_Z_OFFSET),
        CONVEYOR_CLOCK,
        ROBOARM_WAIT,
        ROBOARM_MOVE,
        ROBOARM_RETURN);
  m_worldEntities.Add(roboarm);

  producer := TProvider.Grabber(
        Point3DFromCoords(HEAD_CONV_X - 10, 35 + HEAD_CONV_Y, HEAD_CONV_Z), PROVIDER_CLOCK);
  producer.ProvideOnFrame := PROVIDERS_HEAD;
  producer.MaxFrame := PROVIDERS_MAX;
  m_worldEntities.Add(producer);

  for i := 1 to PARTS_STOCK do begin
    e := TRobotPart.RobotPart(Point3DFromCoords(0, 0, 0), rptHead);
    producer.AddStock(e);
  end;

  conveyor := TTestConveyor.Conveyor(
        Point3DFromCoords(HEAD_CONV_X, HEAD_CONV_Y, HEAD_CONV_Z),
        CONVEYOR_CLOCK, 20);
  conveyor.InputSource(producer);
  m_worldEntities.Add(conveyor);

  gravity := TTestConveyor.Ghost(
        Point3DFromCoords(HEAD_CONV_X, HEAD_CONV_Y, HEAD_CONV_Z + 21 * PLATE_LENGTH + 50), CONVEYOR_CLOCK);
  gravity.InputSource(conveyor);
  m_worldEntities.Add(gravity);

  reaper := TGrimReaper.GrimReaper(
        Point3DFromCoords(HEAD_CONV_X + 50 - 25, -800 + HEAD_CONV_Y, HEAD_CONV_Z + 50 + 21 * PLATE_LENGTH - 50));
  reaper.InputSource(gravity);
  m_worldEntities.Add(reaper);

  roboarm.InputSource(conveyor);
  roboarm.OutputSource(mainConveyor);

  ToggleMotion;
end;

procedure TJakRandr.AddTestEntities;
var
  e, conveyor: IWorldEntity;
  t: TPolygon;
begin
  e := TTestWE.Create(Point3DFromCoords(500.0, 0.0, 0.0), 1, 0);
  m_worldEntities.Add(e);
  (*e := TTestWE.Create(Point3DFromCoords(500.0, 0.0, 0.0), 0, 0);
  m_worldEntities.Add(e);
  e := TTestAxis.Create;
  m_worldEntities.Add(e);*)

  (*e := TArm.Compound(Point3DFromCoords(-300.0, 0.0, 0.0), 20);
  m_worldEntities.Add(e);*)

  e := TTestConveyor.Conveyor(Point3DFromCoords(0.0, 0, 1000), 12, 30, 400);
  conveyor := e;
  e.Rotate(pi / 12, 0(*-pi / 6*), 0);
  m_worldEntities.Add(e);

  (*e := TCartof.Part(Point3DFromCoords(0, 500, 0));
  m_worldEntities.Add(e);*)

  e := TProvider.Grabber(Point3DFromCoords(0, 34, 1000), 500);
  (e as TProvider).AddStock(TCartof.Part(Point3DFromCoords(0, 0, 0)));
  (e as TProvider).AddStock(TCartof.Part(Point3DFromCoords(0, 0, 0)));
  (e as TProvider).AddStock(TCartof.Part(Point3DFromCoords(0, 0, 0)));
  (e as TProvider).AddStock(TCartof.Part(Point3DFromCoords(0, 0, 0)));
  (e as TProvider).AddStock(TCartof.Part(Point3DFromCoords(0, 0, 0)));
  (e as TProvider).AddStock(TCartof.Part(Point3DFromCoords(0, 0, 0)));
  (e as TProvider).AddStock(TCartof.Part(Point3DFromCoords(0, 0, 0)));
  (e as TProvider).AddStock(TCartof.Part(Point3DFromCoords(0, 0, 0)));
  m_WorldEntities.Add(e);

  (conveyor as TTestConveyor).InputSource(e as TProvider);

  e := TTestConveyor.Conveyor(Point3DFromCoords(0, -388, 1448 + 1000), 40, 10, 400);
  e.Rotate(0, -pi / 6, 0);
  m_worldEntities.Add(e);

  (e as TTestConveyor).InputSource(conveyor as TTestConveyor);

  conveyor := e;
  e := TTestConveyor.Ghost(Point3DFromCoords(-250 - 25 * 4, -388, 1448 + 1000 + 433 + 34 * 4), 20);
  m_worldEntities.Add(e);
  (e as TTestConveyor).InputSource(conveyor as TTestConveyor);

  conveyor := e;
  e := TGrimReaper.GrimReaper(Point3DFromCoords(
        conveyor.GetLocation.x +  50, conveyor.GetLocation.y - 500, conveyor.GetLocation.z - 50));
  (e as TGrimReaper).InputSource(conveyor as TTestConveyor);
  m_worldEntities.Add(e);

  (* start everything! *)
  ToggleMotion;
end;

procedure TJakRandr.FormCreate(Sender: TObject);
begin
  m_disp := TJakRandrEngine.Create(DisplaySurface.Canvas, clBlack);

  m_cameraManip := cmNone;

  m_move := false;

  Self.DoubleBuffered := true;

  m_worldEntities := TListOfWorldEntities.Create;

  //AddTestEntities;
  AddEntities;

  m_disp.O := Point3DFromCoords(CAMERA_DEFAULT_X, CAMERA_DEFAULT_Y, CAMERA_DEFAULT_Z);
  m_disp.RX := CAMERA_DEFAULT_RX;
  m_disp.RY := CAMERA_DEFAULT_RY;
  m_disp.RZ := CAMERA_DEFAULT_RZ;
end;

procedure TJakRandr.FormDeactivate(Sender: TObject);
begin
  RenderClock.Enabled := false;
end;

procedure TJakRandr.FormActivate(Sender: TObject);
begin
  RenderClock.Enabled := true;
end;

procedure TJakRandr.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  RenderClock.Enabled := false;
end;

procedure TJakRandr.DisplaySurfaceClick(Sender: TObject);
begin
end;

procedure TJakRandr.DisplaySurfaceMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  m_lastClicked.X := X;
  m_lastClicked.Y := Y;

  case Button of
  mbLeft:
    if ssCtrl in Shift then
      m_cameraManip := cmPanH
    else if ssShift in Shift then
      m_cameraManip := cmZoom
    else m_cameraManip := cmPanUV;
  mbRight:
    if ssCtrl in Shift then
      m_cameraManip := cmRotZ
    else
      m_cameraManip := cmRot;
  mbMiddle:
    m_cameraManip := cmPanH;
  end;
end;

procedure TJakRandr.DisplaySurfaceMouseEnter(Sender: TObject);
begin
  m_cameraManip := cmNone;
end;

procedure TJakRandr.DisplaySurfaceMouseLeave(Sender: TObject);
begin

end;

procedure TJakRandr.DisplaySurfaceMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  dx, dy: integer;
  r: real;
  i, j, k: real;
  dv: TPoint3D;
  ox, oy, oz: TPoint3D;
  o: TPoint3D;
begin
  if m_cameraManip = cmNone then exit;

  dx := m_lastClicked.X - X;
  dy := m_lastClicked.Y - Y;

  if (dx = 0) and (dy = 0) then exit;

  m_lastClicked.X := X;
  m_lastClicked.Y := y;

  o := Point3DFromCoords(0, 0, 0);
  ox := Point3DFromCoords(1.0, 0.0, 0.0);
  RotateNode(ox, o, -m_disp.RX, -m_disp.RY, -m_disp.RZ);
  oy := Point3DFromCoords(0.0, 1.0, 0.0);
  RotateNode(oy, o, -m_disp.RX, -m_disp.RY, -m_disp.RZ);
  oz := Point3DFromCoords(0.0, 0.0, 1.0);
  RotateNode(oz, o, -m_disp.RX, -m_disp.RY, -m_disp.RZ);

  case m_cameraManip of
  cmPanUV: begin
    i := dx * 4000.0 / DisplaySurface.Canvas.Width;
    j := -dy * 3000.0 / DisplaySurface.Canvas.Height; (* y is flipped *)

    dv := Point3DFromCoords(1, 0, 0);
    incr(m_disp.O.x, DotProduct(dv, ox) * i);
    incr(m_disp.O.y, DotProduct(dv, oy) * i);
    incr(m_disp.O.z, DotProduct(dv, oz) * i);

    dv := Point3DFromCoords(0, 1, 0);
    incr(m_disp.O.x, DotProduct(dv, ox) * j);
    incr(m_disp.O.y, DotProduct(dv, oy) * j);
    incr(m_disp.O.z, DotProduct(dv, oz) * j);
    end;
  cmPanH: begin
    k := -dy * 3000.0 / DisplaySurface.Canvas.Height; (* y is still flipped *)

    dv := Point3DFromCoords(0, 0, 1);
    incr(m_disp.O.x, DotProduct(dv, ox) * k);
    incr(m_disp.O.y, DotProduct(dv, oy) * k);
    incr(m_disp.O.z, DotProduct(dv, oz) * k);
    end;
  cmRot: begin
    r := dx * 2.0 * pi / DisplaySurface.Canvas.Width;
    m_disp.RY := m_disp.RY + r;

    r := dy * 2.0 * pi / DisplaySurface.Canvas.Height;
    m_disp.RX := m_disp.RX + r;
    end;
  cmRotZ: begin
    r := -dx * 2.0 * pi / DisplaySurface.Canvas.Width;
    m_disp.RZ := m_disp.RZ + r;
    end;
  cmZoom: begin
    r := -dy * 5000.0 / DisplaySurface.Canvas.Height;
    m_disp.D := m_disp.D + r;
    if m_disp.D < MIN_CAMERA_DISTANCE then
      m_disp.D := MIN_CAMERA_DISTANCE
    else if m_disp.D > MAX_CAMERA_DISTANCE then
      m_disp.D := MAX_CAMERA_DISTANCE;
    end;
  end;

  (* normalize angles *)
  while m_disp.RY > 2.0 * pi do
    m_disp.RY := m_disp.RY - 2.0 * pi;
  while m_disp.RY < 0 do
    m_disp.RY := m_disp.RY + 2.0 * pi;
  while m_disp.RX > 2.0 * pi do
    m_disp.RX := m_disp.RX - 2.0 * pi;
  while m_disp.RX < 0 do
    m_disp.RX := m_disp.RX + 2.0 * pi;
  while m_disp.RZ > 2.0 * pi do
    m_disp.RZ := m_disp.RZ - 2.0 * pi;
  while m_disp.RZ < 0 do
    m_disp.RZ := m_disp.RZ + 2.0 * pi;
end;

procedure TJakRandr.DisplaySurfaceMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  m_cameraManip := cmNone;
end;

procedure TJakRandr.DisplaySurfaceResize(Sender: TObject);
begin
end;

procedure TJakRandr.FormDestroy(Sender: TObject);
begin
  m_disp.Free;

  m_worldEntities.Clear;
  m_worldEntities.Free;
end;

procedure TJakRandr.FormKeyPress(Sender: TObject; var Key: char);
begin
end;

procedure TJakRandr.ShowAbout;
var
  b: boolean;
begin
  b := m_move;
  if b then ToggleMotion;

  MessageDlg('JakRandr',
        'Proiect realizat de Vlad Meșco. ©2012'#13#10 +
        'anul III, Tehnologia Informației, gr. 4301'#13#10 +
        #13#10 +
        '3D Animation Software Engine written from scratch.'#13#10 +
        #13#10 +
        'Shift + Esc - exit'#13#10 +
        'Space - toggle animatnion'#13#10 +
        'Shift + Enter - recenter camera'#13#10 +
        'Mouse - manipulate camera'#13#10 +
        'Ctrl + P - take screenshot'#13#10 +
        'F2 - view current camera coordinates and rotations',
        mtInformation,
        [mbOK],
        0);

  if b then ToggleMotion;
end;

procedure TJakRandr.ShowCameraInfo;
var
  s: string;
  b: boolean;
begin
  b := m_move;
  if b then ToggleMotion;

  s := Format('O=(%f,%f,%f) R=(%f,%f,%f)',
        [m_disp.O.x, m_disp.O.y, m_disp.O.z,
        m_disp.RX, m_disp.RY, m_disp.RZ]);
  MessageDlg('JakRandr - Camera information', s, mtInformation, [mbOK], 0);

  if b then ToggleMotion;
end;

procedure TJakRandr.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
  );
var
  i: integer;
begin
  case Key of
  VK_PRINT: ScreenShot;
  VK_P: begin;
    if ssCtrl in Shift then
      ScreenShot;
    end;
  VK_F2: ShowCameraInfo;
  VK_SPACE: ToggleMotion;
  VK_RETURN: if ssShift in Shift then CentreCamera;
  VK_ESCAPE: if ssShift in Shift then begin
    RenderClock.Enabled:=false;
    for i := 0 to m_worldEntities.Count - 1 do
      m_worldEntities[i].Stop;
    Self.Close;
    end;
  VK_F1: ShowAbout;
  end;
end;

procedure TJakRandr.FormResize(Sender: TObject);
begin
  DisplaySurface.Width:=Self.ClientWidth;
  DisplaySurface.Height:=Self.ClientHeight;
  DisplaySurface.Picture.Bitmap.Width:=DisplaySurface.Width;
  DisplaySurface.Picture.Bitmap.Height:=DisplaySurface.Height;
end;

procedure TJakRandr.RenderClockTimer(Sender: TObject);
var
  i: integer;
  t0, t: DWORD;
begin
  t0 := GetTickCount;
  m_disp.BeginScene;
  for i := 0 to m_worldEntities.Count - 1 do
    m_worldEntities.Items[i].Render(@m_disp);
  m_disp.CommitScene;
  t := GetTickCount;

  if t >= t0 then
    t := t - t0
  else
    t := t + (MAXDWORD - t0);

  // limmit framerate
  if t > 150 then t := 150;
  if t < 10 then t := 10;

  if (t > DYNAMIC_FRAMERATE_HIGH * RenderClock.Interval) then begin
    RenderClock.Interval := t;
    Self.Caption := DEFAULT_CAPTION + ' ~ ' + IntToStr(floor(1000.0 / t)) + 'fps';
  end else if (t < DYNAMIC_FRAMERATE_LOW * RenderClock.Interval) then begin
    RenderClock.Interval := t;
    Self.Caption := DEFAULT_CAPTION + ' ~ ' + IntToStr(floor(1000.0 / t)) + 'fps';
  end;
end;

procedure TJakRandr.ToggleMotion;
var
  i: integer;
begin
  for i := 0 to m_worldEntities.Count - 1 do
    if m_move then
      m_worldEntities[i].Stop
    else
      m_worldEntities[i].Start;
  m_move := not m_move;
end;

procedure TJakRandr.CentreCamera;
begin
  m_disp.O := Point3DFromCoords(CAMERA_DEFAULT_X, CAMERA_DEFAULT_Y, CAMERA_DEFAULT_Z);
  m_disp.RX := CAMERA_DEFAULT_RX;
  m_disp.RY := CAMERA_DEFAULT_RY;
  m_disp.RZ := CAMERA_DEFAULT_RZ;
  m_disp.D := 5000.0;
end;

procedure TJakRandr.ScreenShot;
var
  d: TSaveDialog;
  stopped: boolean;
  fileName: String;
begin
  if m_move then begin
    ToggleMotion;
    stopped := true;
  end else
    stopped := false;

  fileName := '';


  d := TSaveDialog.Create(Self);
  d.Options:= [ofOverwritePrompt, ofPathMustExist, ofCreatePrompt,
                ofEnableSizing, ofViewDetail, ofAutoPreview];
  d.FileName := 'JakRandr.png';
  d.Filter := 'Png|*.png|All files|*.*';
  d.FilterIndex := 1;
  d.DefaultExt := 'png';
  d.Title:='JakRandr - Save screenshot';

  if d.Execute then
    fileName := d.FileName;

  d.Free;

  if fileName <> '' then
    DisplaySurface.Picture.SaveToFile(fileName, 'png');

  if stopped then
    ToggleMotion;
end;

end.

