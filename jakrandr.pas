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
  GfxUtils, CoreUtils, LCLType, TestUtils, TestConveyor, Cartof, Windows, Math,
  Provider;

const
  DEFAULT_CAPTION = 'JakRandr - F1 for help';
  DYNAMIC_FRAMERATE_LOW = 0.5;
  DYNAMIC_FRAMERATE_HIGH = 0.9;

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
  public
    { public declarations }
  end; 

var
  randr: TJakRandr;

implementation

{$R *.lfm}

{ TJakRandr }

procedure TJakRandr.FormCreate(Sender: TObject);
var
  e, conveyor: IWorldEntity;
begin
  m_disp := TJakRandrEngine.Create(DisplaySurface.Canvas, clBlack);

  m_cameraManip := cmNone;

  m_move := true;

  Self.DoubleBuffered := true;

  m_worldEntities := TListOfWorldEntities.Create;

  e := TTestWE.Create(Point3DFromCoords(500.0, 0.0, 0.0), 1, 0);
  m_worldEntities.Add(e);
  e := TTestWE.Create(Point3DFromCoords(500.0, 0.0, 0.0), 0, 0);
  m_worldEntities.Add(e);
  e := TTestAxis.Create;
  m_worldEntities.Add(e);

  e := TArm.Compound(Point3DFromCoords(-300.0, 0.0, 0.0), 20);
  m_worldEntities.Add(e);

  e := TTestConveyor.Conveyor(Point3DFromCoords(0.0, 0, 1000), 12, 30, 400);
  conveyor := e;
  e.Rotate(pi / 12, 0(*-pi / 6*), 0);
  m_worldEntities.Add(e);

  e := TCartof.Part(Point3DFromCoords(0, 500, 0));
  m_worldEntities.Add(e);

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
  e := TTestConveyor.Ghost(Point3DFromCoords(-250 - 25 * 2, -388, 1448 + 1000 + 433 + 40 * 2), 20);
  m_worldEntities.Add(e);
  (e as TTestConveyor).InputSource(conveyor as TTestConveyor);
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
  VK_SPACE: ToggleMotion;
  VK_RETURN: if ssShift in Shift then CentreCamera;
  VK_ESCAPE: if ssShift in Shift then begin
    RenderClock.Enabled:=false;
    for i := 0 to m_worldEntities.Count - 1 do
      m_worldEntities[i].Stop;
    Self.Close;
    end;
  VK_F1: begin
    MessageDlg('JakRandr',
        'Proiect realizat de Vlad Meșco. ©2012'#13#10 +
        #13#10 +
        '3D Animation Software Engine written from scratch.'#13#10 +
        #13#10 +
        'Shift + Esc - exit'#13#10 +
        'Space - toggle animatnion'#13#10 +
        'Shift + Enter - recenter camera'#13#10 +
        'Mouse - manipulate camera'#13#10 +
        'Ctrl + P - take screenshot',
        mtInformation,
        [mbOK],
        0);
    end;
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
  m_disp.O := Point3DFromCoords(0.0, 0.0, 0.0);
  m_disp.RX := 0.0;
  m_disp.RY := 0.0;
  m_disp.RZ := 0.0;
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

