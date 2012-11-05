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
  GfxUtils, CoreUtils;

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
    procedure DisplaySurfaceMouseLeave(Sender: TObject);
    procedure DisplaySurfaceMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DisplaySurfaceMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DisplaySurfaceResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure RenderClockTimer(Sender: TObject);
  private
    { private declarations }
    m_disp: TJakRandrEngine;
    m_worldEntities: TListOfWorldEntities;

    m_cameraManip: TCameraManip;
    m_lastClicked: TPoint;
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
  e: IWorldEntity;
begin
  m_disp := TJakRandrEngine.Create(DisplaySurface.Canvas, clBlack);

  m_worldEntities := TListOfWorldEntities.Create;

  e := TTestWE.Create(Point3DFromCoords(500.0, 0.0, 0.0), 0, 0);
  m_worldEntities.Add(e);
  e := TTestWE.Create(Point3DFromCoords(500.0, 0.0, 0.0), 1, 0);
  m_worldEntities.Add(e);
  e := TTestAxis.Create;
  m_worldEntities.Add(e);

  m_cameraManip := cmNone;

  Self.DoubleBuffered := true;
end;

procedure TJakRandr.FormDeactivate(Sender: TObject);
begin
  RenderClock.Enabled := false;
end;

procedure TJakRandr.FormActivate(Sender: TObject);
begin
  RenderClock.Enabled := true;
end;

procedure TJakRandr.DisplaySurfaceClick(Sender: TObject);
(*var
  i: integer;
  b: boolean;*)
begin
  (* move in Pause method
  b := not RenderClock.Enabled;
  RenderClock.Enabled := b;

  for i := 0 to m_worldEntities.Count - 1 do begin
    if b then
      m_worldEntities.Items[i].Start
    else
      m_worldEntities.Items[i].Stop;
  end;
  *)
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

procedure TJakRandr.DisplaySurfaceMouseLeave(Sender: TObject);
begin

end;

procedure TJakRandr.DisplaySurfaceMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  dx, dy: integer;
  r: real;
begin
  dx := m_lastClicked.X - X;
  dy := m_lastClicked.Y - Y;
  m_lastClicked.X := X;
  m_lastClicked.Y := y;

  case m_cameraManip of
  cmPanUV: begin
    r := dx * 4000.0 / DisplaySurface.Canvas.Width;
    incr(m_disp.O.x, r);
    r := -dy * 3000.0 / DisplaySurface.Canvas.Height; (* y is flipped *)
    incr(m_disp.O.y, r);
    end;
  cmPanH: begin
    r := -dy * 3000.0 / DisplaySurface.Canvas.Height; (* y is still flipped *)
    incr(m_disp.O.z, r);
    end;
  cmRot: begin
    r := dx * 2.0 * pi / DisplaySurface.Canvas.Width;
    m_disp.RY := m_disp.RY + r;
    (* normalize angles *)
    while m_disp.RY > 2.0 * pi do
      m_disp.RY := m_disp.RY - 2.0 * pi;
    while m_disp.RY < 0 do
      m_disp.RY := m_disp.RY + 2.0 * pi;
    r := dy * 2.0 * pi / DisplaySurface.Canvas.Height;
    m_disp.RX := m_disp.RX + r;
    (* normalize angles *)
    while m_disp.RX > 2.0 * pi do
      m_disp.RX := m_disp.RX - 2.0 * pi;
    while m_disp.RX < 0 do
      m_disp.RX := m_disp.RX + 2.0 * pi;
    end;
  cmRotZ: begin
    r := -dx * 2.0 * pi / DisplaySurface.Canvas.Width;
    m_disp.RZ := m_disp.RZ + r;
    (* normalize angles *)
    while m_disp.RZ > 2.0 * pi do
      m_disp.RZ := m_disp.RZ - 2.0 * pi;
    while m_disp.RZ < 0 do
      m_disp.RZ := m_disp.RZ + 2.0 * pi;
    end;
  cmZoom: begin
    r := dy * 5000.0 / DisplaySurface.Canvas.Height;
    m_disp.D := m_disp.D + r;
    end;
  end;
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
var
  i: integer;
begin
  m_disp.Free;

  for i := 0 to m_worldEntities.Count - 1 do
    m_worldEntities.Items[i]._Release;
  m_worldEntities.Clear;
  m_worldEntities.Free;
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
begin
  m_disp.BeginScene;
  for i := 0 to m_worldEntities.Count - 1 do
    m_worldEntities.Items[i].Render(@m_disp);
  m_disp.CommitScene;
end;

end.

