unit JakRandr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  GfxUtils, CoreUtils;

type

  { TJakRandr }

  TJakRandr = class(TForm)
    DisplaySurface: TImage;
    Timer1: TTimer;
    procedure DisplaySurfaceClick(Sender: TObject);
    procedure DisplaySurfaceResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    m_disp: TJakRandrEngine;
    m_test: TTestWE;
    m_test2: TTestWE;
  public
    { public declarations }
  end; 

var
  randr: TJakRandr;

implementation

{$R *.lfm}

{ TJakRandr }

procedure TJakRandr.FormCreate(Sender: TObject);
begin
  m_disp := TJakRandrEngine.Create(DisplaySurface.Canvas, clBlack);
  m_test := TTestWE.Create(Point3DFromCoords(0.0, 0.0, 0.0), 0, 0);
  m_test2 := TTestWE.Create(Point3DFromCoords(500.0, 0.0, 0.0), 1, 0);
end;

procedure TJakRandr.FormDeactivate(Sender: TObject);
begin
  Timer1.Enabled := false;
end;

procedure TJakRandr.FormActivate(Sender: TObject);
begin
  //Timer1.Enabled := true;
end;

procedure TJakRandr.DisplaySurfaceClick(Sender: TObject);
begin
  Timer1.Enabled := not Timer1.Enabled;
end;

procedure TJakRandr.DisplaySurfaceResize(Sender: TObject);
begin
end;

procedure TJakRandr.FormDestroy(Sender: TObject);
begin
  m_disp.Free;
  m_test.Free;
  m_test2.Free;
end;

procedure TJakRandr.FormResize(Sender: TObject);
begin
  DisplaySurface.Width:=Self.ClientWidth;
  DisplaySurface.Height:=Self.ClientHeight;
  DisplaySurface.Picture.Bitmap.Width:=DisplaySurface.Width;
  DisplaySurface.Picture.Bitmap.Height:=DisplaySurface.Height;
end;

procedure TJakRandr.Timer1Timer(Sender: TObject);
begin
  m_disp.BeginScene;
  m_test.Render(@m_disp);
  m_test2.Render(@m_disp);
  m_disp.CommitScene;
end;

end.

