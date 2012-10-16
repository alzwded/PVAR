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
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    m_disp: TJakRandrEngine;
    m_test: TTestWE;
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
  cvs: TCanvas;
begin
  cvs := DisplaySurface.Canvas;
  m_disp := TJakRandrEngine.Create(@cvs, clBlack);
  m_test := TTestWE.Create(Point3DFromCoords(100.0, 100.0, 100.0), 0, 0);
end;

procedure TJakRandr.FormDeactivate(Sender: TObject);
begin
  Timer1.Enabled := false;
end;

procedure TJakRandr.FormActivate(Sender: TObject);
begin
  Timer1.Enabled := true;
end;

procedure TJakRandr.FormDestroy(Sender: TObject);
begin
  m_disp.Free;
  m_test.Free;
end;

procedure TJakRandr.Timer1Timer(Sender: TObject);
begin
  m_disp.BeginScene;
  m_test.Render(@m_disp);
  m_disp.CommitScene;
end;

end.

