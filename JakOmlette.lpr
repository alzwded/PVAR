program JakOmlette;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, JakRandr, GfxUtils, CoreUtils, TestUtils, TestConveyor, Cartof,
Provider, GrimReaper, Rotator, FlipArm, BuildableRobot, RoboArm, LegPutOnner;

{$R *.res}

begin
  Application.Title:='JakRandr';
  Application.Initialize;
  Application.CreateForm(TJakRandr, randr);
  Application.Run;
end.

