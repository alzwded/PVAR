program JakOmlette;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, JakRandr, GfxUtils, CoreUtils, TestUtils
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TJakRandr, randr);
  Application.Run;
end.

