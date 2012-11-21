unit Provider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils;

type
  TProvider = class(AGrabber)
    procedure Loop; override;
  end;

implementation

procedure TProvider.Loop;
begin

end;

end.

