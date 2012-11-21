unit Provider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils;

const
  PROVIDE_ON_FRAME = 10;
  OFF_SCREEN_POINT = -1e+27;

type
  TProvider = class(AGrabber)
    (* helper function to add entities outside of any grabing action *)
    procedure AddStock(e: IWorldEntity);

    procedure Loop; override;
    procedure Render(engine: PJakRandrEngine); override;
  private
    m_pc: integer;
  end;

implementation

procedure TProvider.Render(engine: PJakRandrEngine); begin end;

procedure TProvider.Loop;
begin
  m_pc := (m_pc + 1) mod PROVIDE_ON_FRAME;

  if m_pc = 0 then begin
    Entities[0].MoveTo(GetRotatedPoint(Centre));
  end;
end;

procedure TProvider.AddStock(e: IWorldEntity);
begin
  e.MoveTo(Point3DFromCoords(0, 0, OFF_SCREEN_POINT));
  Entities.Add(e);
end;

end.

