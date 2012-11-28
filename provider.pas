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

    procedure Init; override;
    procedure Loop; override;
    procedure Render(engine: PJakRandrEngine); override;
  private
    m_pc: integer;
    m_provideOnFrame: integer;
  public
    property ProvideOnFrame: integer read m_provideOnFrame write m_provideOnFrame;
  end;

implementation

procedure TProvider.Render(engine: PJakRandrEngine); begin end;

procedure TProvider.Init;
begin
  m_provideOnFrame := PROVIDE_ON_FRAME;
end;

procedure TProvider.Loop;
begin
  if (InanimateObjects.Count > 0) and (m_pc = 0) then begin
    InanimateObjects[0].MoveTo(GetRotatedPoint(Centre));
  end;

  m_pc := (m_pc + 1) mod m_provideOnFrame;
end;

procedure TProvider.AddStock(e: IWorldEntity);
begin
  e.MoveTo(Point3DFromCoords(0, 0, OFF_SCREEN_POINT));
  InanimateObjects.Add(e);
end;

end.

