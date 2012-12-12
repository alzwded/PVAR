unit Provider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils;

const
  PROVIDE_ON_FRAME = 10;

type
  TProvider = class(AGrabber)
    (* helper function to add entities outside of any grabing action *)
    procedure AddStock(e: IWorldEntity);

    procedure Start; override; (* reason: provide ON start *)
    procedure Init; override;
    procedure Loop; override;
    procedure Render(engine: PJakRandrEngine); override;
  private
    m_pc: integer;
    m_provideOnFrame: integer;
    m_firstStart: boolean;
  public
    property ProvideOnFrame: integer read m_provideOnFrame write m_provideOnFrame;
  end;

implementation

procedure TProvider.Start;
begin
  if m_firstStart then begin
    Loop;
    m_firstStart := false;
  end;
  inherited Start;
end;

procedure TProvider.Render(engine: PJakRandrEngine); begin end;

procedure TProvider.Init;
begin
  m_provideOnFrame := PROVIDE_ON_FRAME;
  m_firstStart := true;
end;

procedure TProvider.Loop;
begin
  if (InanimateObjects.Count > 0) and (m_pc = 0) then begin
    InanimateObjects[0].MoveTo(GetRotatedPoint(Centre));
    InanimateObjects[0].Hidden := False;;
  end;

  if m_provideOnFrame > 0 then
    m_pc := (m_pc + 1) mod m_provideOnFrame
  else
    m_pc := 0;
end;

procedure TProvider.AddStock(e: IWorldEntity);
begin
  //e.MoveTo(Point3DFromCoords(0, 0, OFF_SCREEN_POINT));
  e.Hidden := true;
  InanimateObjects.Add(e);
end;

end.

