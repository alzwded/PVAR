unit Rotator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils;

const
  ROTATOR_MAX_FRAMES = 50;

type
  TChosenOne = record
    m_e: IWorldEntity;
    m_phr: integer;
  end;

  TRotator = class(AGrabber)
    constructor Rotator(cen: TPoint3D; intrvl: Cardinal; rx, ry, rz: real);
    procedure Loop; override;
  private
    m_bbox: TBoundingBox;
    m_Rx, m_Ry, m_Rz: real;
    m_n: integer;
    m_chosens: array of TChosenOne;

    procedure ShiftLeft;
    procedure Consider(e: IWorldEntity);
  end;

implementation

procedure TRotator.Consider(e: IWorldEntity);
var
  chosen: TChosenOne;
  i: integer;
begin
  for i := 0 to m_n - 1 do begin
    if m_chosens[i].m_e.Equals(e) then
      exit;
  end;
  chosen.m_e := e;
  chosen.m_phr := 0;
  SetLength(m_chosens, m_n + 1);
  m_chosens[m_n] := chosen;
  inc(m_n);
end;

constructor TRotator.Rotator(cen: TPoint3D; intrvl: Cardinal; rx, ry, rz: real);
begin
  m_rx := rx;
  m_ry := ry;
  m_rz := rz;
  m_n := 0;

  inherited Grabber(cen, intrvl);
end;

procedure TRotator.ShiftLeft;
var
  i: integer;
begin
  if m_n = 0 then exit;
  if m_n = 1 then begin
    SetLength(m_chosens, 0);
  end;

  for i := 1 to m_n - 1 do begin
    m_chosens[i - 1] := m_chosens[i];
  end;
  dec(m_n);
  SetLength(m_chosens, m_n);
end;

procedure TRotator.Loop;
var
  e: IWorldEntity;
  c: TPoint3D;
  i: integer;
begin
  c := GetRotatedPoint(Centre);
  m_bbox.p1 := Point3DFromCoords(
        c.x - 50, c.y - 100, c.z - 50);
  m_bbox.p2 := Point3DFromCoords(
        c.x + 50, c.y, c.z + 50);

  if TryGrab(@m_bbox, False, e) then begin
    Consider(e);
  end;

  i := 0;
  while i < m_n do begin
    while m_chosens[i].m_phr >= ROTATOR_MAX_FRAMES do begin
      ShiftLeft;
      if i >= m_n then break;
    end;
    if i >= m_n then break;

    inc(m_chosens[i].m_phr);
    m_chosens[i].m_e.Rotate(
        m_rx / ROTATOR_MAX_FRAMES,
        m_ry / ROTATOR_MAX_FRAMES,
        m_rz / ROTATOR_MAX_FRAMES);

    inc(i);
  end;
end;

end.

