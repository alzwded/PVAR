unit Cartof;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GfxUtils, CoreUtils;

const
  NICE_YELLOW = 10351863; // 9D F4 F7
  NICE_GREEN = 5959770; // 5A F0 5A

type
  TCartof = class(TPart)
    procedure InitMesh; override;
    function GetBoundingBox: PBoundingBox; override;
  private
    m_bbox: TBoundingBox;
  end;

implementation

procedure TCartof.InitMesh;
var
  e: IEntity3D;
  c: TPoint3D;
begin
  c := GetRotatedPoint(Location);

  // Bottom
  e := TPolygon.Quad(
        Point3DFromCoords(c.x - 50, c.y, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y, c.z + 50),
        Point3DFromCoords(c.x - 50, c.y, c.z + 50));
  (e as TPolygon).FillColour := NICE_GREEN;
  Geometry.Add(e);

  // Sides
  // - back
  e := TPolygon.Quad(
        Point3DFromCoords(c.x - 50, c.y, c.z - 50),
        Point3DFromCoords(c.x - 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y, c.z - 50));
  (e as TPolygon).FillColour := NICE_YELLOW;
  Geometry.Add(e);
  // - right
  e := TPolygon.Quad(
        Point3DFromCoords(c.x + 50, c.y, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x + 50, c.y, c.z + 50));
  (e as TPolygon).FillColour := NICE_YELLOW;
  Geometry.Add(e);
  // - front
  e := TPolygon.Quad(
        Point3DFromCoords(c.x + 50, c.y, c.z + 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x - 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x - 50, c.y, c.z + 50));
  (e as TPolygon).FillColour := NICE_YELLOW;
  Geometry.Add(e);
  // - left
  e := TPolygon.Quad(
        Point3DFromCoords(c.x - 50, c.y, c.z + 50),
        Point3DFromCoords(c.x - 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x - 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x - 50, c.y, c.z - 50));
  (e as TPolygon).FillColour := NICE_YELLOW;
  Geometry.Add(e);

  // top
  e := TPolygon.Quad(
        Point3DFromCoords(c.x - 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x - 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z - 50));
  (e as TPolygon).FillColour := NICE_GREEN;
  Geometry.Add(e);

  decr(c.x, 50);
  m_c := RealPoint3DFromPoint(c);
end;

function TCartof.GetBoundingBox: PBoundingBox;
var
  c: TPoint3D;
begin
  c := GetRotatedPoint(m_c);

  m_bbox.p1 := Point3DFromCoords(c.x - 50, c.y, c.z - 50);
  m_bbox.p2 := Point3DFromCoords(c.x + 50, c.y + 100, c.z + 50);

  GetBoundingBox := @m_bbox;
end;

end.

