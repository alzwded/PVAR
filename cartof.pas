unit Cartof;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GfxUtils, CoreUtils;

type
  TCartof = class(TPart)
    procedure InitMesh; override;
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
  Geometry.Add(e);

  // Sides
  // - back
  e := TPolygon.Quad(
        Point3DFromCoords(c.x - 50, c.y, c.z - 50),
        Point3DFromCoords(c.x - 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y, c.z - 50));
  Geometry.Add(e);
  // - right
  e := TPolygon.Quad(
        Point3DFromCoords(c.x + 50, c.y, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x + 50, c.y, c.z + 50));
  Geometry.Add(e);
  // - front
  e := TPolygon.Quad(
        Point3DFromCoords(c.x + 50, c.y, c.z + 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x - 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x - 50, c.y, c.z + 50));
  Geometry.Add(e);
  // - left
  e := TPolygon.Quad(
        Point3DFromCoords(c.x - 50, c.y, c.z + 50),
        Point3DFromCoords(c.x - 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x - 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x - 50, c.y, c.z - 50));
  Geometry.Add(e);

  // top
  e := TPolygon.Quad(
        Point3DFromCoords(c.x - 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z - 50),
        Point3DFromCoords(c.x + 50, c.y + 100, c.z + 50),
        Point3DFromCoords(c.x - 50, c.y + 100, c.z + 50));
  Geometry.Add(e);
end;

end.

