unit TestConveyor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils, Graphics;

const
  CONVEYOR_SPEED = 10;

type
  TTestConveyor = class(AGrabber)
    procedure Init; override;
    procedure Loop; override;
    function GetTranslationVectorPerFrame: TPoint3D;
  private
    m_platesEnd: integer;
    m_support: TSupport;
  end;

implementation

procedure TTestConveyor.Init;
var
  e: IWorldEntity;
  sup: TSupport;
  skin: TSkin;
  p: TPoint3D;
  i: integer;
  offset: integer;
begin
  m_support := TSupport.Support(Centre.p);
  m_support.AddNode(Point3DFromCoords(Centre.p.x - 60, Centre.p.y - 25, Centre.p.z));
  m_support.AddNode(Point3DFromCoords(Centre.p.x - 60, Centre.p.y + 25, Centre.p.z));
  m_support.AddNode(Point3DFromCoords(Centre.p.x + 60, Centre.p.y - 25, Centre.p.z));
  m_support.AddNode(Point3DFromCoords(Centre.p.x - 60, Centre.p.y - 25, Centre.p.z + 500));
  m_support.AddNode(Point3DFromCoords(Centre.p.x - 60, Centre.p.y + 25, Centre.p.z + 500));
  m_support.AddNode(Point3DFromCoords(Centre.p.x + 60, Centre.p.y - 25, Centre.p.z + 500));
  m_support.AddNode(Point3DFromCoords(Centre.p.x, Centre.p.y, Centre.p.z));
  m_support.AddNode(Point3DFromCoords(Centre.p.x, Centre.p.y, Centre.p.z + 500));

  skin := TSkin.Skin;

  // base x10, top side
  for i := 0 to 9 do begin
    p := Point3DFromCoords(Centre.p.x, Centre.p.y, Centre.p.z + 50 * i);

    sup := TSupport.Support(p);
    // 70.710678 = 50*sqrt(2)
    // 35.355339 = ^ / 2
    sup.AddNode(Point3DFromCoords(p.x - 60, p.y + 35.355339, p.z));
    sup.AddNode(Point3DFromCoords(p.x + 60, p.y + 35.355339, p.z));
    Entities.Add(sup);

    if i > 0 then
      skin.BindQuad((Entities[i - 1] as TSupport).Nodes[0],
                sup.Nodes[0],
                sup.Nodes[1],
                (Entities[i - 1] as TSupport).Nodes[1],
                clGray,
                clSilver);
  end;
  offset := Entities.Count;
  // front two blips
  p := Point3DFromCoords(Centre.p.x, Centre.p.y, Centre.p.z + 500);
  RotateNode(p, GetRotatedPoint(m_support.Nodes[7]^), -pi / 2, 0, 0);
  sup := TSupport.Support(p);
  sup.AddNode(Point3DFromCoords(p.x - 60, p.y, p.z));
  sup.AddNode(Point3DFromCoords(p.x + 60, p.y, p.z));
  skin.BindQuad((Entities[offset - 1] as TSupport).Nodes[0],
        (Entities[offset - 1] as TSupport).Nodes[1],
        sup.Nodes[1],
        sup.Nodes[0],
        clGray,
        clSilver);
  offset := Entities.Count;
  // base x10, bottom side
  for i := 9 downto 0 do begin
    p := Point3DFromCoords(Centre.p.x, Centre.p.y, Centre.p.z + 50 * i);

    sup := TSupport.Support(p);
    sup.AddNode(Point3DFromCoords(p.x - 60, p.y - 35.355339, p.z));
    sup.AddNode(Point3DFromCoords(p.x + 60, p.y - 35.355339, p.z));
    Entities.Add(sup);

    if i = 0 then
      skin.BindQuad((Entities[offset - 1] as TSupport).Nodes[0],
                (Entities[offset - 1] as TSupport).Nodes[1],
                sup.Nodes[1],
                sup.Nodes[0],
                clGray,
                clSilver)
    else
      skin.BindQuad((Entities[offset + (9 - i) - 1] as TSupport).Nodes[0],
                (Entities[offset + (9 - i) - 1] as TSupport).Nodes[1],
                sup.Nodes[1],
                sup.Nodes[0],
                clGray,
                clSilver);
  end;
  offset := Entities.Count;
  // back two blips
  //N.B. last line if Entities[0]
  p := Point3DFromCoords(Centre.p.x, Centre.p.y, Centre.p.z);
  RotateNode(p, GetRotatedPoint(m_support.Nodes[6]^), pi / 2, 0, 0);
  sup := TSupport.Support(p);
  sup.AddNode(Point3DFromCoords(p.x - 60, p.y, p.z));
  sup.AddNode(Point3DFromCoords(p.x + 60, p.y, p.z));
  skin.BindQuad((Entities[offset - 1] as TSupport).Nodes[0],
        (Entities[offset - 1] as TSupport).Nodes[1],
        sup.Nodes[1],
        sup.Nodes[0],
        clGray,
        clSilver);
  skin.BindQuad(sup.Nodes[0],
        sup.Nodes[1],
        (Entities[0] as TSupport).Nodes[1],
        (Entities[0] as TSupport).Nodes[0],
        clGray,
        clSilver);
  // done with plates
  m_platesEnd := Entities.Count - 1;

  // add skin
  Entities.Add(skin);

  // add the frame
  Entities.Add(m_support);
end;

(* rotate arbitrarily as if rotating only on X
        Let segment (A,B) exist
        Let G be the centroid
        1. Translate vetor such that G == O
        2. Let v be (O,B)
        3. r_i = (v_i . v) * angle
           where r_i is rotation around Ox,Oy,Oz
           v_i are vectors OX,OY,OZ
*)
procedure TTestConveyor.Loop;
var
  v: TPoint3D;
  side: TPlanarity;
  angle, rx, ry, rz: real;
  OB, v_i: TPoint3D;
  x, y, z: real;
  frontPlane, backPlane: TPolygon;
  i: integer;
begin
  // get the correct vector
  v := GetTranslationVectorPerFrame;

  // get angular speed
  angle := pi * (CONVEYOR_SPEED / 50);
  // attempt _some_ normalization
  if abs(angle) < 0.00000001 then angle := 0.0
  else if abs(angle - pi) < 0.00000001 then angle := pi;

  // build the limiting polygons at either edges
  frontPlane := TPolygon.Triangle(
                GetRotatedPoint(m_support.Nodes[3]^),
                GetRotatedPoint(m_support.Nodes[4]^),
                GetRotatedPoint(m_support.Nodes[5]^));
  backPlane := TPolygon.Triangle(
                GetRotatedPoint(m_support.Nodes[0]^),
                GetRotatedPoint(m_support.Nodes[1]^),
                GetRotatedPoint(m_support.Nodes[2]^));

  // rotate plates by a smidgun on the correct vector
  for i := 0 to m_platesEnd do begin
    side := SideOfPlane(
        frontPlane,
        GetRotatedPoint((Entities[i] as TSupport).Location));

    if (side = plOn) or (side = plBehind) then begin
      OB := GetRotatedPoint((Entities[i] as TSupport).Nodes[1]^);
      SubstractVector(OB, GetRotatedPoint((Entities[i] as TSupport).Location));
      NormalizeVector(OB);

      v_i := Point3DFromCoords(1, 0, 0);
      rx := DotProduct(OB, v_i) * angle;
      v_i := Point3DFromCoords(0, 1, 0);
      ry := DotProduct(OB, v_i) * angle;
      v_i := Point3DFromCoords(0, 0, 1);
      rz := DotProduct(OB, v_i) * angle;

      Entities[i].RotateAround(GetRotatedPoint(m_support.Nodes[6]^), rx, ry, rz);

      continue;
    end else begin
      side := SideOfPlane(
        backPlane,
        GetRotatedPoint((Entities[i] as TSupport).Location));

      if (side = plOn) or (side = plFront) then begin
        OB := GetRotatedPoint((Entities[i] as TSupport).Nodes[1]^);
        SubstractVector(OB, GetRotatedPoint((Entities[i] as TSupport).Location));
        NormalizeVector(OB);

        v_i := Point3DFromCoords(1, 0, 0);
        rx := DotProduct(OB, v_i) * angle;
        v_i := Point3DFromCoords(0, 1, 0);
        ry := DotProduct(OB, v_i) * angle;
        v_i := Point3DFromCoords(0, 0, 1);
        rz := DotProduct(OB, v_i) * angle;

        Entities[i].RotateAround(GetRotatedPoint(m_support.Nodes[7]^), rx, ry, rz);

        continue;
      end;
    end; (* else *)
    Entities[i].Translate(v);
  end;


  frontPlane.Free;
  backPlane.Free;

  // move the entities on the conveyor by a smidgun on the correct vector
  for i := 0 to InanimateObjects.Count do
    InanimateObjects[i].Translate(v);

  // if at correct phase, ask m_inputs for some input
  //TODO
end;

function TTestConveyor.GetTranslationVectorPerFrame: TPoint3D;
var
  ret: TPoint3D;
  rc, rd: TPoint3D;
begin
  // get a normal vector along the frame
  rc := GetRotatedPoint(m_support.Nodes[0]^);
  rd := GetRotatedPoint(m_support.Nodes[3]^);
  ret := Point3DFromCoords(rd.x - rc.x, rd.y - rc.y, rd.z - rc.z);
  NormalizeVector(ret);

  // multiply by modulus
  ret.x := ret.x * CONVEYOR_SPEED;
  ret.y := ret.y * CONVEYOR_SPEED;
  ret.z := ret.z * CONVEYOR_SPEED;

  GetTranslationVectorPerFrame := ret;
end;

end.

