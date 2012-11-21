unit TestConveyor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils, Graphics, Math;

const
  CONVEYOR_SPEED = 5;
  PLATE_LENGTH = 50; (* DO NOT CHANGE YET
                        the square root constant needs to also be adapted
                        and cached, TODO *)

type
  TTestConveyor = class(AGrabber)
    constructor Conveyor(c: TPoint3D; interval: cardinal; nbPlates: integer = 9; width: integer = 120);
    procedure Init; override;
    procedure Loop; override;
    function GetTranslationVectorPerFrame: TPoint3D;
  private
    m_platesEnd: integer;
    m_support: TSupport;
    m_nbPlates: integer;
    m_width: integer;
    m_grabbingBox: TBoundingBox;
  public
    function GrabbingBox: PBoundingBox;
  end;

implementation

constructor TTestConveyor.Conveyor(c: TPoint3D; interval: cardinal; nbPlates: integer = 9; width: integer = 120);
begin
  m_nbPlates := nbPlates;
  m_width := width;
  inherited Grabber(c, interval);
end;

procedure TTestConveyor.Init;
var
  sup: TSupport;
  skin: TSkin;
  p: TPoint3D;
  i: integer;
  offset: integer;
  angle: real;
begin
  //angle := (pi / 2) * (PLATE_LENGTH / (CONVEYOR_SPEED / PLATE_LENGTH));
  angle := pi / 2;

  m_support := TSupport.Support(Centre.p);
  m_support.AddNode(Point3DFromCoords(Centre.p.x - m_width div 2, Centre.p.y - 25, Centre.p.z));
  m_support.AddNode(Point3DFromCoords(Centre.p.x - m_width div 2, Centre.p.y + 25, Centre.p.z));
  m_support.AddNode(Point3DFromCoords(Centre.p.x + m_width div 2, Centre.p.y - 25, Centre.p.z));
  m_support.AddNode(Point3DFromCoords(Centre.p.x - m_width div 2, Centre.p.y - 25, Centre.p.z + m_nbPlates * PLATE_LENGTH));
  m_support.AddNode(Point3DFromCoords(Centre.p.x - m_width div 2, Centre.p.y + 25, Centre.p.z + m_nbPlates * PLATE_LENGTH));
  m_support.AddNode(Point3DFromCoords(Centre.p.x + m_width div 2, Centre.p.y - 25, Centre.p.z + m_nbPlates * PLATE_LENGTH));
  m_support.AddNode(Point3DFromCoords(Centre.p.x, Centre.p.y, Centre.p.z));
  m_support.AddNode(Point3DFromCoords(Centre.p.x, Centre.p.y, Centre.p.z + m_nbPlates * PLATE_LENGTH));
  m_support.AddNode(Point3DFromCoords(Centre.p.x + 50, Centre.p.y, Centre.p.z));

  skin := TSkin.Skin;

  // base x10, top side
  for i := 0 to m_nbPlates do begin
    p := Point3DFromCoords(Centre.p.x, Centre.p.y + 35.355339, Centre.p.z + PLATE_LENGTH * i);

    sup := TSupport.Support(p);
    // 70.710678 = 50*sqrt(2)
    // 35.355339 = ^ / 2
    sup.AddNode(Point3DFromCoords(p.x - m_width div 2, p.y, p.z));
    sup.AddNode(Point3DFromCoords(p.x + m_width div 2, p.y, p.z));
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
  p := Point3DFromCoords(Centre.p.x, Centre.p.y - 35.355339, Centre.p.z + m_nbPlates * PLATE_LENGTH);
  //RotateNode(p, GetRotatedPoint(m_support.Nodes[7]^), -pi / 2, 0, 0);
  RotateNode(p, GetRotatedPoint(m_support.Nodes[7]^), -angle, 0, 0);
  sup := TSupport.Support(p);
  sup.AddNode(Point3DFromCoords(p.x - m_width div 2, p.y, p.z));
  sup.AddNode(Point3DFromCoords(p.x + m_width div 2, p.y, p.z));
  Entities.Add(sup);
  skin.BindQuad((Entities[offset - 1] as TSupport).Nodes[0],
        sup.Nodes[0],
        sup.Nodes[1],
        (Entities[offset - 1] as TSupport).Nodes[1],
        clGray,
        clSilver);
  offset := Entities.Count;
  // base x10, bottom side
  for i := m_nbPlates downto 0 do begin
    p := Point3DFromCoords(Centre.p.x, Centre.p.y - 35.355339, Centre.p.z + PLATE_LENGTH * (i));

    sup := TSupport.Support(p);
    sup.AddNode(Point3DFromCoords(p.x - m_width div 2, p.y, p.z));
    sup.AddNode(Point3DFromCoords(p.x + m_width div 2, p.y, p.z));
    Entities.Add(sup);

    if i = m_nbPlates then
      skin.BindQuad((Entities[offset - 1] as TSupport).Nodes[0],
                sup.Nodes[0],
                sup.Nodes[1],
                (Entities[offset - 1] as TSupport).Nodes[1],
                clGray,
                clSilver)
    else
      skin.BindQuad((Entities[offset + (m_nbPlates - i) - 1] as TSupport).Nodes[0],
                sup.Nodes[0],
                sup.Nodes[1],
                (Entities[offset + (m_nbPlates - i) - 1] as TSupport).Nodes[1],
                clGray,
                clSilver);
  end;
  offset := Entities.Count;
  // back two blips
  //N.B. last line if Entities[0]
  p := Point3DFromCoords(Centre.p.x, Centre.p.y - 35.355339, Centre.p.z);
  RotateNode(p, GetRotatedPoint(m_support.Nodes[6]^), angle, 0, 0);
  sup := TSupport.Support(p);
  sup.AddNode(Point3DFromCoords(p.x - m_width div 2, p.y, p.z));
  sup.AddNode(Point3DFromCoords(p.x + m_width div 2, p.y, p.z));
  Entities.Add(sup);
  skin.BindQuad((Entities[offset - 1] as TSupport).Nodes[0],
        sup.Nodes[0],
        sup.Nodes[1],
        (Entities[offset - 1] as TSupport).Nodes[1],
        clGray,
        clSilver);
  skin.BindQuad(sup.Nodes[0],
        (Entities[0] as TSupport).Nodes[0],
        (Entities[0] as TSupport).Nodes[1],
        sup.Nodes[1],
        clGray,
        clSilver);
  // done with plates
  m_platesEnd := Entities.Count - 1;

  // add skin
  Entities.Add(skin);

  // add the frame
  Entities.Add(m_support);
end;

procedure TTestConveyor.Loop;
var
  v, reverseV: TPoint3D;
  side: TPlanarity;
  angle: real;
  OB: TPoint3D;
  frontPlane, backPlane, horizPlane: TPolygon;
  i: integer;
  vect: TPoint3D;
  cosz, sinz: real;
  crAngle: real;
  arbitraryValue: real;
  e: IWorldEntity;
begin
  // get the correct vector
  v := GetTranslationVectorPerFrame;
  reverseV := Point3DFromCoords(-v.x, -v.y, -v.z);

  // get angular speed
  angle := (pi / 2) * (CONVEYOR_SPEED / PLATE_LENGTH);
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
  horizPlane := TPolygon.Triangle(
                GetRotatedPoint(m_support.Nodes[6]^),
                GetRotatedPoint(m_support.Nodes[7]^),
                GetRotatedPoint(m_support.Nodes[8]^));

  (* find rotation around OZ *)
  (* TODO
  OB := GetRotatedPoint(m_support.Nodes[8]^);
  SubstractVector(OB, GetRotatedPoint(m_c));
  NormalizeVector(OB);

  arbitraryValue := OB.y;
  cosz := arbitraryValue;
  sinz := sin(arccos(cosz));
  *)
  (* TODO
  OB.z := 0;
  NormalizeVector(OB);
  cosz := DotProduct(OB, oz);
  sinz := ModulusOfVector(CrossProduct(OB, oz)); *)
  sinz := 1.0;
  cosz := 0.0;

  (* define the vector for the theta=90* case *)
  vect := GetRotatedPoint(m_support.Nodes[6]^);
  SubstractVector(vect, GetRotatedPoint(m_support.Nodes[7]^));
  vect := Point3DFromCoords(vect.x, 0.0, vect.z);
  NormalizeVector(vect);

  // rotate plates by a smidgun on the correct vector
  for i := 0 to m_platesEnd do begin
    side := SideOfPlane(
        frontPlane,
        GetRotatedPoint((Entities[i] as TSupport).Location));

    if (side = plBehind) then begin
      (* theta = 1, fi = 0 when rotZ = 0
         theta = 0, fi = 1 when rotZ = 1 etc
      *)
      crAngle := angle;
      if SideOfPlane(vect,
              GetRotatedPoint(m_support.Nodes[7]^),
              GetRotatedPoint((Entities[i] as TSupport).Location))
              = plFront then
        crAngle := -crAngle;

      Entities[i].RotatePolar(
              GetRotatedPoint(m_support.Nodes[7]^),
              crAngle * sinz,
              angle * cosZ);

      continue;
    end else begin
      side := SideOfPlane(
        backPlane,
        GetRotatedPoint((Entities[i] as TSupport).Location));

      crAngle := -angle;
      if SideOfPlane(vect,
              GetRotatedPoint(m_support.Nodes[6]^),
              GetRotatedPoint((Entities[i] as TSupport).Location))
              = plBehind then
        crAngle := -crAngle;

      if (side = plFront) then begin
        Entities[i].RotatePolar(
                GetRotatedPoint(m_support.Nodes[6]^),
                crAngle * sinz,
                angle * cosz);

        continue;
      end else begin
          side := SideOfPlane(horizPlane, GetRotatedPoint((Entities[i] as TSupport).Location));

          if side = plFront then
            Entities[i].Translate(v)
          else
            Entities[i].Translate(reverseV);
      end;
    end;
  end;


  frontPlane.Free;
  backPlane.Free;
  horizPlane.Free;

  // move the entities on the conveyor by a smidgun on the correct vector
  if InanimateObjects.Count > 0 then
  for i := 0 to InanimateObjects.Count - 1 do
    InanimateObjects[i].Translate(v);

  // if at correct phase, ask m_inputs for some input
  if TryGrab(GrabbingBox, e) then
    InanimateObjects.Add(e);
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

function TTestConveyor.GrabbingBox: PBoundingBox;
var
  c: TPoint3D;
begin
  c := GetRotatedPoint(m_c);
  m_grabbingBox.p1 := Point3DFromCoords(c.x - m_width / 2, c.y - PLATE_LENGTH / 2, c.z - PLATE_LENGTH / 2);
  m_grabbingBox.p2 := Point3DFromCoords(c.x + m_width / 2, c.y + PLATE_LENGTH / 2, c.z + PLATE_LENGTH / 2);

  GrabbingBox := @m_grabbingBox;
end;

end.

