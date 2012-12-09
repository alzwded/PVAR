unit GrimReaper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoreUtils, GfxUtils, Graphics;

const
  NAIL_WIDTH = 50;
  NAIL_HEIGHT = 200;

  GRIMREAPER_SPIKE_SPLOSION_TRIGGER_AT = 0.7;

  SPLODE_AMOUNT = 8;
  SPLODE_FOR = 20;

  Splosion_Top = 0;
  Splosion_Left = 1;
  Splosion_Back = 2;
  Splosion_Right = 3;
  Splosion_Front = 4;
  Splosion_Bottom = 5;

  Splosion_TopVector: TPoint3D = (x: 0; y: 1; z: 0); (* honestly, with the syntax? *)
  Splosion_BottomVector: TPoint3D = (x: 0; y: -1; z: 0);
  Splosion_RightVector: TPoint3D = (x: 1; y: 0; z: 0);
  Splosion_LeftVector: TPoint3D = (x: -1; y: 0; z: 0);
  Splosion_FrontVector: TPoint3D = (x: 0; y: 0; z: 1);
  Splosion_BackVector: TPoint3D = (x: 0; y: 0; z: -1);

  Splosion_t1 = 0; (*       back   (top-  *)
  Splosion_t2 = 1; (*        /\      view)*)
  Splosion_t3 = 2; (*       /| \          *)
  Splosion_t4 = 3; (* left /1|_2\ right   *)
  Splosion_b1 = 4; (*      \4| 3/         *)
  Splosion_b2 = 5; (*       \| /          *)
  Splosion_b3 = 6; (*        \/           *)
  Splosion_b4 = 7; (*       front         *)

  GrimReaper_Splosion = 0;
  GrimReaper_BedOfNails = 1;

type
  TBedOfNails = class(TPart)
    constructor BedOfNails(c: TPoint3D; u, v: integer);
    procedure InitMesh; override;
  private
    m_u, m_v: integer;
  end;

  TSplosionState = (
    xsIdle,
    xsArmed,
    xsBoomA,
    xsBoomB);

  TSplosion = class(ACompound)
    constructor Splosion(c: TPoint3D);
    procedure Init; override;
    procedure Loop; override;
    procedure Splode;
  private
    m_state: TSplosionState;
    m_phase: integer;

    procedure ToggleColours;
    procedure Enlarge;
    procedure Defuse;
  end;

  TGrimReaper = class(AGrabber)
    constructor GrimReaper(c: TPoint3D);
    destructor Destroy; override;
    procedure Init; override;
    procedure Loop; override;
  private
    m_bbox: TBoundingBox;
  end;

procedure MoveVectorTo(var p: TRealPoint3D; where: TPoint3D);

implementation

procedure MoveVectorTo(var p: TRealPoint3D; where: TPoint3D);
begin
  p.p := where;
  p.rx := 0;
  p.rz := 0;
  p.ry := 0;
  p.rotationCentre := where;
end;

(* TBedOfNails *)

constructor TBedOfNails.BedOfNails(c: TPoint3D; u, v: integer);
begin
  m_u := u;
  m_v := v;

  inherited Part(c);
end;

procedure TBedOfNails.InitMesh;
var
  i, j: integer;
  p: TPoint3D;
  e: TPolygon;
begin
  p := GetLocation;
  p.x := p.x - NAIL_WIDTH * m_u div 2;
  p.z := p.z - NAIL_WIDTH * m_v div 2;

  for i := 0 to m_u - 1 - 1 do begin
    for j := 0 to m_v - 1 - 1 do begin
      e := TPolygon.Triangle(
                Point3DFromCoords(p.x + NAIL_WIDTH * i, p.y, p.z + NAIL_WIDTH * j),
                Point3DFromCoords(p.x + NAIL_WIDTH * i, p.y, p.z + NAIL_WIDTH * (j + 1)),
                Point3DFromCoords(p.x + NAIL_WIDTH * i + NAIL_WIDTH / 2, p.y + NAIL_HEIGHT, p.z + NAIL_WIDTH * j + NAIL_WIDTH / 2));
      e.ContourColour:=clBlack;
      e.FillColour:=RGBToColor(20, 20, 20);
      Geometry.Add(e);

      e := TPolygon.Triangle(
                Point3DFromCoords(p.x + NAIL_WIDTH * (i + 1), p.y, p.z + NAIL_WIDTH * j),
                Point3DFromCoords(p.x + NAIL_WIDTH * i, p.y, p.z + NAIL_WIDTH * j),
                Point3DFromCoords(p.x + NAIL_WIDTH * i + NAIL_WIDTH / 2, p.y + NAIL_HEIGHT, p.z + NAIL_WIDTH * j + NAIL_WIDTH / 2));
      e.ContourColour:=clBlack;
      e.FillColour:=RGBToColor(20, 20, 20);
      Geometry.Add(e);

      e := TPolygon.Triangle(
                Point3DFromCoords(p.x + NAIL_WIDTH * (i + 1), p.y, p.z + NAIL_WIDTH * (j + 1)),
                Point3DFromCoords(p.x + NAIL_WIDTH * (i + 1), p.y, p.z + NAIL_WIDTH * j),
                Point3DFromCoords(p.x + NAIL_WIDTH * i + NAIL_WIDTH / 2, p.y + NAIL_HEIGHT, p.z + NAIL_WIDTH * j + NAIL_WIDTH / 2));
      e.ContourColour:=clBlack;
      e.FillColour:=RGBToColor(20, 20, 20);
      Geometry.Add(e);

      e := TPolygon.Triangle(
                Point3DFromCoords(p.x + NAIL_WIDTH * i, p.y, p.z + NAIL_WIDTH * (j + 1)),
                Point3DFromCoords(p.x + NAIL_WIDTH * (i + 1), p.y, p.z + NAIL_WIDTH * (j + 1)),
                Point3DFromCoords(p.x + NAIL_WIDTH * i + NAIL_WIDTH / 2, p.y + NAIL_HEIGHT, p.z + NAIL_WIDTH * j + NAIL_WIDTH / 2));
      e.ContourColour:=clBlack;
      e.FillColour:=RGBToColor(20, 20, 20);
      Geometry.Add(e);
    end;
  end;
end;

(* TSplosion *)

constructor TSplosion.Splosion(c: TPoint3D);
begin
  m_state := xsIdle;
  m_phase := 0;

  inherited Compound(c, 37);
end;

procedure TSplosion.Init;
var
  sup: TSupport;
  skin: TSkin;
  i: integer;
begin
  sup := TSupport.Support(Centre.p);
  for i := 1 to 6 do
    sup.AddNode(Centre.p);
  Entities.Add(sup);

  skin := TSkin.Skin;
  skin.BindTria(
        sup.Nodes[Splosion_Top],
        sup.Nodes[Splosion_Back],
        sup.Nodes[Splosion_Left],
        clWhite,
        clYellow);
  skin.BindTria(
        sup.Nodes[Splosion_Top],
        sup.Nodes[Splosion_Right],
        sup.Nodes[Splosion_Back],
        clWhite,
        clRed);
  skin.BindTria(
        sup.Nodes[Splosion_Top],
        sup.Nodes[Splosion_Front],
        sup.Nodes[Splosion_Right],
        clWhite,
        clYellow);
  skin.BindTria(
        sup.Nodes[Splosion_Top],
        sup.Nodes[Splosion_Left],
        sup.Nodes[Splosion_Front],
        clWhite,
        clRed);
  skin.BindTria(
        sup.Nodes[Splosion_Bottom],
        sup.Nodes[Splosion_Left],
        sup.Nodes[Splosion_Back],
        clWhite,
        clYellow);
  skin.BindTria(
        sup.Nodes[Splosion_Bottom],
        sup.Nodes[Splosion_Back],
        sup.Nodes[Splosion_Right],
        clWhite,
        clRed);
  skin.BindTria(
        sup.Nodes[Splosion_Bottom],
        sup.Nodes[Splosion_Right],
        sup.Nodes[Splosion_Front],
        clWhite,
        clYellow);
  skin.BindTria(
        sup.Nodes[Splosion_Bottom],
        sup.Nodes[Splosion_Front],
        sup.Nodes[Splosion_Left],
        clWhite,
        clRed);
  skin.Hidden := True;
  Entities.Add(skin);
end;

procedure TSplosion.Loop;
begin
  case m_state of
  xsIdle: exit;
  xsArmed: begin
    m_state := xsBoomA;
    m_phase := 0;
    end;
  xsBoomA: begin
    if m_phase < SPLODE_FOR then begin
      ToggleColours;
      Enlarge;
      m_state := xsBoomB;
      inc(m_phase);
    end
    else
      Defuse;
    end;
  xsBoomB: begin
    if m_phase < SPLODE_FOR then begin
      ToggleColours;
      Enlarge;
      m_state := xsBoomA;
      inc(m_phase);
    end
    else
      Defuse;
    end;
  end;
end;

procedure TSplosion.Enlarge;
var
  v: TPoint3D;
begin
  v := Point3DFromCoords(
        (m_phase + 1) * Splosion_TopVector.x * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_TopVector.y * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_TopVector.z * SPLODE_AMOUNT);
  TranslateVector(v, GetLocation);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Top]^, v);

  v := Point3DFromCoords(
        (m_phase + 1) * Splosion_LeftVector.x * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_LeftVector.y * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_LeftVector.z * SPLODE_AMOUNT);
  TranslateVector(v, GetLocation);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Left]^, v);

  v := Point3DFromCoords(
        (m_phase + 1) * Splosion_BackVector.x * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_BackVector.y * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_BackVector.z * SPLODE_AMOUNT);
  TranslateVector(v, GetLocation);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Back]^, v);

  v := Point3DFromCoords(
        (m_phase + 1) * Splosion_RightVector.x * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_RightVector.y * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_RightVector.z * SPLODE_AMOUNT);
  TranslateVector(v, GetLocation);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Right]^, v);

  v := Point3DFromCoords(
        (m_phase + 1) * Splosion_FrontVector.x * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_FrontVector.y * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_FrontVector.z * SPLODE_AMOUNT);
  TranslateVector(v, GetLocation);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Front]^, v);

  v := Point3DFromCoords(
        (m_phase + 1) * Splosion_BottomVector.x * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_BottomVector.y * SPLODE_AMOUNT,
        (m_phase + 1) * Splosion_BottomVector.z * SPLODE_AMOUNT);
  TranslateVector(v, GetLocation);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Bottom]^, v);

  Entities[0].Rotate(pi / 6 * m_phase, pi / 12 * m_phase, pi / 24 * m_phase);
end;

procedure TSplosion.ToggleColours;
var
  cl1, cl2: TColor;
begin
  if (m_phase mod 2) = 1 then begin
    cl1 := RGBToColor(255, 100, 0);
    cl2 := clYellow;
  end else begin
    cl1 := clYellow;
    cl2 := RGBToColor(255, 100, 0);
  end;

  ((Entities[1] as TSkin).Geometry[Splosion_t1] as TPointerPolygon).FillColour:=cl1;
  ((Entities[1] as TSkin).Geometry[Splosion_t2] as TPointerPolygon).FillColour:=cl2;
  ((Entities[1] as TSkin).Geometry[Splosion_t3] as TPointerPolygon).FillColour:=cl1;
  ((Entities[1] as TSkin).Geometry[Splosion_t4] as TPointerPolygon).FillColour:=cl2;
  ((Entities[1] as TSkin).Geometry[Splosion_b1] as TPointerPolygon).FillColour:=cl2;
  ((Entities[1] as TSkin).Geometry[Splosion_b2] as TPointerPolygon).FillColour:=cl1;
  ((Entities[1] as TSkin).Geometry[Splosion_b3] as TPointerPolygon).FillColour:=cl2;
  ((Entities[1] as TSkin).Geometry[Splosion_b4] as TPointerPolygon).FillColour:=cl1;
end;

procedure TSplosion.Splode;
var
  v, rp: TPoint3D;
begin
  m_state := xsArmed;
  rp := GetRotatedPoint(Centre);

  v := Splosion_TopVector;
  TranslateVector(v, rp);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Top]^, v);
  v := Splosion_BottomVector;
  TranslateVector(v, rp);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Bottom]^, v);

  v := Splosion_LeftVector;
  TranslateVector(v, rp);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Left]^, v);
  v := Splosion_RightVector;
  TranslateVector(v, rp);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Right]^, v);

  v := Splosion_FrontVector;
  TranslateVector(v, rp);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Front]^, v);
  v := Splosion_BackVector;
  TranslateVector(v, rp);
  MoveVectorTo((Entities[0] as TSupport).Nodes[Splosion_Back]^, v);

  Entities[1].Hidden := false;
end;

procedure TSplosion.Defuse;
begin
  m_state := xsIdle;
  m_phase := 0;
  Entities[1].Hidden := true;
end;

(* TGrimReaper *)

constructor TGrimReaper.GrimReaper(c: TPoint3D);
begin
  m_bbox.p1 := Point3DFromCoords(c.x - 50, c.y - 50, c.z - 50);
  m_bbox.p2 := Point3DFromCoords(c.x + 50, c.y + 50, c.z + 50);

  inherited Grabber(c, 10);
end;

destructor TGrimReaper.Destroy;
begin
  inherited Destroy;
end;

procedure TGrimReaper.Init;
begin
  Entities.Add(TSplosion.Splosion(GetLocation));
  Entities.Add(TBedOfNails.BedOfNails(
        Point3DFromCoords(
                GetLocation.x,
                GetLocation.y - NAIL_HEIGHT * GRIMREAPER_SPIKE_SPLOSION_TRIGGER_AT,
                GetLocation.z),
        6, 6));
end;

procedure TGrimReaper.Loop;
var
  e: IWorldEntity;
begin
  e := Nil;
  if TryGrab(@m_bbox, true, e) then begin
    e.Free;
    (Entities[GrimReaper_Splosion] as TSplosion).Splode;
  end;
end;

end.

