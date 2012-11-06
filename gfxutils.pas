unit GfxUtils;

{$mode objfpc}{$H+}

(* enable short-circuiting boolean evaluation (DO NOT DISABLE THIS) *)
{$B-}

{$DEFINE CHECK_PLANARITY}
(*EFINE DEBUG_ADD_ENTITY*)
(*$DEFINE AGGRESSIVE_CLIPPING*)

(* utilities for creating, sorting and rendering 3d triangles, quadrangles,
   spheres and sprites to a canvas *)
(* available 3d entities:
   Triangle, via TPolygon.Triangle
   Quandrangle, via TPolygon.Quad
   Sphere, TSphere.Sphere
   Sprite, TSprite.Sprite

   for tria, quad and sphere you can set outline and fill colours via
      ContourColour and FillColour
   for Sprite you need to set its in-world width and height

   limitation: currently, all the nodes of a quad need to be in the same plane
*)
(* the class that handles the projection of things is TJakRandrProjector
   this class does not do any sorting and our-right renders anything passed to
   it

   use InOrder(a,b) to determine if things are correctly z-ordered and only then
   make calls to TJakRandrProjector.Draw

   limitation: polygon clipping is not (yet) supported
      a polygon is either completely in front or completely behind another
*)

(* new plan:
   everything is stored with TRealPoint3D to prevent data loss in rotations
   on rendering everything is rotated to face camera (in AddEntity)
       Newell's algorithm should THEN work
*)
interface

uses
  Classes, SysUtils, Graphics, fgl, Math;

const
  MIN_CAMERA_DISTANCE = 50.0;

type
  TPoint3D = record
    x, y, z: real;
  end;

  TRealPoint3D = record
    p: TPoint3D;
    rotationCentre: TPoint3D;
    rx, ry, rz: real;
  end;

  IEntity3D = interface(IInterface)
    procedure Translate(dp: TPoint3D);
    procedure Rotate(centre: TPoint3D; rotx, roty, rotz: real);
    procedure Dump;
    function GetFacingCamera(O: TPoint3D; rx, ry, rz: real): IEntity3D;
  end;

  TLine = class(TInterfacedObject, IEntity3D)
    constructor Line(p1, p2: TPoint3D);
    (* implementation of IEntity3D *)
    procedure Translate(dp: TPoint3D);
    procedure Rotate(centre: TPoint3D; rotx, roty, rotz: real);
    procedure Dump;
    function GetFacingCamera(O: TPoint3D; rx, ry, rz: real): IEntity3D;
  private
    m_nodes: array[0..1] of TRealPoint3D;
    m_contourColour: TColor;

    function GetNode(i: integer): TRealPoint3D;
    function GetNbNodes: integer; // because delphi doesn't do class constants
  public
    property Nodes[i: integer]: TRealPoint3D read GetNode;
    property NbNodes: Integer read GetNbNodes;
    property ContourColour: TColor read m_contourColour write m_contourColour;
  end;

  TSphere = class(TInterfacedObject, IEntity3D)
    constructor Sphere(centre: TPoint3D; radius: real);
    (* implementation of IEntity3D *)
    procedure Translate(dp: TPoint3D);
    procedure Rotate(centre: TPoint3D; rotx, roty, rotz: real);
    procedure Dump;
    function GetFacingCamera(O: TPoint3D; rx, ry, rz: real): IEntity3D;
  private
    m_c: TRealPoint3D;
    m_r: real;
    m_contourColour: TColor;
    m_fillColour: TColor;
  public
    property Centre: TRealPoint3D read m_c;
    property Radius: Real read m_r;
    property ContourColour: TColor read m_contourColour write m_contourColour;
    property FillColour: TColor read m_fillColour write m_fillColour;
  end;

  TPolygon = class(TInterfacedObject, IEntity3D)
    constructor Triangle(p1, p2, p3: TPoint3D);
    constructor Quad(p1, p2, p3, p4: TPoint3D);
    destructor Destroy; override;
    (* implementation of IEntity3D *)
    procedure Translate(dp: TPoint3D);
    procedure Rotate(centre: TPoint3D; rotx, roty, rotz: real);
    procedure Dump;
    function GetFacingCamera(O: TPoint3D; rx, ry, rz: real): IEntity3D;
  private
    m_n: Integer;
    m_nodes: array of TRealPoint3D;
    m_contourColour: TColor;
    m_fillColour: TColor;

    function GetNode(i: integer): TRealPoint3D;
  public
    property Nodes[i: integer]: TRealPoint3D read GetNode;
    property NbNodes: Integer read m_n;
    property ContourColour: TColor read m_contourColour write m_contourColour;
    property FillColour: TColor read m_fillColour write m_fillColour;
  end;

  TSprite = class(TInterfacedObject, IEntity3D)
    constructor Sprite(centre: TPoint3D; graphic: TBitmap; width, height: real);
    destructor Destroy; override;
    (* implementation of IEntity3D *)
    procedure Translate(dp: TPoint3D);
    procedure Rotate(centre: TPoint3D; rotx, roty, rotz: real);
    procedure Dump;
    function GetFacingCamera(O: TPoint3D; rx, ry, rz: real): IEntity3D;
  private
    m_c: TRealPoint3D;
    m_g: TBitmap;
    m_width: real;
    m_height: real;
  public
    property Centre: TRealPoint3D read m_c;
    property Graphic: TBitmap read m_g;
    property Width: Real read m_width write m_width;
    property Height: Real read m_height write m_height;
  end;

  TPlanarity = ( plOn, plFront, plBehind ); (* points ON the plane are ignored *)

  TEntity3DList = specialize TFPGInterfacedObjectList<IEntity3D>;

  (* projects triangles, quadrangles, spheres or sprites to canvas *)
  TJakRandrProjector = class(TObject)
    (* constructors *)
    constructor Create(canvas: TCanvas; bgColor: TColor);

    (* draw 3d entities *)
    procedure Draw(entity: IEntity3D);
  private
    procedure DrawPolygon(poli: TPolygon);
    procedure DrawSphere(sphere: TSphere);
    procedure DrawSprite(sprite: TSprite);
    procedure DrawLine(line: TLine);

  public
    (* draw 2d entities *)
    procedure DrawPoint(p: TPoint3D; color: TColor);
    procedure DrawLine(p1, p2: TPoint3D; color: TColor);

    (* draw other *)
    procedure Clear;

    function GetViewportLocation: TPoint3D;

  private
    m_O: TPoint3D;
    m_rx, m_ry, m_rz: real;
    m_focalDistance: real;

  public
    property O: TPoint3D read m_O write m_O;
    property CRX: real read m_rx write m_rx;
    property CRY: real read m_ry write m_ry;
    property CRZ: real read m_rz write m_rz;
    property CAMERA_DISTANCE: real read m_focalDistance write m_focalDistance;

  private
    m_canvas: TCanvas;
    m_bgColor: TColor;

    function Project(p: TPoint3D): TPoint;
    function DistanceToViewport(p: TPoint3D): real;
  public
    property Background: TColor read m_bgColor write m_bgColor;
  public (* need InOrder* functions here because I need m_z_*Component *)
    (* entry function for determining if e1 is closer to the viewport than e2
       true means e1 is closer than e2, hence e2 should be drawn first *)
    function InOrder(e1, e2: IEntity3D): boolean;
  private
    (* implementation for sphere-sphere *)
    function InOrderSpheres(s1, s2: TSphere): boolean;
    (* implementation for sphere-polygon *)
    function InOrderSpherePolygon(s: TSphere; p: TPolygon): boolean;
    (* wrapper for not InOrderSpherePolygon *)
    function InOrderPolygonSphere(p: TPolygon; s: TSphere): boolean;
    (* implementation of Newell's algorithm *)
    function InOrderPolygons(t1, t2: TPolygon): boolean;
    (* implementation for sprites *)
    function InOrderSprites(s1, s2: TSprite): boolean;
    (* implementation for sprite-sphere *)
    function InOrderSpriteSphere(sprite: TSprite; s: TSphere): boolean;
    (* implementation for sphere-sprite *)
    function InOrderSphereSprite(s: TSphere; sprite: TSprite): boolean;
    (* implementation for sprite-polygon *)
    function InOrderSpritePolygon(sprite: TSprite; p: TPolygon): boolean;
    (* implementation for polygon-sprite *)
    function InOrderPolygonSprite(p: TPolygon; sprite: TSprite): boolean;
    (* implementation for line-line *)
    function InOrderLines(l1, l2: TLine): boolean;
    (* implementation for line-Polygon *)
    function InOrderLinePolygon(l: TLine; p: TPolygon): boolean;
    (* implementation for Polygon-line *)
    function InOrderPolygonLine(p: TPolygon; l: TLine): boolean;
    (* implementation for line-sprite *)
    function InOrderLineSprite(l: TLine; s: TSprite): boolean;
    (* implementation for sprite-line *)
    function InOrderSpriteLine(s: TSprite; l: TLine): boolean;
    (* implementation for line-sphere *)
    function InOrderLineSphere(l: TLine; s: TSphere): boolean;
    (* implementation for sphere-line *)
    function InOrderSphereLine(s: TSphere; l: TLine): boolean;
  end;

  (* TBD: see if a buffer is necessary of if Canvas suffices
     -- it is *)
  TJakRandrEngine = class(TObject)
    constructor Create(canvas: TCanvas; bgColor: TColor);
    destructor Destroy; override;
    (* cleans the entity list and the frame buffer *)
    procedure BeginScene;
    (* draws the entities in z order*)
    procedure CommitScene;
    (* clear the entity list *)
    procedure AbortScene;
    procedure AddEntity(entity: IEntity3D);
  private
    m_visu: TJakRandrProjector;
    m_buffer: TBitmap;
    m_canvas: TCanvas;
    m_entities: TEntity3DList;

    procedure ClearEntities;
  public
    property O: TPoint3D read m_visu.m_O write m_visu.m_O;
    property RX: real read m_visu.m_rx write m_visu.m_rx;
    property RY: real read m_visu.m_ry write m_visu.m_ry;
    property RZ: real read m_visu.m_rz write m_visu.m_rz;
    property D: real read m_visu.m_focalDistance write m_visu.m_focalDistance;
  end;

  PJakRandrEngine = ^TJakRandrEngine;

procedure TranslateVector(var p: TPoint3D; dp: TPoint3D);
procedure SubstractVector(var p: TPoint3D; dp: TPoint3D);
function Point3DFromCoords(x, y, z: real): TPoint3D;
function RealPoint3DFromCoords(x, y, z: real): TRealPoint3D;
function RealPoint3DFromPoint(p: TPoint3D): TRealPoint3D;
function GetRotatedPoint(p: TRealPoint3D): TPoint3D;
function Centroid(p: TPolygon): TPoint3D;

(* warning! careful what you do with this
   if unsure, just don't use it and apply rotations manually
*)
procedure ApplyRotationToPoint(
  var p: TRealPoint3D;
  rotCentre: TRealPoint3D;
  rx, ry, rz: real);

function IsPointInPolygon(
        p: TPoint;
        poli: array of TPoint;
        start, count: integer)
: boolean;

procedure incr(var whom: real; value: real = 1.0);
procedure decr(var whom: real; value: real = 1.0);

(* check if p is in front, behind of or on the plane *)
function SideOfPlane(plane: TPolygon; p: TPoint3D): TPlanarity;
function SideOfPlane(normal: TPoint3D; pointOnPlane, p: TPoint3D): TPlanarity;
(* returns the normalized vector of the plane's normal *)
function NormalForPlane(plane: TPolygon): TPoint3D;
function DotProduct(v1, v2: TPoint3D): real;
function CrossProduct(v1, v2: TPoint3D): TPoint3D;
procedure NormalizeVector(var v: TPoint3D);
function Distance(p1, p2: TPoint3D): real;
procedure RotateNode(var node: TPoint3D; centre: TPoint3D; rx, ry, rz: real);

implementation

(* TJakRandrEngine *)

constructor TJakRandrEngine.Create(canvas: TCanvas; bgColor: TColor);
begin
  m_entities := TEntity3DList.Create;
  m_canvas := canvas;
  m_buffer := TBitmap.Create;
  m_visu := TJakRandrProjector.Create(m_buffer.Canvas, bgColor);
end;

destructor TJakRandrEngine.Destroy;
begin
  m_visu.Free;
  m_entities.Free;
  m_buffer.Free;
end;

procedure nop; begin end;
procedure TJakRandrEngine.ClearEntities;
var
  i: integer;
begin
  (* yeah, I really don't understand how delphi uses COM internally,
     but sometimes I get a ref count of 2 here, which means mem leak,
     (at least on linux) so fuck you delphi *)
  for i := 0 to m_entities.Count - 1 do
    while m_entities[i]._Release > 1 do nop;

  m_entities.Clear;
end;

procedure TJakRandrEngine.BeginScene;
begin
  m_buffer.Width := m_canvas.Width;
  m_buffer.Height := m_canvas.Height;
  m_buffer.Canvas.Pen.Width := 1;
  ClearEntities;
  m_visu.Clear;
end;

procedure TJakRandrEngine.AbortScene;
begin
  ClearEntities;
  m_visu.Clear;
end;

procedure TJakRandrEngine.CommitScene;
var
  i: integer;
begin
  for i := 0 to m_entities.Count - 1 do begin
    m_visu.Draw(m_entities.Items[i])
  end;
  m_canvas.Draw(0, 0, m_buffer);
end;

procedure TJakRandrEngine.AddEntity(entity: IEntity3D);
var
  i: integer;
  candidate: IEntity3D;
begin
  // FIXME problem is here
  candidate := entity.GetFacingCamera(
        m_visu.O,
        m_visu.m_rx,
        m_visu.m_ry,
        m_visu.m_rz);

  (*$IFDEF DEBUG_ADD_ENTITY*)
  writeln('Begin sorting new entity');
  candidate.Dump;
  (*$ENDIF*)
  //for i := 0 to m_entities.Count - 1 do begin
  i := m_entities.Count;
  while (i > 0)
        and (m_visu.InOrder(candidate, m_entities.Items[i - 1]))
  do begin
    (*$IFDEF DEBUG_ADD_ENTITY*)
    writeln('  comparing with:');
    write('  '); m_entities.Items[i - 1].Dump;
    (*$ENDIF*)
    dec(i);
  end;

  (*$IFDEF DEBUG_ADD_ENTITY*)
  writeln(i, ' ', m_entities.Count);
  (*$ENDIF*)
  if i = m_entities.Count then begin
    (*$IFDEF DEBUG_ADD_ENTITY*)
    writeln('  inserting at end');
    (*$ENDIF*)
    m_entities.Add(candidate);
    exit;
  end;
  (*$IFDEF DEBUG_ADD_ENTITY*)
  writeln('  inserting at ', i);
  (*$ENDIF*)
  m_entities.Insert(i, candidate);
end;

(* TJakRandrProjector *)

constructor TJakRandrProjector.Create(canvas: TCanvas; bgColor: TColor);
begin
  if canvas = Nil then
    Raise Exception.Create('NULL pointer!');
  m_canvas := canvas;
  m_bgColor := bgColor;

  m_O := Point3DFromCoords(0, 0, 0);
  m_rx := 0.0; //-pi / 12.0;//pi / 3.0;(*pi / 2.0;*)(*pi / 12;*)
  m_ry := 0.0; //pi / 6.0;//pi / 4.0;(*pi / 6;*)
  m_rz := 0.0;//pi / 2.0;
  m_focalDistance := 5000;
end;

function TJakRandrProjector.DistanceToViewport(p: TPoint3D): real;
var
  viewport, origin: TPoint3D;
  distanceToCamera, distanceToLine: real;
  v1, v2: TPoint3D;
  c1, c2: real;
  b: real;
  Pb: TPoint3D;
begin
  viewport := GetViewportLocation;
  origin := O;

  DistanceToViewport := abs(viewport.z - p.z);
  exit;

  distanceToCamera := Distance(viewport, p);

  v1 := Point3DFromCoords(
        viewport.x - origin.x,
        viewport.y - origin.y,
        viewport.z - origin.z);
  v2 := Point3DFromCoords(
        p.x - origin.x,
        p.y - origin.y,
        p.z - origin.z);

  c1 := DotProduct(v1, v2);
  c2 := DotProduct(v1, v1); //Distance(viewport, origin);

  b := c1 / c2;

  Pb := Point3DFromCoords(
        origin.x + b * v2.x,
        origin.y + b * v2.y,
        origin.z + b * v2.z);

  distanceToLine := Distance(p, Pb);

  //TODO remove abs, do not draw offscreen entities
  DistanceToViewport := sqrt(abs(sqr(distanceToCamera) - sqr(distanceToLine)));
end;

function TJakRandrProjector.Project(p: TPoint3D): TPoint;
var
  ret: TPoint;
  rx, ry: real;
  s: real;
begin
  (* ideal window:
       4000 x 3000
     1. compute coords
     2. scale down from 4000x3000 to current window size
  *)
  (* apply camera rotation *)
  (* not needed, polys already rotated with camera, orthogonal projection *)

  rx := p.x;
  ry := p.y;

  (* offset from origin *)
  (* TODO add actual offsetCamera procedure and properties to class *)

  (* scaling factor *)
  (*s := CAMERA_DISTANCE / Distance(p, GetViewportLocation);*)
  (* above is a bit dumb because it takes the distance from the camera rather
     than the viewing plane...
     need to implement view plane and compute distance till there
     also, need to clip stuff out if it's behind the camera *)
  s := CAMERA_DISTANCE / DistanceToViewport(p);

  (* scale down *)
  rx := (rx * m_canvas.Width) / 4000.0 * s;
  ry := (ry * m_canvas.Height) / 3000.0 * s;
  (* note on relevance of z: *)
  (* rx := p.x * screenWidth / p.z; ry := p.y * screenWidth / p.z *)

  rx := rx + m_canvas.Width / 2;
  ry := ry + m_canvas.Height / 2;

  (* treat infinities *)
  if rx = Infinity then
    rx := 2 * m_canvas.Width
  else if rx = NegInfinity then
    ry := -2 * m_canvas.Width;

  if ry = Infinity then
    ry := 2 * m_canvas.Height
  else if ry = NegInfinity then
    ry := -2 * m_canvas.Height;

  (* convert to int, flip Y axis for normality's sake *)
  ret.X := round(rx);
  ret.Y := m_canvas.Height - round(ry);

  (* return *)
  Project := ret;
end;

procedure TJakRandrProjector.DrawPoint(p: TPoint3D; color: TColor);
var
  onScreenPoint: TPoint;
begin
  onScreenPoint := Project(p);

  m_canvas.Pixels[onScreenPoint.X, onScreenPoint.Y] := color;
end;

procedure TJakRandrProjector.DrawLine(p1, p2: TPoint3D; color: TColor);
var
  OSP_p1, OSP_p2: TPoint;
begin
  OSP_p1 := Project(p1);
  OSP_p2 := Project(p2);

  m_canvas.Pen.Color := color;
  m_canvas.Line(OSP_p1, OSP_p2);
end;

procedure TJakRandrProjector.DrawLine(line: TLine);
var
  p1, p2: TPoint;
  pWidth: integer;
  normal: TPoint3D;
  (*$IFNDEF AGGRESSIVE_CLIPPING*)
  thereExistsAtLeastOnPointInFrontOfClipPlane: boolean;
  (*$ENDIF*)
begin
  p1 := Project(GetRotatedPoint(line.Nodes[0]));
  p2 := Project(GetRotatedPoint(line.Nodes[1]));

  normal := Point3DFromCoords(
        O.x - GetViewportLocation.x,
        O.y - GetViewportLocation.y,
        O.z - GetViewportLocation.z);

  (*$IFNDEF AGGRESSIVE_CLIPPING*)
  thereExistsAtLeastOnePointInFrontOfClipPlane := false;
  (*$ENDIF*)

  if (SideOfPlane(normal, GetViewportLocation, GetRotatedPoint(line.Nodes[0]))
                <> plFront)
        (*$IFDEF AGGRESSIVE_CLIPPING*)
        or
        (*$ELSE*)
        and
        (*$ENDIF*)
        (SideOfPlane(normal, GetViewportLocation, GetRotatedPoint(line.Nodes[1]))
                <> plFront) then
    exit;

  // store pen width
  pWidth := m_canvas.Pen.Width;

  // draw
  m_canvas.Pen.Width := 2;
  m_canvas.Pen.Color := line.m_contourColour;
  m_canvas.Line(p1, p2);

  // restore pen width
  m_canvas.Pen.Width := pWidth;
end;

procedure TJakRandrProjector.Draw(entity: IEntity3D);
begin
  if entity is TPolygon then DrawPolygon(entity as TPolygon)
  else if entity is TSphere then DrawSphere(entity as TSphere)
  else if entity is TSprite then DrawSprite(entity as TSprite)
  else if entity is TLine then DrawLine(entity as TLine)
  ;
end;

procedure TJakRandrProjector.DrawPolygon(poli: TPolygon);
var
  points: array of TPoint;
  i: integer;
  normal, p: TPoint3D;
  (*$IFNDEF AGGRESSIVE_CLIPPING*)
  thereExistsAtLeastOnePointInFrontOfClipPlane: boolean;
  (*$ENDIF*)
begin
  normal := Point3DFromCoords(
        O.x - GetViewportLocation.x,
        O.y - GetViewportLocation.y,
        O.z - GetViewportLocation.z);

  SetLength(points, poli.NbNodes);

  (*$IFNDEF AGGRESSIVE_CLIPPING*)
  thereExistsAtLeastOnePointInFrontOfClipPlane := false;
  (*$ENDIF*)

  for i := 0 to poli.NbNodes - 1 do begin
    (* if poli.Nodes[i] behind clipping plane, return *)
    p := GetRotatedPoint(poli.Nodes[i]);
    (*$IFDEF AGGRESSIVE_CLIPPING*)
    if SideOfPlane(normal, GetViewportLocation, p) <> plFront then
      exit;
    (*$ELSE*)
    if (not thereExistsAtLeastOnePointInFrontOfClipPlane)
        and (SideOfPlane(normal, GetViewportLocation, p) = plFront) then
      thereExistsAtLeastOnePointInFrontOfClipPlane := true;
    (*$ENDIF*)

    points[i] := Project(p);
  end;

  (*$IFNDEF AGGRESSIVE_CLIPPING*)
  if not thereExistsAtLeastOnePointInFrontOfClipPlane then
    exit;
  (*$ENDIF*)

  m_canvas.Pen.color := poli.ContourColour;
  m_canvas.Brush.color := poli.FillColour;
  m_canvas.Brush.Style := bsSolid;

  m_canvas.Polyline(points, 0, poli.NbNodes);
  m_canvas.Polygon(points, False, 0, poli.NbNodes);
end;

procedure TJakRandrProjector.DrawSphere(sphere: TSphere);
var
  p: TPoint;
  rx, ry: integer;
begin
  p := Project(GetRotatedPoint(sphere.Centre));

  m_canvas.Pen.Color := sphere.ContourColour;
  m_canvas.Brush.Color := sphere.FillColour;
  m_canvas.Brush.Style := bsSolid;

  rx := round(sphere.Radius * m_canvas.Width / 4000.0);
  ry := round(sphere.Radius * m_canvas.Height / 3000.0);

  (* winding algorithm for drawing a sphere ffs *)
  while (rx > 0) and (ry > 0) do begin
    m_canvas.EllipseC(p.X, p.Y, rx, ry);

    if (rx > 0) and (ry > 1) then dec(rx);
    if (ry > 0) and (rx > 1) then dec(ry);
  end;
end;

procedure TJakRandrProjector.DrawSprite(sprite: TSprite);
var
  p: TPoint;
  w, h: real;
  magicRect: TRect;
begin
  p := Project(GetRotatedPoint(sprite.Centre));

  w := sprite.Width * m_canvas.Width / 4000.0;
  h := sprite.Height * m_canvas.Height / 3000.0;

  magicRect.Left := p.X - round(w / 2.0);
  magicRect.Right := p.X + round(w / 2.0);
  magicRect.Top := p.Y - round(h / 2.0);
  magicRect.Bottom := p.Y + round(h / 2.0);

  m_canvas.StretchDraw(magicRect, sprite.Graphic);
end;

procedure TJakRandrProjector.Clear;
begin
  m_canvas.Brush.Color := m_bgColor;
  m_canvas.Brush.Style := bsSolid;
  m_canvas.FillRect(0, 0, m_canvas.Width, m_canvas.Height);
end;

function TJakRandrProjector.GetViewportLocation: TPoint3D;
var
  ret: TPoint3D;
begin
  ret := Point3DFromCoords(0.0, 0.0, CAMERA_DISTANCE);
  GetViewportLocation := ret;
end;

(* sort functions *)
(* entry function *)

function TJakRandrProjector.InOrder(e1, e2: IEntity3D): boolean;
begin
  if      (e1 is TSphere)  and (e2 is TSphere)  then
    InOrder := InOrderSpheres
          (e1 as TSphere,       e2 as TSphere )
  else if (e1 is TPolygon) and (e2 is TPolygon) then
    InOrder := InOrderPolygons
          (e1 as TPolygon,      e2 as TPolygon)
  else if (e1 is TSphere)  and (e2 is TPolygon) then
    InOrder := InOrderSpherePolygon
          (e1 as TSphere,       e2 as TPolygon)
  else if (e1 is TPolygon) and (e2 is TSphere)  then
    InOrder := InOrderPolygonSphere
          (e1 as TPolygon,      e2 as TSphere )
  else if (e1 is TSprite)  and (e2 is TSprite)  then
    InOrder := InOrderSprites
          (e1 as TSprite,       e2 as TSprite )
  else if (e1 is TSprite)  and (e2 is TPolygon) then
    InOrder := InOrderSpritePolygon
          (e1 as TSprite,       e2 as TPolygon)
  else if (e1 is TPolygon) and (e2 is TSprite)  then
    InOrder := InOrderPolygonSprite
          (e1 as TPolygon,      e2 as TSprite )
  else if (e1 is TSprite)  and (e2 is TSphere)  then
    InOrder := InOrderSpriteSphere
          (e1 as TSprite,       e2 as TSphere )
  else if (e1 is TSphere)  and (e2 is TSprite)  then
    InOrder := InOrderSphereSprite
          (e1 as TSphere,       e2 as TSprite )
  else if (e1 is TLine)    and (e2 is TLine)    then
    InOrder := InOrderLines
          (e1 as TLine,         e2 as TLine)
  else if (e1 is TLine)    and (e2 is TPolygon)    then
    InOrder := InOrderLinePolygon
          (e1 as TLine,         e2 as TPolygon)
  else if (e1 is TPolygon)    and (e2 is TLine)    then
    InOrder := InOrderPolygonLine
          (e1 as TPolygon,         e2 as TLine)
  else if (e1 is TLine)    and (e2 is TSphere)    then
    InOrder := InOrderLineSphere
          (e1 as TLine,         e2 as TSphere)
  else if (e1 is TSphere)    and (e2 is TLine)    then
    InOrder := InOrderSphereLine
          (e1 as TSphere,         e2 as TLine)
  else if (e1 is TLine)    and (e2 is TSprite)    then
    InOrder := InOrderLineSprite
          (e1 as TLine,         e2 as TSprite)
  else if (e1 is TSprite)    and (e2 is TLine)    then
    InOrder := InOrderSpriteLine
          (e1 as TSprite,         e2 as TLine)
  else Raise Exception.Create('Not implemented for given object types');
end;

(* implementations *)

(* special case for spheres *)
(* sphere vs. sphere:
     if distance between centre is greater than the sum of radii, return
       c1 > c2
     else if s1 fully contians s2, return true
     else if s2 fully contains s1, return false
     else return c1 > c2 *)
function TJakRandrProjector.InOrderSpheres(s1, s2: TSphere): boolean;
var
  d: real;
begin
  d := Distance(s1.Centre.p, s2.Centre.p);

  if d > (s1.Radius + s2.Radius) then
    InOrderSpheres := s1.Centre.p.z > s2.Centre.p.z
  else if s1.Radius > s2.Radius then begin
    if d <= s1.Radius - s2.Radius then
      InOrderSpheres := true
    else
      InOrderSpheres := s1.Centre.p.z > s2.Centre.p.z;
  end else begin
    if d <= s2.Radius - s1.Radius then
      InOrderSpheres := false
    else
      InOrderSpheres := s1.Centre.p.z > s2.Centre.p.z; (* check this *)
  end;
end;

(* sphere vs. polygon:
   if c.z > anyVertexOfPoly.z return true
   else if c.z < anyVertexOfPoly.z return false
   else if s fully contains poly return true
   else if s doesn't contain poly at all then or partially contains poly
     return centre is on same side as plane of poly as viewport

   poly vs. sphere -- not( call( sphere vs. poly ) );
*)
function TJakRandrProjector.InOrderSpherePolygon(s: TSphere; p: TPolygon): boolean;
var
  containment: TPlanarity; (* plOn => Polygon is inside Sphere *)
  i: integer;
  side, viewportSide: TPlanarity;
  viewport: TPoint3D;
label
  last_test; (* in case any test fails, goto fallback test *)
begin
  (*
     if very in front, return true
     if very behind, return false
     if fully contained, return true
     return Centre of sphere on the same side of plane as viewport
  *)
  (* decide which test we're running *)
  if p.Nodes[0].p.z <= (s.Centre.p.z - s.Radius) then
    containment := plBehind (* test behind-ness *)
  else if p.Nodes[0].p.z >= (s.Centre.p.z - s.Radius) then
    containment := plFront (* test infront-ness *)
  else begin
    if Distance(s.Centre.p, p.Nodes[0].p) > s.Radius then
      goto last_test (* catch-all test *)
    else
      containment := plOn; (* test containiness *)
  end;

  if containment = plOn then begin
    for i := 1 to p.NbNodes - 1 do
      if Distance(s.Centre.p, p.Nodes[i].p) > s.Radius then
        goto last_test; (* polygon all over the place, go to catch all test *)
    InOrderSpherePolygon := true; (* polygon much more behind than sphere *)
    exit;
     end else if containment = plFront then begin
    for i := 1 to p.NbNodes - 1 do
      if p.Nodes[i].p.z < s.Centre.p.z then
        goto last_test; (* polygon all over the place, go to catch all test *)
    InOrderSpherePolygon := false; (* polygon much closer than sphere *)
    exit;
  end else if containment = plBehind then begin
    for i := 1 to p.NbNodes - 1 do
      if p.Nodes[i].p.z > s.Centre.p.z then
        goto last_test; (* polygon all over the place, go to catch all test *)
    InOrderSpherePolygon := true; (* polygon is fully within the sphere *)
    exit;
  end;

  last_test:
  viewport := GetViewportLocation;
  viewportSide := SideOfPlane(p, viewport);
  if viewportSide = plOn then begin
    InOrderSpherePolygon := s.Centre.p.z < Centroid(p).z;
  end;

  side := SideOfPlane(p, s.Centre.p);
  if (side = viewportSide) then
    InOrderSpherePolygon := true
  else
    InOrderSpherePolygon := false;
end;

function TJakRandrProjector.InOrderPolygonSphere(p: TPolygon; s: TSphere): boolean;
begin
  InOrderPolygonSphere := not InOrderSpherePolygon(s, p);
end;

(* try these:
        when disjoint by x or by y, return true without further test
        try to swap return values for tests 4,5,7,8
        in test other, compare centroids
*)

(* implementation of Newell's algorithm *)
(* implement for triangles and quads *)
(* for quads we assume all nodes are in the same plane *)
(*    assert that in constructor *)
(*      by making sure two normals are very, very close together *)
function TJakRandrProjector.InOrderPolygons(t1, t2: TPolygon): boolean;
var
  i: integer;
  passes: Boolean;
  min, max: real;
  viewportSide, side: TPlanarity;
  projection1, projection2: array of TPoint;
label
  test2, test3, test4, test5, test6, test7, test8, testOther;
begin
  (* test 1 - no overlap on Z *)
  (*   get minZ of t2, get maxZ of t2 *)
  min := t2.Nodes[0].p.z;
  max := t2.Nodes[0].p.z;
  for i := 1 to t2.NbNodes - 1 do
    if t2.Nodes[i].p.z > max then
      max := t2.Nodes[i].p.z
    else if t2.Nodes[i].p.z < min then
      min := t2.Nodes[i].p.z;

  for i := 0 to t1.NbNodes - 1 do
    if (t1.Nodes[i].p.z >= min) and (t1.Nodes[i].p.z <= max) then
      goto test2;

  (*$IFDEF DEBUG_NEWELL*)
  writeln('test1');
  (*$ENDIF*)
  (* test passes *)
  InOrderPolygons := (Centroid(t1).z < Centroid(t2).z);
  exit;

  test2:
  (* test 2 - no overlap on X *)
  (*   idem *)
  min := t2.Nodes[0].p.x;
  max := t2.Nodes[0].p.x;
  for i := 1 to t2.NbNodes - 1 do
    if t2.Nodes[i].p.x < min then
      min := t2.Nodes[i].p.x
    else if t2.Nodes[i].p.x > max then
      max := t2.Nodes[i].p.x;

  for i := 0 to t1.NbNodes - 1 do
    if (t1.Nodes[i].p.x > min) and (t1.Nodes[i].p.x < max) then
      goto test3;

  (*$IFDEF DEBUG_NEWELL*)
  writeln('test2');
  (*$ENDIF*)
  InOrderPolygons := true;
  exit;

  test3:
  (* test 3 - no overlap on Y *)
  (*   idem *)

  min := t2.Nodes[0].p.y;
  max := t2.Nodes[0].p.y;
  for i := 1 to t2.NbNodes - 1 do
    if t2.Nodes[i].p.y < min then
      min := t2.Nodes[i].p.y
    else if t2.Nodes[i].p.y > max then
      max := t2.Nodes[i].p.y;

  for i := 0 to t1.NbNodes - 1 do
    if (t1.Nodes[i].p.y > min) and (t1.Nodes[i].p.y < max) then
      goto test4;

  (*$IFDEF DEBUG_NEWELL*)
  writeln('test3');
  (*$ENDIF*)
  InOrderPolygons := true;
  exit;

  test4:
  (* test 4 - all vertices of t1 are on the opposite side of t2 versus viewpoint *)
  (*   idem but with SideOfPlane instead of minZ/maxZ *)
  (*   if true, return true, else continue with tests *)
  viewportSide := SideOfPlane(t2,
        GetViewportLocation);
  if viewportSide = plOn then
    goto test5;

  for i := 0 to t1.NbNodes - 1 do begin
    side := SideOfPlane(t2, t1.Nodes[i].p);
    if side = viewportSide then
      goto test5;
  end;

  (*$IFDEF DEBUG_NEWELL*)
  writeln('test4');
  (*$ENDIF*)
  InOrderPolygons := true;
  exit;

  test5:
  (* test 5 - all vertices of t2 are on the same side of t1 versus viewpoint *)
  (*   idem but with SideOfPlane *)
  viewportSide := SideOfPlane(t1,
        GetViewportLocation);
  if viewportSide = plOn then
    goto test6;

  for i := 0 to t2.NbNodes - 1 do begin
    side := SideOfPlane(t1, t2.Nodes[i].p);
    if side = plOn then continue;
    if side <> viewportSide then
      goto test6;
  end;

  (*$IFDEF DEBUG_NEWELL*)
  writeln('test5');
  (*$ENDIF*)
  InOrderPolygons := true;
  exit;

  test6:
  (* test 6 - projections overlap *)
  (*   check with point in triangle *)
  SetLength(projection1, t1.NbNodes);
  SetLength(projection2, t2.NbNodes);
  for i := 0 to t1.NbNodes - 1 do
    projection1[i] := Project(t1.Nodes[i].p);
  for i := 0 to t2.NbNodes - 1 do
    projection2[i] := Project(t2.Nodes[i].p);

  passes := true;
  for i := 0 to t1.NbNodes - 1 do
    if IsPointInPolygon(projection1[i], projection2, 0, t2.NbNodes) then begin
      passes := false;
      break;
    end;

  SetLength(projection1, 0);
  SetLength(projection2, 0);

  if passes then begin
    (*$IFDEF DEBUG_NEWELL*)
    writeln('test6');
    (*$ENDIF*)
    InOrderPolygons := true;
    exit;
  end;

  test7:
  (* test 7 - all vertices of t2 are on opposite side of t1 (false if passes)*)
  (* same as 4 but swap t2 and t1 *)
  viewportSide := SideOfPlane(t1,
        GetViewportLocation);
  if viewportSide = plOn then
    goto test8;

  for i := 0 to t2.NbNodes - 1 do begin
    side := SideOfPlane(t1, t2.Nodes[i].p);
    if side = viewportSide then begin
      goto test8;
    end;
  end;

  (*$IFDEF DEBUG_NEWELL*)
  writeln('test7');
  (*$ENDIF*)
  InOrderPolygons := false;
  exit;

  test8:
  (* test 8 - all vertices of t1 are on same side of t2 (false if passes) *)
  (* same as 5 but swap t2 and t1 *)
  viewportSide := SideOfPlane(t2,
        GetViewportLocation);
  if viewportSide = plOn then
    goto testOther;

  for i := 0 to t1.NbNodes - 1 do begin
    side := SideOfPlane(t2, t1.Nodes[i].p);
    if side = plOn then continue;
    if side <> viewportSide then
      goto testOther;
  end;

  (*$IFDEF DEBUG_NEWELL*)
  writeln('test9');
  (*$ENDIF*)
  InOrderPolygons := false;
  exit;

  testOther:
  (*$IFDEF DEBUG_NEWELL*)
  writeln('test centroid');
  (*$ENDIF*)
  InOrderPolygons := (Centroid(t1).z < Centroid(t2).z);
  exit;
  (* else ... *)
  writeln('polygon splitting not supported, defaulting to true and hoping for the best');
  InOrderPolygons := true;
end;

function TJakRandrProjector.InOrderSprites(s1, s2: TSprite): boolean;
begin
  InOrderSprites := s1.Centre.p.z > s2.Centre.p.z;
end;

function TJakRandrProjector.InOrderSpriteSphere(sprite: TSprite; s: TSphere): boolean;
begin
  InOrderSpriteSphere := sprite.Centre.p.z > s.Centre.p.z;
end;

function TJakRandrProjector.InOrderSphereSprite(s: TSphere; sprite: TSprite): boolean;
begin
  InOrderSphereSprite := s.Centre.p.z > sprite.Centre.p.z;
end;

(* true if viewport and sprite.centre are on the same side of p's plane
   false otherwise *)
function TJakRandrProjector.InOrderSpritePolygon(sprite: TSprite; p: TPolygon): boolean;
var
  viewport: TPoint3D;
  sideOfSprite: TPlanarity;
begin
  viewport := GetViewportLocation;

  sideOfSprite := SideOfPlane(p, sprite.Centre.p);

  if (sideOfSprite = plOn) or (sideOfSprite = SideOfPlane(p, viewport)) then
    InOrderSpritePolygon := true
  else
    InOrderSpritePolygon := false;
end;

function TJakRandrProjector.InOrderPolygonSprite(p: TPolygon; sprite: TSprite): boolean;
begin
  InOrderPolygonSprite := not InOrderSpritePolygon(sprite, p);
end;

function TJakRandrProjector.InOrderLines(l1, l2: TLine): boolean;
var
  cz1, cz2: real;
begin
  cz1 := (GetRotatedPoint(l1.Nodes[0]).z + GetRotatedPoint(l1.Nodes[1]).z) / 2.0;
  cz2 := (GetRotatedPoint(l2.Nodes[0]).z + GetRotatedPoint(l2.Nodes[1]).z) / 2.0;
  InOrderLines := cz1 <= cz2;
end;

function TJakRandrProjector.InOrderLinePolygon(l: TLine; p: TPolygon): boolean;
var
  cl: TPoint3D;
  viewportSide, side1, side2: TPlanarity;
label
  lastTest;
begin
  viewportSide := SideOfPlane(p, GetViewportLocation);
  if viewportSide = plOn then
    goto lastTest;

  side1 := SideOfPlane(p, GetRotatedPoint(l.Nodes[0]));
  side2 := SideOfPlane(p, GetRotatedPoint(l.Nodes[1]));
  if (side1 = plOn) then
    if (side2 <> viewportSide) then
      InOrderLinePolygon := true
    else
      InOrderLinePolygon := false
  else if (side2 = plOn) or (side1 = side2) then
    if (side1 <> viewportSide) then
      InOrderLinePolygon := true
    else
      InOrderLinePolygon := false;

lastTest:
  cl := Point3DFromCoords(
        (GetRotatedPoint(l.Nodes[0]).x + GetRotatedPoint(l.Nodes[1]).x) / 2.0,
        (GetRotatedPoint(l.Nodes[0]).y + GetRotatedPoint(l.Nodes[1]).y) / 2.0,
        (GetRotatedPoint(l.Nodes[0]).z + GetRotatedPoint(l.Nodes[1]).z) / 2.0);

  InOrderLinePolygon := cl.z < Centroid(p).z;
end;

function TJakRandrProjector.InOrderPolygonLine(p: TPolygon; l: TLine): boolean;
begin
  InOrderPolygonLine := not InOrderLinePolygon(l, p);
end;

function TJakRandrProjector.InOrderLineSprite(l: TLine; s: TSprite): boolean;
var
  clz: real;
begin
  clz := ((GetRotatedPoint(l.Nodes[0]).z + GetRotatedPoint(l.Nodes[1]).z)) / 2.0;
  InOrderLineSprite := clz <= GetRotatedPoint(s.Centre).z;
end;

function TJakRandrProjector.InOrderSpriteLine(s: TSprite; l: TLine): boolean;
begin
  InOrderSpriteLine := not InOrderLineSprite(l, s);
end;

function TJakRandrProjector.InOrderLineSphere(l: TLine; s: TSphere): boolean;
var
  clz: real;
begin
  clz := ((GetRotatedPoint(l.Nodes[0]).z + GetRotatedPoint(l.Nodes[1]).z)) / 2.0;
  InOrderLineSphere := clz <= GetRotatedPoint(s.Centre).z;
end;

function TJakRandrProjector.InOrderSphereLine(s: TSphere; l: TLine): boolean;
begin
  InOrderSphereLine := not InOrderLineSphere(l, s);
end;

(* TLine *)

constructor TLine.Line(p1, p2: TPoint3D);
begin
  m_nodes[0] := RealPoint3DFromPoint(p1);
  m_nodes[1] := RealPoint3DFromPoint(p2);
  m_contourColour := clWhite;
end;

procedure TLine.Translate(dp: TPoint3D);
var
  i: integer;
begin
  for i := 0 to 1 do begin
    TranslateVector(m_nodes[i].p, dp);
    TranslateVector(m_nodes[i].rotationCentre, dp);
  end;
end;

procedure TLine.Rotate(centre: TPoint3D; rotx, roty, rotz: real);
var
  i: integer;
begin
  for i := 0 to 1 do begin
    ApplyRotationToPoint(m_nodes[i], RealPoint3DFromPoint(centre), rotx, roty, rotz);
  end;
end;

procedure TLine.Dump;
var
  //i: integer;
  p: TPoint3D;
begin
  (*
  write('Line: ', 2, ' ');
  for i := 0 to 1 do begin
    p := GetRotatedPoint(m_nodes[i]);
    write('(', p.x, ',', p.y, ',', p.z, ') ');
  end;
  writeln;*)
  p := Point3DFromCoords(
        (GetRotatedPoint(m_nodes[0]).x + GetRotatedPoint(m_nodes[1]).x) / 2,
        (GetRotatedPoint(m_nodes[0]).y + GetRotatedPoint(m_nodes[1]).y) / 2,
        (GetRotatedPoint(m_nodes[0]).z + GetRotatedPoint(m_nodes[1]).z) / 2);
  writeln('Line: ', p.x, ' ', p.y, ' ', p.z);
end;

function TLine.GetFacingCamera(O: TPoint3D; rx, ry, rz: real): IEntity3D;
var
  p1, p2: TPoint3D;
  ret: IEntity3D;
begin
  p1 := GetRotatedPoint(m_nodes[0]);
  RotateNode(p1, O, -rx, -ry, -rz);
  SubstractVector(p1, O);
  p2 := GetRotatedPoint(m_nodes[1]);
  RotateNode(p2, O, -rx, -ry, -rz);
  SubstractVector(p2, O);

  ret := TLine.Line(p1, p2);
  (ret as TLine).m_contourColour := m_contourColour;

  GetFacingCamera := ret;
end;

function TLine.GetNbNodes: integer;
begin
  GetNbNodes := 2;
end;

function TLine.GetNode(i: integer): TRealPoint3D;
begin
  if (i >= 2) or (i < 0) then
    Raise Exception.Create('out of range!');
  GetNode := m_nodes[i];
end;

(* TSphere *)

constructor TSphere.Sphere(centre: TPoint3D; radius: real);
begin
  m_c := RealPoint3DFromPoint(centre);
  m_r := radius;
  m_contourColour := clBlack;
  m_fillColour := clWhite;
end;

procedure TSphere.Translate(dp: TPoint3D);
begin
  incr(m_c.p.x, dp.x);
  incr(m_c.p.y, dp.y);
  incr(m_c.p.z, dp.z);
  incr(m_c.rotationCentre.x, dp.x);
  incr(m_c.rotationCentre.y, dp.y);
  incr(m_c.rotationCentre.z, dp.z);
end;

procedure TSphere.Rotate(centre: TPoint3D; rotx, roty, rotz: real);
begin
  ApplyRotationToPoint(m_c, RealPoint3DFromPoint(centre), rotx, roty, rotz);
end;

procedure TSphere.Dump;
var
  p: TPoint3D;
begin
  p := GetRotatedPoint(Centre);
  writeln('Sphere: (', p.x, ',', p.y, ',', p.z, ',) ', Radius);
end;

function TSphere.GetFacingCamera(O: TPoint3D; rx, ry, rz: real): IEntity3D;
var
  c: TPoint3D;
  ret: IEntity3D;
begin
  (* TODO also, offset point to O *)
  c := GetRotatedPoint(m_c);
  SubstractVector(c, O);
  RotateNode(c, Point3DFromCoords(0, 0, 0), -rx, -ry, -rz);

  ret := TSphere.Sphere(c, m_r);
  (ret as TSphere).m_contourColour := m_contourColour;
  (ret as TSphere).m_fillColour := m_fillColour;

  GetFacingCamera := ret;
end;

(* TPolygon *)

constructor TPolygon.Triangle(p1, p2, p3: TPoint3D);
begin
  m_n := 3;
  SetLength(m_nodes, m_n);
  m_nodes[0] := RealPoint3DFromPoint(p1);
  m_nodes[1] := RealPoint3DFromPoint(p2);
  m_nodes[2] := RealPoint3DFromPoint(p3);
  m_contourColour := clBlack;
  m_fillColour := clWhite;
end;

constructor TPolygon.Quad(p1, p2, p3, p4: TPoint3D);
{$IFDEF CHECK_PLANARITY}
var
  n1, n2: TPoint3D;
  tr1, tr2: TPolygon;
{$ENDIF}
begin
  m_n := 4;
  SetLength(m_nodes, m_n);
  m_nodes[0] := RealPoint3DFromPoint(p1);
  m_nodes[1] := RealPoint3DFromPoint(p2);
  m_nodes[2] := RealPoint3DFromPoint(p3);
  m_nodes[3] := RealPoint3DFromPoint(p4);

  m_contourColour := clBlack;
  m_fillColour := clWhite;

{$IFDEF CHECK_PLANARITY}
  tr1 := TPolygon.Triangle(p1, p2, p3);
  tr2 := TPolygon.Triangle(p1, p2, p4);
  n1 := NormalForPlane(tr1);
  n2 := NormalForPlane(tr2);
  tr1.Free;
  tr2.Free;

  if not((abs(n1.x - n2.x) < 0.00001)
       and (abs(n1.y - n2.y) < 0.00001)
       and (abs(n1.z - n2.z) < 0.00001))
       then
    Raise Exception.Create('TPolygon.Quad: nodes are not in the same plane! Not supported!');
{$ENDIF}
end;

destructor TPolygon.Destroy;
begin
  SetLength(m_nodes, 0);
end;

function TPolygon.GetNode(i: integer): TRealPoint3D;
begin
  if (i >= m_n) or (i < 0) then
    Raise Exception.Create('out of range!');
  GetNode := m_nodes[i];
end;

procedure TPolygon.Translate(dp: TPoint3D);
var
  i: integer;
begin
  for i := 0 to m_n-1 do begin
    incr(m_nodes[i].p.x, dp.x);
    incr(m_nodes[i].p.y, dp.y);
    incr(m_nodes[i].p.z, dp.z);
    incr(m_nodes[i].rotationCentre.x, dp.x);
    incr(m_nodes[i].rotationCentre.y, dp.y);
    incr(m_nodes[i].rotationCentre.z, dp.z);
  end;
end;

procedure TPolygon.Rotate(centre: TPoint3D; rotx, roty, rotz: real);
var
  i: integer;
begin
  for i := 0 to m_n - 1 do
    ApplyRotationToPoint(m_nodes[i], RealPoint3DFromPoint(centre), rotx, roty, rotz);
end;

procedure TPolygon.Dump;
(*var
  i: integer;
  p: TPoint3D;*)
begin
  writeln('Polygon: ', Centroid(Self).x, ' ', Centroid(Self).y, ' ', Centroid(Self).z);
  (*
  write('Polygon: ', m_n, ' ');
  for i := 0 to m_n - 1 do begin
    p := GetRotatedPoint(m_nodes[i]);
    write('(', p.x, ',', p.y, ',', p.z, ') ');
  end;
  writeln;
  *)
end;

function TPolygon.GetFacingCamera(O: TPoint3D; rx, ry, rz: real): IEntity3D;
var
  p1, p2, p3, p4: TPoint3D;
  ret: IEntity3D;
begin
  (* TODO also, offset points to O *)
  p1 := GetRotatedPoint(m_nodes[0]);
  SubstractVector(p1, O);
  RotateNode(p1, Point3DFromCoords(0, 0, 0), -rx, -ry, -rz);
  p2 := GetRotatedPoint(m_nodes[1]);
  SubstractVector(p2, O);
  RotateNode(p2, Point3DFromCoords(0, 0, 0), -rx, -ry, -rz);
  p3 := GetRotatedPoint(m_nodes[2]);
  SubstractVector(p3, O);
  RotateNode(p3, Point3DFromCoords(0, 0, 0), -rx, -ry, -rz);
  if m_n = 3 then
    ret := TPolygon.Triangle(p1, p2, p3)
  else begin
    p4 := GetRotatedPoint(m_nodes[3]);
    SubstractVector(p4, O);
    RotateNode(p4, Point3DFromCoords(0, 0, 0), -rx, -ry, -rz);

    ret := TPolygon.Quad(p1, p2, p3, p4);
  end;

  (ret as TPolygon).m_contourColour := m_contourColour;
  (ret as TPolygon).m_fillColour := m_fillColour;

  GetFacingCamera := ret;
end;

(* TSprite *)

constructor TSprite.Sprite(centre: TPoint3D; graphic: TBitmap;
  width, height: real);
begin
  m_c := RealPoint3DFromPoint(centre);
  m_g := graphic;
  m_width := width;
  m_height := height;
end;

destructor TSprite.Destroy;
begin
  m_g.Free;
end;

procedure TSprite.Translate(dp: TPoint3D);
begin
  incr(m_c.p.x, dp.x);
  incr(m_c.p.y, dp.y);
  incr(m_c.p.z, dp.z);
  incr(m_c.rotationCentre.x, dp.x);
  incr(m_c.rotationCentre.y, dp.y);
  incr(m_c.rotationCentre.z, dp.z);
end;

procedure TSprite.Rotate(centre: TPoint3D; rotx, roty, rotz: real);
begin
  ApplyRotationToPoint(m_c, RealPoint3DFromPoint(centre), rotx, roty, rotz);
end;

procedure TSprite.Dump;
var
  p: TPoint3D;
begin
  p := GetRotatedPoint(m_c);
  writeln('Sprite: (', p.x, ',', p.y, ',', p.z, ')');
end;

function TSprite.GetFacingCamera(O: TPoint3D; rx, ry, rz: real): IEntity3D;
var
  c: TPoint3D;
begin
  (* TODO also offset point to O *)
  c := GetRotatedPoint(m_c);
  SubstractVector(c, O);
  RotateNode(c, Point3DFromCoords(0, 0, 0), -rx, -ry, -rz);

  GetFacingCamera := TSprite.Sprite(c, m_g, m_width, m_height);
end;


(* globals *)

procedure TranslateVector(var p: TPoint3D; dp: TPoint3D);
begin
  incr(p.x, dp.x);
  incr(p.y, dp.y);
  incr(p.z, dp.z);
end;

procedure SubstractVector(var p: TPoint3D; dp: TPoint3D);
begin
  decr(p.x, dp.x);
  decr(p.y, dp.y);
  decr(p.z, dp.z);
end;

function Point3DFromCoords(x, y, z: real): TPoint3D;
var
  ret: TPoint3D;
begin
  ret.x := x;
  ret.y := y;
  ret.z := z;
  Point3DFromCoords := ret;
end;

function RealPoint3DFromCoords(x, y, z: real): TRealPoint3D;
var
  ret: TRealPoint3D;
begin
  ret.p := Point3DFromCoords(x, y, z);
  ret.rotationCentre := Point3DFromCoords(0.0, 0.0, 0.0);
  ret.rx := 0.0;
  ret.ry := 0.0;
  ret.rz := 0.0;
  RealPoint3DFromCoords := ret;
end;

function RealPoint3DFromPoint(p: TPoint3D): TRealPoint3D;
var
  ret: TRealPoint3D;
begin
  ret.p := p;
  ret.rotationCentre := Point3DFromCoords(0.0, 0.0, 0.0);
  ret.rx := 0.0;
  ret.ry := 0.0;
  ret.rz := 0.0;
  RealPoint3DFromPoint := ret;
end;

(* warning! careful what you do with this
   if unsure, just don't use it and apply rotations manually
*)
procedure ApplyRotationToPoint(
  var p: TRealPoint3D;
  rotCentre: TRealPoint3D;
  rx, ry, rz: real);
begin
  p.rotationCentre := GetRotatedPoint(rotCentre);
  incr(p.rx, rx);
  incr(p.ry, ry);
  incr(p.rz, rz);
end;

function GetRotatedPoint(p: TRealPoint3D): TPoint3D;
var
  ret: TPoint3D;
begin
  ret.x := p.p.x;
  ret.y := p.p.y;
  ret.z := p.p.z;
  RotateNode(ret, p.rotationCentre, p.rx, p.ry, p.rz);
  GetRotatedPoint := ret;
end;

(* utils *)

procedure incr(var whom: real; value: real);
begin
  whom := whom + value;
  if abs(whom) < 0.00001 then whom := 0.0;
end;

procedure decr(var whom: real; value: real);
begin
  whom := whom - value;
  if abs(whom) < 0.00001 then whom := 0.0;
end;

(* see README.md for acknowledgements *)
function IsPointInPolygon(
  p: TPoint;
  poli: array of TPoint;
  start, count: integer): boolean;
var
  i, j: integer;
  c: boolean;
begin
  j := start + count - 1;
  c := false;
  for i := start to start + count - 1 do begin
    if ((poli[i].y > p.y) <> (poli[j].y > p.y))
        and (p.x < ((poli[j].x - poli[i].x)
                * (p.y - poli[i].y)
                / (poli[j].y - poli[i].y)
                + poli[i].x)) then
      c := not c;
    j := i;
  end;

  IsPointInPolygon := c;
end;

(* alternative implementation for SideOfPlane *)
(*
  // equation for node on a plane: N . V + d = 0
  // where N is the normal, d is a scalar offset
  N := (V2 - V0) x (V1 - V0);
  d := -N . V0;
  res := N . V + d;
  if res > 0.00001 then SideOfPlane := plFront
  else if res < 0.00001 then SideOfPlane := plBehind
  else SideOfPlane := plOn;
*)

function SideOfPlane(normal: TPoint3D; pointOnPlane, p: TPoint3D): TPlanarity;
var
  myVector: TPoint3D;
  prod: real;
begin
  NormalizeVector(normal);

  myVector := Point3DFromCoords(
        p.x - pointOnPlane.x,
        p.y - pointOnPlane.y,
        p.z - pointOnPlane.z);

  prod := DotProduct(normal, myVector);

  if prod < 0.00001 then
    SideOfPlane := plBehind
  else if prod > 0.00001 then
    SideOfPlane := plFront
  else
    SIdeOfPlane := plOn;
end;

function SideOfPlane(plane: TPolygon; p: TPoint3D): TPlanarity;
var
  normal: TPoint3D;
  myVector: TPoint3D;
  prod: real;
  rotPnt: TPoint3D;
begin
  (* origin is plane.Nodes[0] *)
  normal := NormalForPlane(plane);

  (* translate vector to origin *)
  rotPnt := GetRotatedPoint(plane.Nodes[0]);
  myVector.x := p.x - rotPnt.x;
  myVector.y := p.y - rotPnt.y;
  myVector.z := p.z - rotPnt.z;
  NormalizeVector(myVector);

  prod := DotProduct(normal, myVector);

  if prod > 0.00001 then SideOfPlane := plFront
  else if prod < -0.00001 then SideOfPlane := plBehind
  else SideOfPlane := plOn;
end;

function NormalForPlane(plane: TPolygon): TPoint3D;
var
  v1, v2: TPoint3D;
  ret: TPoint3D;
  p1, p2, p0: TPoint3D;
begin
  p0 := GetRotatedPoint(plane.Nodes[0]);
  p1 := GetRotatedPoint(plane.Nodes[1]);
  p2 := GetRotatedPoint(plane.Nodes[2]);

  v1.x := p1.x - p0.x;
  v1.y := p1.y - p0.y;
  v1.z := p1.z - p0.z;

  v2.x := p2.x - p0.x;
  v2.y := p2.y - p0.y;
  v2.z := p2.z - p0.z;

  ret := CrossProduct(v1, v2);
  NormalizeVector(ret);

  NormalForPlane := ret;
end;

function DotProduct(v1, v2: TPoint3D): real;
begin
  DotProduct := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
end;

function CrossProduct(v1, v2: TPoint3D): TPoint3D;
var
  ret: TPoint3D;
begin
  ret.x := v1.y * v2.z - v1.z * v2.y;
  ret.y := v1.z * v2.x - v1.x * v2.z;
  ret.z := v1.x * v2.y - v1.y * v2.x;
  CrossProduct := ret;
end;

procedure NormalizeVector(var v: TPoint3D);
var
  lngth: real;
begin
  (*        v  *)
  (* v_n = --- *)
  (*       |v| *)
  lngth := sqrt(sqr(v.x) + sqr(v.y) + sqr(v.z));
  v.x := v.x / lngth;
  v.y := v.y / lngth;
  v.z := v.z / lngth;
end;

function Distance(p1, p2: TPoint3D): real;
begin
  Distance := sqrt(sqr(p2.x - p1.x) + sqr(p2.y - p1.y) + sqr(p2.z - p1.z));
end;

procedure RotateNode(var node: TPoint3D; centre: TPoint3D; rx, ry, rz: real);
var
  translationVector: TPoint3D;
  x, y, z: real;
  sinX, sinY, sinZ: real;
  cosX, cosY, cosZ: real;
begin
  (* offset node into 0,0 *)
  x := node.x - centre.x;
  y := node.y - centre.y;
  z := node.z - centre.z;

  (*
        cosYcosZ   -cosXsinZ+sinXsinYcosZ   sinXsinZ+cosXsinYcosZ       x
        cosYsinZ    cosXcosZ+sinXsinYsinZ  -sinXcosZ+cosXsinYsinZ       y
          -sinY            sinXcosY                cosXcosY             z

    x' := x*cosYcosZ - y*cosXsinZ + y*sinXsinYcosZ + z*sinXsinZ + z*cosXsinYcosZ
    y' := x*cosYsinZ + y*cosXcosZ + y*sinXsinYsinZ - z*sinXcosZ + z*cosXsinYsinZ
    z' := -x*sinY    + y*sinXcosY                  + z*cosXcosY
  *)

  sinX := sin(rx);
  sinY := sin(ry);
  sinZ := sin(rz);
  cosX := cos(rx);
  cosY := cos(ry);
  cosZ := cos(rz);

  translationVector.x :=
          x * cosY * cosZ
        - y * (cosX * sinZ - sinX * sinY * cosZ)
        + z * (sinX * sinZ + cosX * sinY * cosZ);
  translationVector.y :=
          x * (cosY * sinZ)
        + y * (cosX * cosZ + sinX * sinY * sinZ)
        - z * (sinX * cosZ - cosX * sinY * sinZ);
  translationVector.z :=
        - x * sinY
        + y * sinX * cosY
        + z * cosX * cosY;


  (*     gimbal-like rotations -- do not use
  if rx <> 0.0 then begin
    y := cos(rx) * translationVector.y - sin(rx) * translationVector.z;
    z := sin(rx) * translationVector.y + cos(rx) * translationVector.z;
    translationVector.y := y;
    translationVector.z := z;
  end;

  if ry <> 0.0 then begin
    x := cos(ry) * translationVector.x + sin(ry) * translationVector.z;
    z := (-(sin(ry))) * translationVector.x + cos(ry) * translationVector.z;
    translationVector.x := x;
    translationVector.z := z;
  end;

  if rz <> 0.0 then begin
    x := cos(rz) * translationVector.x - sin(rz) * translationVector.y;
    y := sin(rz) * translationVector.x + cos(rz) * translationVector.y;
    translationVector.x := x;
    translationVector.y := y;
  end;
  *)

  (* offset node back to where it was *)
  node.x := centre.x + translationVector.x;
  node.y := centre.y + translationVector.y;
  node.z := centre.z + translationVector.z;
end;

function Centroid(p: TPolygon): TPoint3D;
var
  ret: TPoint3D;
  i: integer;
begin
  ret := GetRotatedPoint(p.Nodes[0]);
  for i := 1 to p.NbNodes - 1 do begin
    incr(ret.x, GetRotatedPoint(p.Nodes[i]).x);
    incr(ret.y, GetRotatedPoint(p.Nodes[i]).y);
    incr(ret.z, GetRotatedPoint(p.Nodes[i]).z);
  end;
  ret.x := ret.x / p.NbNodes;
  ret.y := ret.y / p.NbNodes;
  ret.z := ret.z / p.NbNodes;
  Centroid := ret;
end;

end.

{ notes on Newell's algorithm }

(*
 if any test fails, the next one needs to be performed
 if a test passes, the order is correct (p1 closer than p2)
compare two polis:
    1. overlap on Z
    2. overlap on X
    3. overlap on Y
    4. all vertices of p1 are on the opposite side of p2's plane from the viewpoint
        | p1    good  |  \ bad
        | | /         |   \
        | |/          |  / \p1
        | /p2         | /p2
         ----------    ----------
    5. all vertices of p2 are on the same side of p1's plane from the viewpoint
    6. projections overlap

    if all tests fail, swap p1 and p2 and repeat test 3 and 4
    if both tests fail, throw notImplementedException and assume p1/p2 order
  *)

(* notes on ref counting *)
(*
var
  ifaceRef: IEntity3D;
  objRef: TPolygon;
begin
  ifaceRef := TPolygon.Triangle(p1, p2, p3);
  objRef := TPolygon.Triangle(p1, p2, p3);
  objRef.Free; // not freed automatically, need to explicitly free
end. // ifaceRef is freed automatically

so here's how to do it:

in worldEntities:
  create all the 3d entities as IEntity3D, store them in TEntity3DList

problem solved

there exists TInterfaceList which does ref counting in list form
    -- surely there must be a generic based on this

*)


(* rotation *)
(*
  multiplication of 3x3 with 3x1 matrices

  | a11 a12 a13 | | b1 |   | a11*b1 + a12 * b2 + a13 * b3 |
  | a21 a22 a23 | | b2 | = | a21*b1 + a22 * b2 + a23 * b3 |
  | a31 a32 a33 | | b3 |   | a31*b1 + a32 * b2 + a33 * b3 |

  there are too many floating point multiplications if we use only
  one matrix, and most of the time we'll only be rotating around a
  single axis (e.g. spin around ry)

  OVERRULED
  I'm getting weird coordinates with individual gyro rotations, so switching to
  one rotation matrix

  for RX:
  | 1   0    0  |
  | 0  cos -sin |
  | 0  sin  cos |

  x := x;
  y := cos(rx) * y - sin(rx) * z;
  z := sin(rx) * y + cos(rx) * z;

  for RY:
  | cos 0 sin |
  |  0  1  0  |
  |-sin 0 cos |

  x := cos(ry) * x + sin(ry) * z;
  y := y;
  z := -sin(ry) * x + cos(ry) * z;

  for RZ:
  | cos -sin 0 |
  | sin  cos 0 |
  |  0    0  1 |

  x := cos(rz) * x - sin(rz) * y;
  y := sin(rz) * x + cos(rz) * x;
  z := z;
*)

