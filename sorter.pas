{ concept }

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

there exists TInterfaceList

*)

unit GfxUtils2;
  
interface

(* Typedefs *)
type
  IEntity3D = interface(IInterface)
    procedure Translate(dp: TPoint3D);
    procedure Rotate(centre: TPoint3D; rotx, roty, rotz: real);
  end;
  
  TSphere = class(TInterfacedObject, IEntity3D)
    constructor Sphere(centre: TPoint3D; radius: real);
    property Centre: TPoint3D read m_c;
    property Radius: TPoint3D read m_r;
    property ContourColour: TColor read m_contourColour write m_contourColour;
    property FillColour: TColor read m_contourColour write m_contourColour;
    (* implementation of IEntity3D *)
    procedure Translate(dp: TPoint3D);
    procedure Rotate(centre: TPoint3D; rotx, roty, rotz: real);
  private
    m_c: TPoint3D;
    m_r: real;
    m_contourColour: TColor;
    m_fillColour: TColor;
  end;
  
  TPolygon = class(TInterfacedObject, IEntity3D)
    constructor Triangle(p1, p2, p3: TPoint3D);
    constructor Quad(p1, p2, p3, p4: TPoint3D);
    destructor Destroy; override;
    property Nodes: array of TPoint3D read m_nodes;
    property NbNodes: Integer read m_n;
    property ContourColour: TColor read m_contourColour write m_contourColour;
    property FillColour: TColor read m_contourColour write m_contourColour;
    (* implementation of IEntity3D *)
    procedure Translate(dp: TPoint3D);
    procedure Rotate(centre: TPoint3D; rotx, roty, rotz: real);
  private
    m_n: Integer;
    m_nodes: array of TPoint3D;
    m_contourColour: TColor;
    m_fillColour: TColor;
  end;

  TSprite = class(TInterfacedObject, IEntity3D)
    constructor Sprite(centre: TPoint3D; graphic: TBitmap);
    destructor Destroy; override;
    property Centre: TPoint3D read m_c;
    property Graphic: TBitmap read m_g;
    (* implementation of IEntity3D *)
    procedure Translate(dp: TPoint3D);
    procedure Rotate(centre: TPoint3D; rotx, roty, rotz: real);
  private
    m_c: TPoint3D;
    m_g: TBitmap;
  end;

  TPlanarity = ( plOn, plFront, plBehind ); (* points ON the plane are ignored *)
  (* viewpoint is somewhere @ plane.m_p1.x, plane.m_p1.y, 100000 *)
end;

(* entry function for determining if e1 is closer to the viewport than e2
   true means e1 is closer than e2, hence e2 should be drawn first *)
function InOrder(e1, e2: IEntity3D): boolean;
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

(* check if p is in front, behind of or on the plane *)
function SideOfPlane(plane: TPolygon; p: TPoint3D): TPlanarity;
(* returns the normalized vector of the plane's normal *)
function NormalForPlane(plane: TPolygon): TPoint3D;
function DotProduct(v1, v2: TPoint3D): real;
function CrossProduct(v1, v2: TPoint3D): TPoint3D;
procedure NormalizeVector(var v: TPoint3D);
function Distance(p1, p2: TPoint3D): real;
procedure RotateNode(var node: TPoint3D; centre: TPoint3D; rx, ry, rz: real);

implementation

(* TSphere *)

constructor TSphere.Sphere(centre: TPoint3D; radius: real);
begin
  m_c := centre;
  m_r := radius;
  m_contourColour := clBlack;
  m_fillColour := clWhite;
end;

procedure TSphere.Translate(dp: TPoint3D);
begin
  inc(m_c.x, dp.x);
  inc(m_c.y, dp.y);
  inc(m_c.z, dp.z);
end;

procedure TSphere.Rotate(centre: TPoint3D; rotx, roty, rotz: real);
begin
  RotateNode(m_c, centre, rotx, roty, rotz);
end;

(* TPolygon *)

constructor TPolygon.Triangle(p1, p2, p3: TPoint3D);
var
  i: integer;
begin
  m_n := 3;
  SetLength(m_nodes, m_n);
  m_nodes[1] := p1;
  m_nodes[2] := p2;
  m_nodes[3] := p3;
  m_contourColour := clBlack;
  m_fillColour := clWhite;
end;

constructor TPolygon.Quad(p1, p2, p3, p4: TPoint3D);
var
  i: integer;
{$IFDEF CHECK_PLANARITY}
  {$IF CHECK_PLANARITY > 0}
  n1, n2: TPoint3D;
  tr1, tr2: TPolygon;
  {$ENDIF}
{$ENDIF}
begin
  m_n := 4;
  SetLength(m_nodes, m_n);
  m_nodes[1] := p1;
  m_nodes[2] := p2;
  m_nodes[3] := p3;
  m_nodes[4] := p4;

  m_contourColour := clBlack;
  m_fillColour := clWhite;

{$IFDEF CHECK_PLANARITY}
  {$IF CHECK_PLANARITY > 0}
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
{$ENDIF}
end;

destructor TPolygon.Destroy;
begin
  SetLength(m_nodes, 0);
end;

procedure TPolygon.Translate(dp: TPoint3D);
var
  i: integer;
begin
  for i := 1 to m_n do begin
    inc(m_nodes[i].x, dp.x);
    inc(m_nodes[i].y, dp.y);
    inc(m_nodes[i].z, dp.z);
  end;
end;

procedure TPolygon.Rotate(centre: TPoint3D; rotx, roty, rotz: real);
var
  i: integer;
begin
  for i := 1 to m_n do
    RotateNode(m_nodes[i], centre, rotx, roty, rotz);
end;

(* TSprite *)

constructor TSprite.Sprite(centre: TPoint3D; graphic: TBitmap);
begin
  m_c := centre;
  m_g := graphic;
end;

destructor TSprite.Destroy;
begin
  m_g.Free;
end;

procedure TSprite.Translate(dp: TPoint3D);
begin
  inc(m_c.x, dp.x);
  inc(m_c.y, dp.y);
  inc(m_c.z, dp.z);
end;

procedure TSprite.Rotate(centre: TPoint3D; rotx, roty, rotz: real);
begin
  RotatePoint(m_c, centre, rotx, roty, rotz);
end;

(* global sort functions *)
(* entry function *)

function InOrder(e1, e2: IEntity3D): boolean;
begin
  if      (e1 is TSphere)  and (e2 is TSphere)  then
    InOrder := InOrderSpheres
          (e1 as TSphere,       e2 as TSphere )
  else if (e1 is TPolygon) and (e2 is TPolygon) then
    InOrder := InOrderPolygon
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
function InOrderSpheres(s1, s2: TSphere): boolean;
var
  d: real;
begin
  d := Distance(s1.Centre, s2.Centre;

  if d > (s1.Radius + s2.Radius) then
    InOrderSpheres := s1.Centre.z > s2.Centre.z
  else if s1.Radius > s2.Radius then begin
    if d <= s1.Radius - s2.Radius then
      InOrderSpheres := true
    else
      InOrderSpheres := s1.Centre.z > s2.Centre.z;
  end else begin
    if d <= s2.Radius - s1.Radius then
      InOrderSpheres := false
    else
      InOrderSpheres := s1.Centre.z > s2.Centre.z; (* check this *)
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
function InOrderSpherePolygon(s: TSphere; p: TPolygon): boolean;
begin
  writeln('InOrderSpherePolygon: TODO');
end;

function InOrderPolygonSphere(p: TPolygon; s: TSphere): boolean;
begin
  InOrderPolygonSphere := not InOrderSpherePolygon;
end;

(* implementation of Newell's algorithm *)
(* implement for triangles and quads *)
(* for quads we assume all nodes are in the same plane *)
(*    assert that in constructor *)
(*      by making sure two normals are very, very close together *)
function InOrderPolygons(t1, t2: TPolygon): boolean;
var
  i: integer;
  front: boolean;
begin
  (* test 1 - no overlap on Z *)
  (*   get minZ of t2, get maxZ of t2 *)
  (*   check no vertex is inside t2 *)
  (*     if p1 is bigger than maxZ return true *)
  (*     if p1 is smaller than minZ return false *)
  (*   else continue with tests *)

  (* test 2 - no overlap on X *)
  (*   idem *)

  (* test 3 - no overlap on Y *)
  (*   idem *)

  (* test 4 - all vertices of t1 are on the opposite side of t2 versus viewpoint *)
  (*   idem but with SideOfPlane instead of minZ/maxZ *)
  (*   if true, return true, else continue with tests *)

  (* test 5 - all vertices of t2 are on the same side of t1 versus viewpoint *)
  (*   idem but with SideOfPlane *)

  (* test 6 - projections overlap *)
  (*   check with point in triangle *)

  (* test 7 - all vertices of t2 are on opposite side of t1 (false if passes)*)
  (* same as 4 but swap t2 and t1 *)

  (* test 8 - all vertices of t1 are on same side of t2 (false if passes) *)
  (* same as 5 but swap t2 and t1 *)
  (* else ... *)
  writeln('polygon splitting not supported, defaulting to true and hoping for the best');
  InOrder := true;
end;

function InOrderSprites(s1, s2: TSprite): boolean;
begin
  InOrderSprites := s1.Centre.z > s2.Centre.z;
end;

function InOrderSpriteSphere(sprite: TSprite; s: TSphere): boolean;
begin
  InOrderSpriteSphere := sprite.Centre.z > s.Centre.z;
end;

function InOrderSphereSprite(s: TSphere; sprite: TSprite): boolean;
begin
  InOrderSphereSprite := s.Centre.z > sprite.Centre.z;
end;

(* true if viewport and sprite.centre are on the same side of p's plane
   false otherwise *)
function InOrderSpritePolygon(sprite: TSprite; p: TPolygon): boolean;
var
  ret: boolean;
  viewport: TPoint3D;
  sideOfSprite: TPlanarity;
begin
  (* viewport x,y can be just about anywhere on-screen, but z is infinity *)
  viewport.x := sprite.Centre.x;
  viewport.y := sprite.Centre.y;
  viewport.z := 100000.0; (* hopefully nothing is this big *)

  sideOfSprite := SideOfPlane(p, sprite.Centre);

  if (sideOfSprite = plOn) or (sideOfSprite = SideOfPlane(p, viewport)) then
    InOrderSpritePolygon := true
  else
    InOrderSpritePolygon := false;
end;

function InOrderPolygonSprite(p: TPolygon; sprite: TSprite): boolean;
begin
  InOrderPolygonSprite := not InOrderSpritePolygon(sprite, p);
end;

(* utils *)

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

function SideOfPlane(plane: TPolygon; p: TPoint3D): TPlanarity;
var
  normal: TPoint3D;
  myVector: TPoint3D;
  prod: real;
begin
  (* origin is plane.Nodes[1] *)
  normal := NormalForPlane(plane);

  (* translate vector to origin *)
  myVector.x := p.x - plane.Nodes[1].x;
  myVector.y := p.y - plane.Nodes[1].y;
  myVector.z := p.z - plane.Nodes[1].z;
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
begin
  v1.x := plane.Nodes[2].x - plane.Nodes[1].x;
  v1.y := plane.Nodes[2].y - plane.Nodes[1].y;
  v1.z := plane.Nodes[2].z - plane.Nodes[1].z;

  v2.x := plane.Nodes[3].x - plane.Nodes[1].x;
  v2.y := plane.Nodes[3].y - plane.Nodes[1].y;
  v2.z := plane.Nodes[3].z - plane.Nodes[1].z;

  ret := CrossProduct(v1, v2);
  NormalizeVector(ret);

  NormalForPlane := ret;
end;

function DotProduct(v1, v2: TPoint3D): real;
begin
  DotProduct := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
end;

function CrossProduct(v1, v2: TPoint3D): real;
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
begin
  (* offset node by centre *)
  dec(node.x, centre.x);
  dec(node.y, centre.y);
  dec(node.z, centre.z);

  if rx <> 0.0 then begin
    node.y := cos(rx) * node.y - sin(rx) * node.z;
    node.z := sin(rx) * node.y + cos(rx) * node.z;
  end;

  if ry <> 0.0 then begin
    node.x := sin(ry) * node.z + cos(ry) * node.x;
    node.z := cos(ry) * node.z - sin(ry) * node.x;
  end;

  if rz <> 0.0 then begin
    node.x := cos(rz) * node.x - sin(rz) * node.y;
    node.y := sin(rz) * node.x + cos(rz) * node.x;
  end;

  (* offset node back to where it was *)
  inc(node.x, centre.x);
  inc(node.y, centre.y);
  inc(node.z, centre.z);
end;

end.

(* rotation *)
(*
  multiplication of 3x3 with 3x1 matrices

  | a11 a12 a13 | | b1 |   | a11*b1 + a12 * b2 + a13 * b3 |
  | a21 a22 a23 | | b2 | = | a21*b1 + a22 * b2 + a23 * b3 |
  | a31 a32 a33 | | b3 |   | a31*b1 + a32 * b2 + a33 * b3 |

  there are too many floating point multiplications if we use only
  one matrix, and most of the time we'll only be rotating around a
  single axis (e.g. spin around ry)

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
