module Scad where
import qualified Csg
import Text.Printf (printf, PrintfType, PrintfArg)

data Node =  
    Union [Node]
  | Difference [Node]
  | Intersection [Node]
  | Scale (Double, Double, Double) Node
  | Rotate (Double, Double, Double) Node 
  | Translate (Double, Double, Double) Node 
  | Mirror (Double, Double, Double) Node 
  | MultMatrix [[Double]] Node 
  | Color (Int, Int, Int, Int) Node 
  | Minkowski [Node]
  | Hull Node
  | Extrude {h :: Double, center :: Bool, twist :: Double, node :: Node, fn :: Int, fa :: Double, fs :: Double} 
  | DXFExtrude {h :: Double, center :: Bool, twist :: Double, file :: String, layer :: String, fn :: Int, fa :: Double, fs :: Double} 
  | RotateExtrude {node :: Node, fn :: Int, fa :: Double, fs :: Double} 
  | DXFRotateExtrude {file :: String, layer :: String, fn :: Int, fa :: Double, fs :: Double} 
  | STLImport {file :: String} 
  | Group [Node]
-- primitives 
  | Sphere {r :: Double, fn :: Int, fa :: Double, fs :: Double}
  | Cylinder {r :: Double, r2 :: Double, h :: Double, center :: Bool, fn :: Int, fa :: Double, fs :: Double} 
  | Cube {size :: [Double], center :: Bool}
  | Polyhedron Csg.Polyhedron
-- 2D
  | Circle {r :: Double}
  | Polygon {points :: [[Double]], paths :: [[Int]]}
  | Square {size :: [Double], center :: Bool}
  | Projection Bool Node 
  deriving (Eq, Ord)

defaultFa :: Double
defaultFa  = 12
defaultFs :: Double
defaultFs  = 1

-- unit cube of size 1
uCube     :: Node
uCube      = Cube { size = [1,1,1], center=False }
-- unit cylinder of height and radius 1
uCylinder :: Node
uCylinder  = Cylinder { r = 1, r2 = 1, h = 1, center=False, fn = 0, fa = defaultFa, fs = defaultFs }
-- unit sphere of radius 1
uSphere   :: Node
uSphere    = Sphere { r = 1, fn = 0, fa = defaultFa, fs = defaultFs }
-- a cube generator: cube [w,h,d]
cube         :: [Double] -> Node
cube s        = uCube {size = s}
-- a cylinder generator
cylinder     :: Double -> Double -> Node
cylinder r h  = uCylinder {r = r, r2 = r, h = h}
-- a cylinder generator with different radii
cone         :: Double -> Double -> Double -> Node
cone r1 r2 h  = uCylinder {r = r1, r2 = r2, h = h}
-- a sphere generator
sphere       :: Double -> Node
sphere r      = uSphere {r = r}

-- internal: linear interpolation from 0.0 to 1.1 in dt steps
unit         :: Int -> [Double]
unit dt       = [0.0,1.0/(fromIntegral (dt-1))..1.0]
-- a parametric polygon defined by its outline generator function over [0..1.0] in dt steps
line         :: (Double -> [Double]) -> Int -> Node
line fgen dt  = Polygon { points = map fgen t, paths = [[0..(dt-1)]] }
                where t   = unit dt 
-- a parametric polyhedron defined by its outline generator function over [0..1.0]^2 in dt,du steps
plane           :: (Double -> Double -> [Double]) -> Int -> Int -> Node
plane fgen dt du = Polyhedron (Csg.poly 
                     (foldl (++) [] $ map (\x-> map (fgen x) u) t)
                     (foldl (++) [] [[[seq1!!i, seq2!!i, seq3!!i],[seq2!!i, seq4!!i, seq3!!i]] | i<-[0..length seq1 -1]]))
                where t = unit dt
                      u = unit du 
                      seq1 = [u * dt + t | u<-[0..dt-1], t<-[0..du-1]]
                      seq2 = tail seq1 ++ [0]
                      seq3 = drop du seq1 ++ (take du seq1) 
                      seq4 = map ((`mod`(dt*du)).(+dt)) seq2
-- a polygon defined by a bezier curve of arbitrary order and dimension (array size = #points x dimension) 
bezier       :: [[Double]] -> Int -> Node
bezier pts dt = line (bezierfn pts) dt 
bezierfn     :: [[Double]] -> (Double -> [Double])
bezierfn pts  = case compare (length pts) 2 of
                  EQ -> \t -> map (\d -> pts!!0!!d * (1-t) + pts!!1!!d * t) [0..length (pts!!0)-1]
                  GT -> \t -> zipWith (+) 
                                (map (*(1-t)) (bezierfn (init pts) t)) 
                                (map (*t)     (bezierfn (tail pts) t))
                  LT -> error "bezier function invoked with less than two points to interpolate between"
-- a polyhedron defined by a bezier plane of arbitrary order and dimension (array size = #points_u x #points_v x dimension)
bezierPlane          :: [[[Double]]] -> Int -> Int -> Node
bezierPlane pts dt du = plane (bezierPlaneFn pts) dt du
bezierPlaneFn        :: [[[Double]]] -> (Double -> Double -> [Double])
bezierPlaneFn pts     = \t -> (bezierfn (map (\row -> bezierfn row t) pts))
-- a polygon defined by a bicubic bezier curve along a series of control points (P C C P C C P...)
bicubic        :: [[Double]] -> Int -> Node
bicubic pts dt  = Polygon { points = foldl (++) [] (map (\g -> map (bezierfn g) t) groups), 
                            paths  = [[0..dt*sets-1]] }
                  where t = unit dt
                        groups = map ((take 4).(\x->drop (x*3) pts)) [0..sets-1]
                        sets = (length pts-1) `div` 3
-- a torus polyhedron of radius orad +- irad with resolution dt, du
torus                :: Double -> Double -> Int -> Int -> Node
torus orad irad dt du = plane (\u-> \v-> [orad * (sin (u*2*pi)) + irad * (sin (v*2*pi)) * (sin (u*2*pi)), 
                                          orad * (cos (u*2*pi)) + irad * (sin (v*2*pi)) * (cos (u*2*pi)), 
                                          irad * (cos (v*2*pi))]   ) dt du


-- 2D       
-- a unit circle of radius 1
uCircle  :: Node
uCircle   = Circle {r = 1}
-- a polygon template, usually to be overwritten
polygon :: Node
polygon  = Polygon {points = [[0,0],[0,1],[1,0]], paths = [[0..2]]}
-- a unit square of size 1
uSquare :: Node
uSquare   = Square {size=[1,1], center = False}
-- a square generator: square [w,h]
square  :: [Double] -> Node
square s = uSquare {size=s}
-- a circle generator
circle  :: Double -> Node
circle r = uCircle {r = r}

-- union of child nodes. Use Union [Node] to group a list of nodes, 
-- chains are simplified: (a+b)+c = (a+b+c) and (a+b)+(c+d) = (a+b+c+d)
union            :: Node -> Node -> Node
union (Union a) (Union b) = Union (a++b)
union (Union a) b = Union (a++[b])
union a b         = Union [a,b]
(/+)             :: Node -> Node -> Node
(Union a) /+ (Union b) = Union (a++b)
(Union a) /+ b    = Union (a++[b])
a /+ b            = Union [a,b]
-- difference of child nodes
-- chains are simplified (a-b)-c == (a-b-c), but (a-b)-(c-d) is preserved
diff             :: Node -> Node -> Node
diff (Difference a) b  = Difference (a++[b])
diff a b               = Difference [a,b]
(/-)             :: Node -> Node -> Node
(Difference l) /- b    = Difference (l++[b])
a /- b                 = Difference [a,b]
inter            :: Node -> Node -> Node
inter a b         = Intersection [a,b]
-- group nodes without necessarily executing a union() operation
group            :: Node -> Node -> Node
group (Group l) b = Group (l++[b])
group a b         = Group [a,b]
(/.)             :: Node -> Node -> Node
(Group l) /. b    = Group (l++[b])
a /. b            = Group [a,b]
-- minkowski sum over child nodes
mink             :: Node -> Node -> Node
mink a b          = Minkowski [a,b]
-- color child nodes
color            :: (Int, Int, Int, Int) -> Node -> Node
color (r,g,b,a) n = Color (r,g,b,a) n
-- matrix multiplication
mmult            :: [[Double]] -> Node -> Node
mmult m n         = if length m == 4 && 
                      (foldl (&&) True (map ((==4).length) m))
                    then MultMatrix m n
                    else error "mmult requires a 4x4 matrix"
-- scale child nodes
scale            :: (Double, Double, Double) -> Node -> Node
scale (x,y,z) n   = Scale (x,y,z) n
-- uniform scaling
(/*)              :: Node -> Double -> Node
n /* x             = Scale (x,x,x) n
-- scale dimensions separately
(//)             :: Node -> (Double, Double, Double) -> Node
n // (x,y,z)       = Scale (x,y,z) n
-- rotate child nodes
rot              :: (Double, Double, Double) -> Node -> Node
rot (x,y,z) n     = Rotate (x,y,z) n
-- translate child nodes by normal
trans            :: (Double, Double, Double) -> Node -> Node
trans (x,y,z) n   = Translate (x,y,z) n
-- mirror child nodes around normal
mirror           :: (Double, Double, Double) -> Node -> Node
mirror (x,y,z)  n = Mirror (x,y,z) n
-- generate hull of child nodes (OpenSCAD currently supports 2D only)
hull             :: Node -> Node
hull a            = Hull a

-- extrude 2D to 3D by height h
extrude          :: Double -> Node -> Node
extrude h n2      = Extrude {node = n2, h = h, center = False, twist = 0, fn = 0, fa = defaultFa, fs = defaultFs}
-- load DXF file and extrude linearly
dxf              :: Double -> String -> Node
dxf h file        = DXFExtrude {file = file, layer = "", h = h, center = False, twist = 0, fn = 0, fa = defaultFa, fs = defaultFs}
-- extrude by rotation
rotateExtrude    :: Node -> Node
rotateExtrude n2  = RotateExtrude {node = n2, fn = 0, fa = defaultFa, fs = defaultFs}
-- load DXF file and extrude by rotation
dxfRotate        :: String -> Node
dxfRotate file    = DXFRotateExtrude {file = file, layer = "", fn = 0, fa = defaultFa, fs = defaultFs}
-- load STL model from file 
stl              :: String -> Node
stl file          = STLImport {file = file}
-- project 3D shape onto zero plane
project :: Bool -> Node -> Node
project cut n = Projection cut n


-- internal --
showFnFaFs fn fa fs = if fn > 0 then ", $fn=" ++ (show fn) else 
                                (if fa /= defaultFa then ", $fa=" ++ (show fa) else "") 
                             ++ (if fs /= defaultFs then ", $fs=" ++ (show fs) else "")
showCenter c        = if c then ", center=true" else ""
showKids   k        = foldl (\c -> \line -> c ++ line ++ "\n") "" (map show k) 

showDouble         :: Double -> String
showDouble f        = printf "%.3f" f
showVector         :: [Double] -> String
showVector x        = "[" ++ (foldl1 (\x-> \y-> x ++ "," ++ y) (map showDouble x)) ++ "]"
showVector'        :: [[Double]] -> String
showVector' x       = "[" ++ (foldl1 (\x-> \y-> x ++ "," ++ y) (map showVector x)) ++ "]"
showLayer  l        = if l /= "" then ", layer=" ++ (show l) else ""
showTwist  t        = if t /= 0  then ", twist=" ++ (show t) else ""
showBool b          = if b then "true" else "false"

csgUnion :: Node -> Node -> Node
csgUnion (Polyhedron a) (Polyhedron b) = Polyhedron (Csg.csgUnion a b)
csgInter :: Node -> Node -> Node
csgInter (Polyhedron a) (Polyhedron b) = Polyhedron (Csg.csgInter a b)
csgDiff  :: Node -> Node -> Node
csgDiff  (Polyhedron a) (Polyhedron b) = Polyhedron (Csg.csgDiff a b)
-- simplify any given node hierarchy to a single polyhedron
render  :: Node -> Node 
render (Polyhedron p) = Polyhedron p
render (Union kids) | length kids == 1 = render $ kids!!0
                    | otherwise        = foldl1 csgUnion $ map render kids
render (Intersection kids) | length kids == 1 = render $ kids!!0
                           | otherwise        = foldl1 csgInter $ map render kids
render (Difference kids) | length kids == 1 = render $ kids!!0
                         | otherwise        = foldl1 csgDiff $ map render kids
render n = error ("render is not implemented for all nodes. " ++ (show n))

-- display any given node as scad command hierarchy
instance Show Node where
  show (Sphere r fn fa fs) = "sphere(r=" ++ (show r) 
                          ++ (showFnFaFs fn fa fs)
                          ++ ");"

  show (Cylinder r r2 h c fn fa fs) = "cylinder(h=" ++ (show h) 
                          ++ (if r2==r then 
                                    ", r=" ++ (show r) 
                              else  ", r1=" ++ (show r) ++ ", r2=" ++ (show r2))
                          ++ (showFnFaFs fn fa fs)
                          ++ (showCenter c)
                          ++ ");"

  show (Cube s c)  = "cube(" ++ (show s) 
                          ++ (showCenter c)
                          ++ ");"

  show (Polyhedron (Csg.Polyhedron p t e))    = "polyhedron(points = " ++ (showVector' p) ++ ", triangles = " ++ (show t) ++ ");"

  show (Union kids)        = "union () {\n" ++ (showKids kids) ++"}"
  show (Difference kids)   = "difference () {\n" ++ (showKids kids) ++"}"
  show (Intersection kids) = "intersection () {\n" ++ (showKids kids) ++"}"
  show (Group kids)        = "{\n" ++ (showKids kids) ++"}"
  show (Minkowski kids)    = "minkowski () {\n" ++ (showKids kids) ++"}"

  show (Scale (x,y,z) kid)     = "scale (" ++ (showVector [x, y, z]) ++ ")\n" ++ (show kid) ++ "\n"
  show (Mirror (x,y,z) kid)    = "mirror (" ++ (showVector [x, y, z]) ++ ")\n" ++ (show kid) ++ "\n"
  show (Rotate (x,y,z) kid)    = "rotate (" ++ (showVector [x, y, z]) ++ ")\n" ++ (show kid) ++ "\n"
  show (Translate (x,y,z) kid) = "translate (" ++ (showVector [x, y, z]) ++ ")\n" ++ (show kid) ++ "\n"
  show (Color (r,g,b,a) kid)   = "color (" ++ (show [r, g, b, a]) ++ ")\n" ++ (show kid) ++ "\n"
  show (Hull kid)              = "hull () " ++ (show kid) ++ "\n"

  show (MultMatrix m kid)      = "multmatrix (" ++ (show m) ++ ") {\n" ++ (show kid) ++ "\n}"
  show (Extrude h c t kid fn fa fs) = "linear_extrude (height=" ++ (show h) ++ (showTwist t)
                                   ++ (showCenter c) ++ (showFnFaFs fn fa fs) 
                                   ++ ")\n" ++ (show kid) ++ "\n"
  show (RotateExtrude kid fn fa fs) = "rotate_extrude (" 
                                   ++ (showFnFaFs fn fa fs) 
                                   ++ ")\n" ++ (show kid) ++ "\n"
  show (DXFExtrude h c t file layer fn fa fs) = "linear_extrude (height=" ++ (show h) ++ (showTwist t)
                                   ++ (showCenter c) ++ (showFnFaFs fn fa fs) 
                                   ++ ", file=" ++ (show file) ++ (showLayer layer) ++");"
  show (DXFRotateExtrude file layer fn fa fs) = "rotate_extrude (" 
                                   ++ (showFnFaFs fn fa fs) 
                                   ++ ", file=" ++ (show file) ++ (showLayer layer) ++");"
  show (STLImport file) = "stl_import (file=" ++ (show file) ++ ");" 
  show (Circle r) = "circle(r=" ++ (show r) ++ ");"
  show (Square s c) = "square(" ++ (show s) ++ (showCenter c) ++ ");"
  show (Polygon p l) = "polygon(points = " ++ (showVector' p)  ++ ", paths = " ++ (show l) ++ ");"

  show (Projection c kid) = "projection (cut = "++(showBool c)++")\n" ++ (show kid)

