module Scad where

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
  | Hull [Node] 
--  | Render [Node] 
  | Group [Node]
-- primitives 
  | Sphere {r :: Double, fn :: Int, fa :: Double, fs :: Double}
  | Cylinder {r :: Double, r2 :: Double, h :: Double, center :: Bool, fn :: Int, fa :: Double, fs :: Double} 
  | Cube {size :: (Double, Double, Double), center :: Bool}
  | Polyhedron {points :: [[Double]], triangles :: [[Int]]}
  deriving (Eq, Ord)

defaultFa :: Double
defaultFa  = 12
defaultFs :: Double
defaultFs  = 1

cube     :: Node
cube      = Cube { size = (1,1,1), center=False }
cylinder :: Node
cylinder  = Cylinder { r = 1, r2 = 1, h = 1, center=False, fn = 0, fa = defaultFa, fs = defaultFs }
sphere   :: Node
sphere    = Sphere { r = 1, fn = 0, fa = defaultFa, fs = defaultFs }
poly     :: Node
poly      = Polyhedron { points    = [[0,0,0],[1,0,0],[0,1,0],[0,0,1]], 
                         triangles = [[0,1,2],[1,0,3],[0,2,3],[2,1,3]] }

union            :: Node -> Node -> Node
union (Union l) b = Union (l++[b])
union a b         = Union [a,b]
(/+)             :: Node -> Node -> Node
(Union l) /+ b    = Union (l++[b])
a /+ b            = Union [a,b]
diff             :: Node -> Node -> Node
diff (Difference l) b  = Difference (l++[b])
diff a b               = Difference [a,b]
(/-)             :: Node -> Node -> Node
(Difference l) /- b    = Difference (l++[b])
a /- b                 = Difference [a,b]
inter            :: Node -> Node -> Node
inter a b         = Intersection [a,b]
group            :: Node -> Node -> Node
group (Group l) b = Group (l++[b])
group a b         = Group [a,b]
(/.)             :: Node -> Node -> Node
(Group l) /. b    = Group (l++[b])
a /. b            = Group [a,b]
hull             :: Node -> Node -> Node
hull a b          = Hull [a,b]
mink             :: Node -> Node -> Node
mink a b          = Minkowski [a,b]

color            :: (Int, Int, Int, Int) -> Node -> Node
color (r,g,b,a) n = Color (r,g,b,a) n
mmult            :: [[Double]] -> Node -> Node
mmult m n         = if length m == 4 && 
                      (foldl (&&) True (map ((==4).length) m))
                    then MultMatrix m n
                    else error "mmult requires a 4x4 matrix"

scale            :: (Double, Double, Double) -> Node -> Node
scale (x,y,z) n   = Scale (x,y,z) n
(/*)              :: Node -> Double -> Node
n /* x             = Scale (x,x,x) n
rot              :: (Double, Double, Double) -> Node -> Node
rot (x,y,z) n     = Rotate (x,y,z) n
trans            :: (Double, Double, Double) -> Node -> Node
trans (x, y,z) n  = Translate (x,y,z) n
mirror           :: (Double, Double, Double) -> Node -> Node
mirror (x, y,z) n = Mirror (x,y,z) n

showFnFaFs fn fa fs = if fn > 0 then ", $fn=" ++ (show fn) else 
                                (if fa /= defaultFa then ", $fa=" ++ (show fa) else "") 
                             ++ (if fs /= defaultFs then ", $fs=" ++ (show fs) else "")
showCenter c        = if c then ", center=true" else ""
showKids   k        = foldl (\c -> \line -> c ++ line ++ "\n") "" (map show k) 
showVector x y z    = "["++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show z) ++"]"
showVector4 x y z a = "["++ (show x) ++ ", " ++ (show y) ++ ", " ++ (show z) ++ ", " ++ (show a) ++"]"

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

  show (Cube (w, h, d) c)  = "cube(" ++ (showVector w h d) 
                          ++ (showCenter c)
                          ++ ");"

  show (Polyhedron p t)    = "polyhedron(points = " ++ (show p) ++ ", triangles = " ++ (show t) ++ ");"

  show (Union kids)        = "union () {\n" ++ (showKids kids) ++"\n}"
  show (Difference kids)   = "difference () {\n" ++ (showKids kids) ++"\n}"
  show (Intersection kids) = "intersection () {\n" ++ (showKids kids) ++"\n}"
  show (Group kids)        = "{\n" ++ (showKids kids) ++"\n}"
  show (Minkowski kids)    = "minkowski () {\n" ++ (showKids kids) ++"\n}"
  show (Hull kids)         = "hull () {\n" ++ (showKids kids) ++"\n}"

  show (Scale (x,y,z) kid)     = "scale (" ++ (showVector x y z) ++ ")\n" ++ (show kid) ++ "\n}"
  show (Mirror (x,y,z) kid)    = "mirror (" ++ (showVector x y z) ++ ") {\n" ++ (show kid) ++ "\n}"
  show (Rotate (x,y,z) kid)    = "rotate (" ++ (showVector x y z) ++ ") {\n" ++ (show kid) ++ "\n}"
  show (Translate (x,y,z) kid) = "translate (" ++ (showVector x y z) ++ ") {\n" ++ (show kid) ++ "\n}"
  show (Color (r,g,b,a) kid)   = "color (" ++ (showVector4 r g b a) ++ ") {\n" ++ (show kid) ++ "\n}"

  show (MultMatrix m kid)   = "multmatrix (" ++ (show m) ++ ") {\n" ++ (show kid) ++ "\n}"

