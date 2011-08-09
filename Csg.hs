module CSG where
import Data.Map as M hiding (map, filter)
import Data.Set as S hiding (map, filter)

type Vector   = [Double]
type Point    = [Double]
type Polygon  = [Point]
type Triangle = [Int]

-- todo: use memoizing data structures providing where methods 
--       for normals, nnormals, dnull and the like as per the reference

data Polyhedron = Polyhedron [Point] [Triangle] Extent -- vertices, polygons, extent
  deriving (Show, Eq, Ord)
data Status = Inside | Outside | Boundary | Unknown -- vertex status, tbd 
  deriving (Show, Eq, Ord, Enum)
data Relation = Coplanar | Intersect | DoNotIntersect -- polygon relationship 
  deriving (Show, Eq, Ord, Enum)
data Extent = Extent Double Double Double Double Double Double 
  deriving (Show, Eq, Ord)
data Line = Line Point Vector -- arbitrary point and unit direction vector 
  deriving (Show, Eq, Ord)
data Plane = Plane Vector Double -- unit normal, distance
  deriving (Show, Eq, Ord)

test_pts  = [[0::Double,0,0],[0,1,0],[1,0,0]]
test_tris = [[0::Int,1,2]]
test      = poly test_pts test_tris

-- construct memoizing structures from basic data
plane   :: Polygon -> Plane
plane p  = Plane n (dnull p n) 
           where n = nnormal p
poly    :: [Point] -> [Triangle] -> Polyhedron
poly p t = Polyhedron p t (extent p)

byIndex  :: [Point] -> [Triangle] -> [Polygon]
byIndex pts tris = map (map (pts!!)) tris

nnormal :: Polygon -> Vector
nnormal tri = norm $ normal tri
normal  :: Polygon -> Vector
normal tri  = cross u v 
              where u = zipWith (-) (tri!!1) (tri!!0)
                    v = zipWith (-) (tri!!2) (tri!!0)

extent    :: [Point] -> Extent
extent pts = Extent (minimum x) (minimum y) (minimum z) (maximum x) (maximum y) (maximum z)
             where x = map(!!0) pts
                   y = map(!!1) pts
                   z = map(!!2) pts 

pairs        :: [Int] -> [(Int,Int)]
pairs l       = [(x,y) | x<-l, y<-l, x/=y]
adjacent     :: [[Int]] -> (Map Int (Set Int))
adjacent tris = foldl (\m-> \p->insertWith (S.union) (fst p) (S.singleton (snd p)) m) M.empty $ foldl (++) [] $ map pairs tris
overlaps     :: Extent -> Extent -> Bool
overlaps (Extent a b c d e f) (Extent g h i j k l) = (a<j)&&(d>g)&&(b<k)&&(e>h)&&(c<l)&&(f>i)

splitBy      :: Polyhedron -> Polyhedron -> Polyhedron
splitBy (Polyhedron pa ta ea) (Polyhedron pb tb eb) = 
                if overlaps ea eb then
                  Polyhedron pa ta ea -- replace with split P 
                else Polyhedron pa ta ea
                where
                  all_pb_x_pa = map (all_pb_x.extent) all_pa_in_b
                  all_pb_x e  = filter (\p->overlaps (extent p) e) all_pb
                  all_pa_in_b = filter (\p->overlaps (extent p) (extent pb)) all_pa
                  all_pa      = byIndex pa ta 
                  all_pb      = byIndex pb tb

vdiv         :: Vector -> Double -> Vector
vdiv v d      = map (/d) v
norm         :: Vector -> Vector
norm a        = a `vdiv` (sqrt $ dot a a)
dot          :: Vector -> Vector -> Double
dot a b       = sum (zipWith (*) a b) 
cross        :: Vector -> Vector -> Vector
cross u v     =[(u!!1) * (v!!2) - (u!!2) * (v!!1),
                (u!!2) * (v!!0) - (u!!0) * (v!!2),
                (u!!0) * (v!!1) - (u!!1) * (v!!0)] 
len          :: Vector -> Double
len v         = sqrt $ sum (map (^2) v)
dnull       :: Polygon -> Vector -> Double
dnull p n    = sum $ zipWith (*) n (p!!0)
dist         :: Polygon -> Point -> Double 
dist po pt    = pt `dot` n - (dnull po n)
                where n = nnormal po

intersect    :: Polygon -> Polygon -> Relation
intersect a b = case cmp of
                 [EQ, EQ, EQ] -> Coplanar
                 [LT, LT, LT] -> DoNotIntersect
                 [GT, GT, GT] -> DoNotIntersect
                 other        -> Intersect -- MayIntersect actually tbc.
                where rel = map (dist a) b
                      cmp = map (compare 0) rel 
                      

split        :: Polygon -> Polygon -> [Polygon]
split a b     = if intersect a b == Intersect then
                  [a] -- split polygon here
                else [a]

solve            :: Double -> Double -> Double -> Double -> Double -> Double -> [Double]
solve a b c d e f = [(b*f - c*e) / (-den),
                     (a*f - c*d) / (den)]
                    where den = (a*e - b*d)
interLine    :: Plane -> Plane -> Line
interLine (Plane np dp) (Plane nq dq) = 
                Line (if   n!!0 > 0    then [0] ++ (solve (np!!1) (np!!2) dp (nq!!1) (nq!!2) dq)
                      else if n!!2 > 0 then (solve (np!!0) (np!!1) dp (nq!!0) (nq!!1) dq) ++ [0]
                      else                  [s3!!0, 0, s3!!1]) n
                where n  = cross np nq
                      s3 = solve (np!!0) (np!!2) dp (nq!!0) (nq!!2) dq

