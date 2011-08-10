module Csg where
import Data.Map as M hiding (map, filter)
import Data.Set as S hiding (map, filter)
import Data.List as L hiding (map, filter, intersect)
import Debug.Trace (trace)

type Vector   = [Double]
type Point    = [Double]
type Polygon  = [Point]
type Triangle = [Int]
type Extent   = [[Double]]

-- todo: use memoizing data structures providing where methods 
--       for normals, nnormals, dnull and the like as per the reference

data Polyhedron = Polyhedron [Point] [Triangle] Extent -- vertices, polygons, extent
  deriving (Show, Eq, Ord)
data Status = Inside | Outside | Boundary | Unknown -- vertex status, tbd 
  deriving (Show, Eq, Ord, Enum)
data Intersection = Intersection Extent Line
  deriving (Show, Eq, Ord)
data Relation = Coplanar | DoNotIntersect | Intersect Intersection Intersection -- polygon relationship 
  deriving (Show, Eq, Ord)
data Line = Line Point Vector -- arbitrary point and unit direction vector 
  deriving (Show, Eq, Ord)
data Plane = Plane Vector Double -- unit normal, distance
  deriving (Show, Eq, Ord)

test_pts  = [[0::Double,0,0],[0,1,0],[1,0,0]]
test_pts2  = [[0::Double,0,1],[0,1,0],[1,0,0]]
test_tris = [[0::Int,1,2]]
test      = poly test_pts test_tris

-- to compensate for floating point errors we compare roughly
(===)    :: Double -> Double -> Bool
a === b   = abs(a-b) < 0.00001 

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
extent pts = [map minimum list, map maximum list]
             where dim  = length (pts!!0)-1
                   list = map (\d->map (!!d) pts) [0..dim]

pairs        :: [Int] -> [(Int,Int)]
pairs l       = [(x,y) | x<-l, y<-l, x/=y]
adjacent     :: [[Int]] -> (Map Int (Set Int))
adjacent tris = foldl (\m-> \p->insertWith (S.union) (fst p) (S.singleton (snd p)) m) M.empty $ foldl (++) [] $ map pairs tris
overlaps     :: Extent -> Extent -> Bool
overlaps a b  = foldl (&&) True $ zipWith (\x-> \y-> ((x!!0)<=(y!!1)&&(x!!1)>=(y!!0))) a b

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

vmul          :: Vector -> Double -> Vector
vmul v d       = map (*d) v
vdiv          :: Vector -> Double -> Vector
vdiv v d       = map (/d) v
vsub          :: Vector -> Vector -> Vector
vsub a b       = zipWith (-) a b
vadd          :: Vector -> Vector -> Vector
vadd a b       = zipWith (+) a b
norm          :: Vector -> Vector
norm a          = a `vdiv` (sqrt $ dot a a)
dot           :: Vector -> Vector -> Double
dot a b        = sum (zipWith (*) a b) 
cross         :: Vector -> Vector -> Vector
cross u v      =[(u!!1) * (v!!2) - (u!!2) * (v!!1),
                 (u!!2) * (v!!0) - (u!!0) * (v!!2),
                 (u!!0) * (v!!1) - (u!!1) * (v!!0)] 
len           :: Vector -> Double
len v          = sqrt $ sum (map (^2) v)
dnull         :: Polygon -> Vector -> Double
dnull p n      = sum $ zipWith (*) n (p!!0)

dist                :: Plane -> Point -> Double 
dist (Plane n d) pt  = pt `dot` n - d
                        
intersect     :: Polygon -> Polygon -> Relation
intersect a b  = case cmp of
                  [EQ, EQ, EQ] -> Coplanar
                  [LT, LT, LT] -> DoNotIntersect
                  [GT, GT, GT] -> DoNotIntersect
                  other        -> 
                      if overlaps sega segb
                      then Intersect (Intersection sega int) (Intersection segb int) else DoNotIntersect
                 where rela = map (dist pb) a
                       relb = map (dist pa) b
                       cmp = map (compare 0) rela 
                       int = interLine pa pb
                       pa  = plane a
                       pb  = plane b
                       sega = interSeg a rela int
                       segb = interSeg b relb int

interLine    :: Plane -> Plane -> Line
interLine (Plane np dp) (Plane nq dq) = 
                Line (if   n!!0 > 0    then [0] ++ (solve (np!!1) (np!!2) dp (nq!!1) (nq!!2) dq)
                      else if n!!2 > 0 then (solve (np!!0) (np!!1) dp (nq!!0) (nq!!1) dq) ++ [0]
                      else                  [s3!!0, 0, s3!!1]) n
                where n  = cross np nq
                      s3 = solve (np!!0) (np!!2) dp (nq!!0) (nq!!2) dq

dobq   :: (Ord a) => [a] -> [a]
dobq v  = if length v == 1 then v++v else v

interSeg          :: Polygon -> [Double] -> Line -> Extent --cleanup
interSeg po da l   = [dobq $ map (fst) $ sort $ plist]
                     where plist = foldl (++) [] ((interSegV po da l) ++ (interSegE po da l))
interSegV         :: Polygon -> [Double] -> Line -> [[(Double, Vector)]]
interSegV po da (Line p v) = [if da!!i === 0 
                              then [(len $ vsub pi p , pi)]
                              else [] 
                             | i<-[0..length po-1], 
                               let pi = po!!i
                             ] 
interSegE         :: Polygon -> [Double] -> Line -> [[(Double, Vector)]]
interSegE po da (Line p v) = [if (di>0) && (dj<0) 
                              then [(len $ vsub ip p, ip)]
                              else []
                             | i<-[0..length po-1], j<-[0..length po-1],
                               let pi = po!!i,
                               let pj = po!!j,
                               let di = da!!i,
                               let dj = da!!j,
                               let ip = vadd pi (vmul (vsub pj pi) ((-di)/(dj-di)))
                             ]

collinear    :: Polygon -> Bool
collinear po  = len (cross (vsub (po!!1) (po!!0)) (vsub (po!!2) (po!!0))) === 0

inPoly      :: Polygon -> Point -> Bool
inPoly po pt = (u > 0) && (v > 0) && (u+v<1) -- ouch. 
               where u = (d11 * d02 - d01 * d12) * inv
                     v = (d00 * d12 - d01 * d02) * inv
                     inv = 1 / (d00 * d11 - d01 * d01)
                     d00 = v0 `dot` v0
                     d01 = v0 `dot` v1
                     d02 = v0 `dot` v2
                     d11 = v1 `dot` v1
                     d12 = v1 `dot` v2
                     v0 = (po!!2) `vsub` p0
                     v1 = (po!!1) `vsub` p0
                     v2 = pt `vsub` p0
                     p0 = po!!0

trisect      :: Polygon -> Point -> [Polygon]
trisect po pt = filter (not.collinear) $ map (++[pt]) pairs 
                where pairs = zipWith (\x-> \y-> [x,y]) po (tail po ++ [head po])

split        :: Polygon -> Polygon -> [Polygon]
split a b     = case intersect a b of 
                  Intersect (Intersection e1 l1) (Intersection e2 l2) -> 
                    [a] -- trisect a by all p1, b by all p2, return
                  _ -> [a]

solve            :: Double -> Double -> Double -> Double -> Double -> Double -> [Double]
solve a b c d e f = [(b*f - c*e) / (-den),
                     (a*f - c*d) / (den)]
                    where den = (a*e - b*d)
