--module Csg where
import Data.Map as M hiding (map, filter, split)
import Data.Set as S hiding (map, filter, split)
import Data.List as L hiding (map, filter, intersect)
import Data.Maybe as Q
-- TODO: actually use a dph for matrix stuff, tyvm

{- test code for profiling -}
main = print $ csgUnion t t2
t    = torus 5 3 6 6 
t2   = torus 6 4 6 6
torus                :: Double -> Double -> Int -> Int -> Polyhedron 
torus orad irad dt du = fplane (\u-> \v-> [orad * (sin (u*2*pi)) + irad * (sin (v*2*pi)) * (sin (u*2*pi)),
                                          orad * (cos (u*2*pi)) + irad * (sin (v*2*pi)) * (cos (u*2*pi)),
                                          irad * (cos (v*2*pi))]   ) (dt+1) (du+1)
fplane           :: (Double -> Double -> [Double]) -> Int -> Int -> Polyhedron
fplane fgen dt du = poly
                     (concatMap (\x-> map (fgen x) t) u)
                     (concatMap (\i-> [[seq1!!i, seq2!!i, seq3!!i],[seq2!!i, seq4!!i, seq3!!i]]) [0..length seq1 -1])
                where t = unit dt
                      u = unit du
                      seq1 = [u * dt + t | u<-[0..du-2], t<-[0..dt-2]]
                      seq2 = map (+1) seq1
                      seq3 = map (+dt) seq1
                      seq4 = map (+1) seq3
unit         :: Int -> [Double]
unit dt       = [0.0,1.0/(fromIntegral dt-1)..1.0]
{- end profiling -} 

type Vector   = [Double]
type Point    = Vector 
--type Polygon  = [Point]
type Polyset  = [Polygon]
type Triangle = [Int]
type Extent   = (Point, Point)

data Polygon = P [Point] Plane Extent -- points (plane nnormal, distance) extent
  deriving (Show, Eq, Ord)
data Polyhedron = Polyhedron [Point] [Triangle] Extent -- vertices, polygons, extent
  deriving (Show, Eq, Ord)
data Intersection = Intersection [(Double, Point)] Line
  deriving (Show, Eq, Ord)
data Relation = Coplanar | DoNotIntersect | Intersect Intersection -- polygon relationship 
  deriving (Show, Eq, Ord)
data Status = Inside | Outside | BoundarySame | BoundaryOpposite
  deriving (Show, Eq, Ord, Enum)
data Line = Line Point Vector -- arbitrary point and unit direction vector 
  deriving (Show, Eq, Ord)
data Plane = Plane Vector Double -- unit normal, distance
  deriving (Show, Eq, Ord)

cube_pts  = [[0::Double,0,0],[0,0,1],[0,1,0],[1,0,0],[0,1,1],[1,0,1],[1,1,0],[1,1,1]]
cube_tris = [[0::Int,1,2],[1,4,2],[0,3,1],[3,5,1],[0,2,3],[3,2,6],[3,6,5],[5,6,7],[5,7,4],[4,1,5],[2,7,6],[7,2,4]]
cube      = poly cube_pts cube_tris
cube2     = poly (map (vadd [0.5,0.5,0.5]) cube_pts) cube_tris
cube3     = poly (map (vadd [0.5,1,0.5])   cube_pts) cube_tris
other_po  = [[0::Double,0,1],[0,1,0],[1,0,0]]

-- to compensate for floating point errors we compare but roughly, in dubio pro equality
precision :: Double
precision  = 0.00001
(===)    :: Double -> Double -> Bool
a === b   = abs(a-b) < precision 
(=/=)    :: Double -> Double -> Bool
a =/= b   = not $ a === b 
(<<<)     :: Double -> Double -> Bool
a <<< b   = b-a > precision
(>>>)     :: Double -> Double -> Bool
a >>> b   = a-b > precision
(<=<)     :: Double -> Double -> Bool
a <=< b   = b-a >= -precision
(>=>)     :: Double -> Double -> Bool
a >=> b   = a-b >= -precision

-- construct memoizing structures from basic data
plane   :: [Point] -> Plane
plane p  = Plane n (dnull p n) 
           where n = nnormal p
poly    :: [Point] -> [Triangle] -> Polyhedron
poly p t = Polyhedron p t (extent p)
pgon    :: [Point] -> Polygon
pgon p   = P p (plane p) (extent p)

polyFromList   :: Polyset -> Polyhedron
polyFromList ps = poly pts tris
                  where pts  = S.toList $ S.fromList (concat pp) 
                        tris = map (map (\pt-> fromJust (L.elemIndex pt pts))) pp
                        points (P pts _ _) = pts
                        pp   = map points ps 
                       

byIndex  :: [Point] -> [Triangle] -> Polyset 
byIndex pts tris = map (pgon.(map (pts!!))) tris

nnormal :: [Point] -> Vector
nnormal tri = norm $ normal tri
normal  :: [Point] -> Vector
normal tri  = cross u v 
              where u = vsub (tri!!1) (tri!!0)
                    v = vsub (tri!!2) (tri!!0)

extent    :: [Point] -> Extent
extent pts = (map minimum list, map maximum list)
             where list = transpose pts

pairs        :: [Int] -> [(Int,Int)]
pairs l       = [(x,y) | x<-l, y<-l, x/=y]
adjacent     :: [[Int]] -> (Map Int (Set Int))
adjacent tris = foldl' (\m-> \p->insertWith (S.union) (fst p) (S.singleton (snd p)) m) M.empty $ concatMap pairs tris

-- todo: merge these two
overlaps     :: Extent -> Extent -> Bool
overlaps (a0, a1) (b0, b1) = foldl' (&&) True (zipWith (<=<) a0 b1) && (foldl' (&&) True (zipWith (>=>) a1 b0))
overlap      :: [(Double, Point)] -> [(Double, Point)] -> [(Double, Point)]
overlap a b   = [max (a!!0) (b!!0), min (a!!1) (b!!1)]

lreverse  :: Polyset -> Polyset
lreverse p = map reversePoly p
             where reversePoly (P pp (Plane pn d) pe) = P (L.reverse pp) (Plane (vmul pn (-1)) (-d)) pe

csgUnion       :: Polyhedron -> Polyhedron -> Polyhedron
csgUnion (Polyhedron pa ta ea) (Polyhedron pb tb eb)
                = polyFromList $ nub ( a ++ (b ++ (c ++ d)))
                  where (a, a', b', b) = splitBy ppa ea ppb eb 
                        c           = filter ((\x->(x == Outside) || (x == BoundarySame)).(inOrOut ppb)) a'
                        d           = filter ((==Outside).(inOrOut ppa)) b'
                        ppa         = byIndex pa ta
                        ppb         = byIndex pb tb
csgInter       :: Polyhedron -> Polyhedron -> Polyhedron
csgInter (Polyhedron pa ta ea) (Polyhedron pb tb eb)
                = polyFromList $ nub (c ++ d) 
                  where (a, a', b', b) = splitBy ppa ea ppb eb 
                        c           = filter ((\x->(x == Inside) || (x == BoundarySame)).(inOrOut ppb)) a'
                        d           = filter ((==Inside).(inOrOut ppa)) b'
                        ppa         = byIndex pa ta
                        ppb         = byIndex pb tb
csgDiff       :: Polyhedron -> Polyhedron -> Polyhedron
csgDiff (Polyhedron pa ta ea) (Polyhedron pb tb eb)
                = polyFromList $ nub (a ++ (c ++ d))
                  where (a, a', b', b) = splitBy ppa ea ppb eb 
                        c           = filter ((\x->(x == Outside) || (x == BoundaryOpposite)).(inOrOut ppb)) a'
                        d           = lreverse $ filter ((==Inside).(inOrOut ppa)) b'
                        ppa         = byIndex pa ta
                        ppb         = byIndex pb tb

splitBy      :: Polyset -> Extent -> Polyset -> Extent -> (Polyset, Polyset, Polyset, Polyset) 
-- returns all A outside B, all A inside B (subsected), all B inside A (subsected), all B outside A
splitBy ppa ea ppb eb = 
                if overlaps ea eb then
                  (snd all_pa, iter3, iter4, snd all_pb)
                else (ppa, [], [], ppb)
                where
                  iter4       = splitHbyH iter2 iter1
                  iter3       = splitHbyH iter1 iter2
                  iter2       = splitHbyH (fst all_pb) (fst all_pa)
                  iter1       = splitHbyH (fst all_pa) (fst all_pb)
                  all_pa      = L.partition (\p->overlaps (ext p) eb) ppa 
                  all_pb      = L.partition (\p->overlaps (ext p) ea) ppb

ext            :: Polygon -> Extent 
ext (P _ _ e)   = e

splitHbyP      :: Polyset -> Polygon -> Polyset -- split all polys in a hedron by a single poly
splitHbyP as b = concatMap (snd.(split b)) as 
                 where (P pb plb eb) = b
splitHbyH      :: Polyset -> Polyset -> Polyset 
splitHbyH as bs = foldl' splitHbyP as bs

vmul          :: Vector -> Double -> Vector
vmul v d       = map (*d) v
vmulv         :: Vector -> Vector -> Vector
vmulv v v'     = zipWith (*) v v'
vdiv          :: Vector -> Double -> Vector
vdiv v d       = map (/d) v
vsub          :: Vector -> Vector -> Vector
vsub a b       = zipWith (-) a b
vadd          :: Vector -> Vector -> Vector
vadd a b       = zipWith (+) a b
norm          :: Vector -> Vector
norm a          = a `vdiv` (len a)
dot           :: Vector -> Vector -> Double
dot a b        = sum (zipWith (*) a b) 
cross         :: Vector -> Vector -> Vector
cross u v      =[b * z - c * y,
                 c * x - a * z,
                 a * y - b * x] 
                where a = u!!0
                      b = u!!1
                      c = u!!2
                      x = v!!0
                      y = v!!1
                      z = v!!2
len           :: Vector -> Double
len v          = sqrt $ dot v v
avg           :: [Point] -> Point
avg po         = vdiv (foldl1 vadd po) 3
dnull         :: [Point] -> Vector -> Double
dnull p n      = sum $ zipWith (*) n (p!!0)

dist                :: Plane -> Point -> Double 
dist (Plane n d) pt  = pt `dot` n - d
                        
intersect     :: Polygon -> Polygon -> Relation
intersect a b  = case cmp of
                  [EQ, EQ, EQ] -> Coplanar
                  [LT, LT, LT] -> DoNotIntersect
                  [GT, GT, GT] -> DoNotIntersect
                  otherwise    -> 
                      if (length sega > 0) && (length segb > 0) && (overlaps (extl sega) (extl segb))
                      then Intersect (Intersection over int) else DoNotIntersect
                 where rela = map (dist ppb) pa
                       relb = map (dist ppa) pb
                       cmp = map (compare 0) rela 
                       int = interLine ppa ppb
                       over = overlap sega segb
                       (Plane na da) = ppa
                       (Plane nb db) = ppb
                       (P pa ppa ea) = a
                       (P pb ppb eb) = b
                       sega = interSeg a rela int
                       segb = interSeg b relb int
                       extl y = ([fst$y!!0], [fst$y!!1])

interLine    :: Plane -> Plane -> Line
interLine (Plane np dp) (Plane nq dq)  
              | n!!0 =/= 0 = Line [0, s10, s11] n
              | n!!2 =/= 0 = Line [s20, s21, 0] n
              | otherwise  = Line [s30, 0, s31] n
                where n  = cross np nq
                      (s10, s11) = solve (np!!1) (np!!2) dp (nq!!1) (nq!!2) dq
                      (s20, s21) = solve (np!!0) (np!!1) dp (nq!!0) (nq!!1) dq
                      (s30, s31) = solve (np!!0) (np!!2) dp (nq!!0) (nq!!2) dq

dobq   :: (Ord a) => [a] -> [a]
dobq v | length v == 1 = v++v 
       | otherwise     = v

interSeg          :: Polygon -> [Double] -> Line -> [(Double, Point)] 
interSeg po da l   = dobq $ sort $ plist
                     where plist = concat ((interSegV po da l) ++ (interSegE po da l))
interSegV         :: Polygon -> [Double] -> Line -> [[(Double, Vector)]]
interSegV (P po pp pe) da (Line p v) = 
                             [if da!!i === 0 
                              then [(len $ vsub pi p , pi)]
                              else [] 
                             | i<-[0..2], 
                               let pi = po!!i
                             ] 
interSegE         :: Polygon -> [Double] -> Line -> [[(Double, Vector)]]
interSegE (P po pp pe) da (Line p v) = 
                             [if (di>>>0) && (dj<<<0) 
                              then [(len $ vsub ip p, ip)]
                              else []
                             | i<-[0..2], j<-[0..2],
                               let pi = po!!i,
                               let pj = po!!j,
                               let di = da!!i,
                               let dj = da!!j,
                               let ip = vadd pi (vmul (vsub pj pi) ((-di)/(dj-di)))
                             ]

collinear    :: Polygon -> Bool
collinear (P po pp pe) = len (normal po) === 0

inPoly      :: Polygon -> Point -> Bool          -- including boundaries
inPoly (P po pp pe) pt = 
               (u >=> 0) && (v >=> 0) && ((u+v) <=< 1)  -- obviously cloned from a non-functional source 
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

trisect      :: Polygon -> Point -> Polyset 
trisect (P po pp pe) pt = 
                filter (not.collinear) $ map (pgon.(pt:)) pairs 
                where pairs = [last po, head po] : zipWith (\x-> \y-> [x,y]) po (tail po)
subsect      :: Polyset -> Point -> Polyset
subsect ps pt = if length ps == 1 then
                  trisect (ps!!0) pt
                else
                  concat subs
                where subs = map (\po-> if inPoly po pt then trisect po pt else [po]) ps 

-- Note to self: Why lazy evaluation is awesome: 
-- this method can subsect both polygons, but will do so only if you actually evaluate the results 
split        :: Polygon -> Polygon -> (Polyset, Polyset)
split a b = if overlaps ea eb then
               case intersect a b of 
                 Intersect (Intersection p l) -> 
                   (subsect (trisect a (snd$p!!0)) (snd$p!!1),
                    subsect (trisect b (snd$p!!0)) (snd$p!!1))
                 otherwise -> ([a],[b])
             else ([a],[b])
             where (P pa pla ea) = a
                   (P pb plb eb) = b

solve            :: Double -> Double -> Double -> Double -> Double -> Double -> (Double, Double)
solve a b c d e f = ((b*f - c*e) / (-den),
                     (a*f - c*d) / (den))
                    where den = (a*e - b*d)

ray          :: Polygon -> Line
ray (P po (Plane pn pd) _) = Line (avg po) pn
intPt        :: Line -> Double -> Point
intPt (Line p0 n) dist  = vadd p0 (vmul n dist)
perturb      :: Line -> Line
perturb (Line p n) = Line p (vadd n [pi/97, -pi/101, pi/103]) -- random would be better
distPL       :: Plane -> Point -> Line -> Double
distPL (Plane pn d) p0 (Line r rn) = (vsub p0 r) `dot` pn / (rn `dot` pn)

pnormal :: Plane -> Vector
pnormal (Plane n _) = n
pplane  :: Polygon -> Plane
pplane  (P _ p _)   = p

inOrOut      :: Polyset -> Polygon -> Status 
inOrOut pp po = classify $ take 1 $ L.sortBy dabs $ filter (analyze) $ map dispro pp 
                where 
                  r              = ray po
                  (Line bary rn) = r
                  dabs (a,b,c,d) (e,f,g,h) = compare (abs a) (abs e)
                  dispro (P px plx ex) = (distPL plx (px!!0) r, dist plx bary, rn `dot` (pnormal plx), (P px plx ex)) -- todo: memoize plane p / extract normal p 
                  analyze (dis, _, pro, p) | dis <<< 0              = False
                                           | isNaN dis              = False
                                           | dis >>> 0 && pro === 0 = False
                                           | dis === 0 && pro =/= 0 = inPoly p bary
                                           | dis >>> 0 && pro =/= 0 = inPoly p (intPt (Line bary rn) dis)
                                           | dis === 0 && pro === 0 = let (Line bary' rn') = perturb (Line bary rn)
                                                                          dis' = dist (pplane p) bary'
                                                                          pro' = rn' `dot` (pnormal$pplane p)
                                                                      in  analyze (dis', 0, pro', p)
                  classify []             = Outside
                  classify ((_, dxp, pxp, p):_) | dxp >>> 0 = Outside
                                                | dxp <<< 0 = Inside
                                                | pxp >>> 0 = BoundarySame
                                                | pxp <<< 0 = BoundaryOpposite

