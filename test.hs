import Scad
import Csg hiding (Polyhedron)
import Data.List

main = print $ render $ (c/>(0.5,0.5,0)) /- c2

c  = Polyhedron Csg.cube
c2 = Polyhedron $ Csg.poly pts $ map reverse $ cube_tris
     where pts = map (\p -> [(0.5::Double) + 0.5 * (sin $ angle p + pi/6), 0.5 + 0.5 * (cos $ angle p + pi/6), p!!2]) cube_pts
           angle p = atan2 (p!!1-0.5) (p!!0-0.5)
