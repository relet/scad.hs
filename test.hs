import Scad
import Csg
import Data.List

data Test = Test String Csg.Polyhedron Csg.Polyhedron
  deriving (Eq, Ord, Show)

main = let res = filter failed $ tests
       in if res == [] then 
         print "All tests succeeded."
       else 
         print ((show $ length res) ++ " / " ++ (show $ length tests) ++ " tests failed.\n" ++ (show res))

failed (Test _ a b) = a /= b

tests = [
          Test "unit cube is stable?" c (Csg.fixOrientation' c),
          Test "Torus is stable?" t (Csg.fixOrientation' t)
        ]



c  = Csg.cube
(Scad.Polyhedron t) = torus 5 3 6 6

-- render $ (c/>(0.5,0.5,0)) /- c2
--c2 = Polyhedron $ Csg.poly pts $ map reverse $ cube_tris
--     where pts = map (\p -> [(0.5::Double) + 0.5 * (sin $ angle p + pi/6), 0.5 + 0.5 * (cos $ angle p + pi/6), p!!2]) cube_pts
--           angle p = atan2 (p!!1-0.5) (p!!0-0.5)
