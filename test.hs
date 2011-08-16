import Scad
import Csg hiding (Polyhedron)

main = print $ render $ c /+ c2

c  = Polyhedron Csg.cube
c2 = Polyhedron $ Csg.poly pts cube_tris
     where pts = map (\pt -> [pt!!0 + sin(pi/8) - 0.5, pt!!1 + cos(pi/8) - 0.5, pt!!2]) cube_pts


