data Polyhedron = Polyhedron [[Double]] [[Int]] --points, triangles
  deriving (Show)

test_tri = Polyhedron [[0,0,0],[0,1,0],[1,0,0]] [[0,1,2],[0,2,1]]

normals :: Polyhedron -> [[Double]]
normals (Polyhedron pts tris) = map normal realtris 
                          where realtris = byIndex pts tris
byIndex  :: [[Double]] -> [[Int]] -> [[[Double]]]
byIndex pts tris = map (map (pts!!)) tris
normal :: [[Double]] -> [Double]
normal tri = [(u!!1) * (v!!2) - (u!!2) * (v!!1),
              (u!!2) * (v!!0) - (u!!0) * (v!!2),
              (u!!0) * (v!!1) - (u!!1) * (v!!0)] 
             where u = zipWith (-) (tri!!1) (tri!!0)
                   v = zipWith (-) (tri!!2) (tri!!0)

extent   :: Polyhedron -> [[Double]] --can be simplified
extent (Polyhedron pts tris) = [[minimum x, maximum x], [minimum y, maximum y],
                                [minimum z, maximum z]]
                               where x = map(!!0) pts
                                     y = map(!!1) pts
                                     z = map(!!2) pts 


