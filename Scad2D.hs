module Scad2D where

data Node2D =
    Circle {r :: Double}
  | Polygon {points :: [[Double]], paths :: [[Int]]}
  | Square {size :: (Double, Double), center :: Bool}
  | Projection Bool String -- this is a hack to avoid mutually recursive modules. 
  | Hull Node2D
  deriving (Eq, Ord)

circle  :: Node2D
circle   = Circle {r = 1}
polygon :: Node2D
polygon  = Polygon {points = [[0,0],[0,1],[1,0]], paths = [[0,1],[1,2],[2,0]]}
square  :: Node2D
square   = Square {size=(1,1), center = False}

hull    :: Node2D -> Node2D
hull a   = Hull a

showCenter c  = if c then ", center=true" else ""
showBool b    = if b then "true" else "false"

projection     :: Bool -> String -> Node2D
projection b s  = Projection b s

instance Show Node2D where
  show (Circle r) = "circle(r=" ++ (show r) ++ ");"
  show (Square (w, h) c) = "square([" ++ (show w) ++ ", " ++ (show h) ++ "]" ++ (showCenter c) ++ ");" 
  show (Polygon p l) = "polygon(points = " ++ (show p) ++ ", paths = " ++ (show l) ++ ");"

  show (Hull kid) = "hull ()\n" ++ (show kid)
  show (Projection c kid) = "projection (cut = "++(showBool c)++")\n" ++ kid




