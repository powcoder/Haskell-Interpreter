https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{- types in Haskell -}
{- Coordinate is a parameterized type -}
data Coordinate t = Coord2D t t | Coord3D t t t | Coord1D t | Zero deriving (Show)

{- Define what equality means for Coordinate.
   Before it was parameterized, the commented line let us compare all different Coordinates.
   Once it was parameterized, things became hard because we don't restrict the parameter.
-}
instance (Eq t) => Eq (Coordinate t) where
--  c1 == c2  = ((getx c1 == getx c2) && (gety c1 == gety c2) && (getz c1 == getz c2))
  Zero == Zero                       = True
  (Coord1D a) == (Coord1D x)         = a == x
  (Coord2D a b) == (Coord2D x y)     = a == x && b == y
  (Coord3D a b c) == (Coord3D x y z) = a == x && b == y && c == z

{- Getter methods. -}
getx Zero = 0
getx (Coord1D x) = x
getx (Coord2D x y) = x
getx (Coord3D x y z) = x
gety Zero = 0
gety (Coord2D x y) = y
gety (Coord1D x) = 0
gety (Coord3D x y z) = y
getz Zero = 0
getz (Coord2D x y) = 0
getz (Coord1D x) = 0
getz (Coord3D x y z) = z

{- We can create a distance function without using the helpers, but it needs lots of cases.
   If we want to compare different forms of Coordinate. -}
uglydistance (Coord2D a b) (Coord2D c d) = sqrt((a-c)*(a-c) + (b-d)*(b-d)) 
uglydistance (Coord3D a b x) (Coord3D c d y) = sqrt((a-c)*(a-c) + (b-d)*(b-d) + (x-y)*(x-y)) 

{- A simple distance function using the helper functions -}
distance c1 c2 = sqrt((getx c1 - getx c2) * (getx c1 - getx c2) + (gety c1 - gety c2) * (gety c1 - gety c2) + (getz c1 - getz c2) * (getz c1 - getz c2))

{- Create an infix |+ operator.  Will return the smallest coordinate that is the "largest"
   of its operands. -}
(|+) (Coord3D x y z) c = Coord3D (x + getx c) (y + gety c) (z + getz c)
(|+) c (Coord3D x y z) = Coord3D (x + getx c) (y + gety c) (z + getz c)
(|+) (Coord2D x y) c   = Coord2D (x + getx c) (y + gety c) 
(|+) c (Coord2D x y)   = Coord2D (x + getx c) (y + gety c) 
(|+) (Coord1D x) c     = Coord1D (x + getx c)
(|+) c (Coord1D x)     = Coord1D (x + getx c)
(|+) Zero Zero         = Zero












