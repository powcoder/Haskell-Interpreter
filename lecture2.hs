https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{- factorial 3 ways -}
factorial1 n =
  if n == 0
   then
    1
   else
    n * factorial1 (n - 1)

factorial2 0 = 1
factorial2 n = n * factorial2 (n - 1)

factorial3 =
  \n ->
    if n == 0
     then
      1
     else
      n * factorial3 (n - 1)

fact_cps 0 return = return 1
fact_cps n return = fact_cps (n - 1) (\v -> return (n * v))

{- merge_cps -}
merge_cps [] l return = return l
merge_cps l [] return = return l
merge_cps l1 l2 return =
  if (head l1) < (head l2)
   then
    merge_cps (tail l1) l2 (\v -> return ((head l1) : v))
   else
    merge_cps l1 (tail l2) (\v -> return ((head l2) : v))

{- split_cps -}
split_cps [] return = return [] []
split_cps [a] return = return [a] []
split_cps l return = split_cps ((tail . tail) l) (\v w -> return ((head l) : v) (((head . tail) l) : w))

{- mergesort_cps -}
mergesort_cps [] return = return []
mergesort_cps [a] return = return [a]
mergesort_cps l return = split_cps l (\a b -> mergesort_cps a (\c -> mergesort_cps b (\d -> merge_cps c d return)))

{- create a Coordinate type that represents a point -}
data Coordinate = Coord2 Double Double | Coord3 Double Double Double deriving (Show)

getx (Coord2 x y) = x
getx (Coord3 x y z) = x
gety (Coord2 x y) = y
gety (Coord3 x y z) = y
getz (Coord2 x y) = 0
getz (Coord3 x y z) = z

distance (Coord2 a b) (Coord2 c d) = sqrt((a - c) * (a - c) + (b - d) * (b -d))
distance (Coord3 a b c) (Coord3 x y z) = sqrt((a - x) * (a - x) + (b - y) * (b -y) + (c -z) * (c-z))

distance2 v w = sqrt((getx v - getx w) * (getx v - getx w) + (gety v - gety w) * (gety v - gety w) + (getz v - getz w) * (getz v - getz w))












