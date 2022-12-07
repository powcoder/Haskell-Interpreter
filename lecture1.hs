https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{- this is a comment -}

{- write factorial "normal" style -}
factorial1 n = 
  if n == 0
   then
    1
   else
    n * factorial1 (n - 1)

{- write factorial as cases -}
factorial2 0 = 1
factorial2 n = n * factorial2 (n - 1)

{- write factorial as a lambda function that is bound to the name factorial3 -}
factorial3 =
  \n -> if n == 0
         then
          1
         else
          n * factorial3 (n - 1)

{- myappend appends two lists -}
myappend :: Eq a => [a] -> [a] -> [a]
myappend l1 l2 =
  if l1 == []
   then
     l2
   else
     {- (cons (car l1) (myappend (cdr l1) l2)) -}
     {- car is head, cdr is tail, and cons is : -}
     (head l1) : (myappend (tail l1) l2)

{- myreverse which reverses a list and uses myappend -}
myreverse [] = []
myreverse l = myappend (myreverse (tail l)) ((head l) : [])

{- the same but it does a function composition of myreverse2 and tail -}
myreverse2 [] = []
myreverse2 l = myappend ((myreverse2 . tail) l) ((head l) : [])

{- replaceall 1 2 [2,2,3,4]  => [1,1,3,4] -}
replaceall x y l =
  if l == []
    then
      []
    else
      if (head l) == x
        then
          y : replaceall x y (tail l)
        else
          (head l) : replaceall x y (tail l)

{- doing a replaceall as a lambda leads to a type ambiguity.  We resolve this by
   specifying what the type of the function is -}
replaceall2 :: Eq a => a -> a -> [a] -> [a]
replaceall2 =
  \x y l ->  if l == []
               then
                 []
               else
                 if (head l) == x
                   then
                     y : replaceall2 x y (tail l)
                   else
                     (head l) : replaceall2 x y (tail l)

{- merge [2,3,6] [1,4,8,9]  => [1,2,4,6,8,9] -}
