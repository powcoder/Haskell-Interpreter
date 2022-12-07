https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{- Monads!!!! -}

data Value t = Value t | NoValue deriving (Show, Eq)

-- return function for Value

myreturn x = Value x

-- bind function
mybind:: Value t -> (t -> Value t1) -> Value t1
mybind (Value x) f = f x
mybind NoValue _ = NoValue

-- create an add function to add two monads
(+++) vx vy =
  vx `mybind` (\x -> vy `mybind` (\y -> myreturn (x + y)))

(//) vx vy =
  vx `mybind` (\x -> vy `mybind` (\y -> if y == 0 then NoValue else myreturn (x / y)))

vapp vx f vy =
  vx `mybind` (\x -> vy `mybind` (\y -> myreturn (f x y)))

-- write a squareroot function where squareroot of a negative value is NoValue (sqrt)

vsqrt vx =
  vx `mybind` (\x -> if x < 0 then NoValue else myreturn (sqrt x))

{- Haskell has many built-in monads.  One is Maybe.
     data Maybe t = Just t | Nothing

     the return function is "return"
     the bind function is ">>="
-}

(++++) mx my =
  mx >>= (\x -> my >>= (\y -> return (x + y)))

{- do the divide and vapp functions using Haskell's Maybe -}

(///) mx my =
  mx >>= (\x -> my >>= (\y -> if y == 0 then Nothing else return (x / y)))

mapp mx f my =
  mx >>= (\x -> my >>= (\y -> return (f x y)))

mapp2 mx f my = do
  x <- mx
  y <- my
  return (f x y)

msqrt mx = do
  x <- mx
  if x < 0 then Nothing else return (sqrt x)


