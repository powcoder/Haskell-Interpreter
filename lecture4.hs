https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
{- Three ways to define a tree, the first uses lists for the left and right tree, the second just defines a left and right tree,
    and the third allows for any number of children placed into a list -}

data LTree t = Leaf t | Internal t [LTree t] [LTree t] deriving (Show)

data Tree t = Leaf2 t | Internal2 t (Tree t) (Tree t) deriving (Show)

data GenTree t = GLeaf t | GInternal t [GenTree t] deriving (Show)

{- 2 ways of writing linorder (for "list tree inorder") -}
linorder (Leaf a) = [a]
linorder (Internal a [l] [r]) = (linorder l) ++ (a : (linorder r))

linorder2 =
  \t ->
    case t of
      Leaf a -> [a]
      Internal a [l] [r] -> (linorder2 l) ++ (a : (linorder2 r))

{- preorder for the Tree binary tree type -}
preorder (Leaf2 a) = [a]
preorder (Internal2 a l r) = (a : preorder l) ++ (preorder r)

{- applyinorder takes a tree and a function and produces a tree with the same structure,
   but the function is applied to every node element -}
applyinorder (Leaf2 a) f = Leaf2 (f a)
applyinorder (Internal2 a l r) f = Internal2 (f a) (applyinorder l f) (applyinorder r f)

{- foldinorder takes a tree, a function, and an initial value
     foldinorder t (:) [] => [0,2,1,4,5,6,8,9,10]
     foldinorder t (+) 0  => 45
-}
foldinorder (Leaf2 a) f i = (f a i)
foldinorder (Internal2 a l r) f i = foldinorder l f (f a (foldinorder r f i))
