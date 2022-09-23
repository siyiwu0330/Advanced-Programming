module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West
  deriving (Eq, Show, Read, Ord)

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move South (x, y) = (x, y-1)
move East  (x, y) = (x+1, y)
-- complete the definition

moves :: [Direction] -> Pos -> Pos
moves [] (x, y) = (x, y)
moves d (x, y) =foldl (\acc  direction-> move direction acc) (x,y) d
-- replace with actual definition of moves, and likewise for the
-- other 'undefined' functions

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add Zero b = b
add (Succ a) (Succ b)   = add a  (Succ(Succ(b)))



mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult _ Zero = Zero
mult (Succ(Zero)) b = b
mult (Succ(a)) b = add b (mult a b) 

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ(a)) = 1+ nat2int a

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat i = Succ(int2nat(i-1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert i Leaf= Node i Leaf Leaf
insert i (Node j lTree rTree)
	| i<j =Node j (insert i lTree) rTree
	| i>j =Node j lTree (insert i rTree) 
	| otherwise = (Node j lTree rTree)

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Eq, Show, Read, Ord)

--In pinsert, the type will become "(Ord a) => a -> PTree a -> PTree a" where "a" belongs to Typeclass "Ord".
pinsert :: (Ord a) => a -> PTree a -> PTree a -- uncomment and replace with the proper type of pinsert
pinsert i PLeaf = PNode i PLeaf PLeaf
pinsert i (PNode j lPTree  rPTree)
	| i<j =PNode j (pinsert i lPTree) rPTree
	| i>j =PNode j lPTree (pinsert i rPTree) 
	| otherwise = (PNode j lPTree rPTree)
