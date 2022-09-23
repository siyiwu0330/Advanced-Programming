module Warmup where

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

type Pos = (Int, Int)
data Direction = North | South | East | West
  deriving (Eq, Show, Read, Ord)

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move East  (x,y) = (x+1, y)
move West  (x,y) = (x-1, y)
-- complete the definition

moves :: [Direction] -> Pos -> Pos
moves [] (x,y) = (x,y)
moves (m:ms) (x,y) = moves ms $ move m (x,y)
-- replace with actual definition of moves, and likewise for the
-- other 'undefined' functions

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add x Zero = x
add x (Succ y) = add (Succ x) y

mult :: Nat -> Nat -> Nat
mult x (Succ Zero) = x
mult x (Succ (Succ y)) = add x (mult x (Succ y))

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = nat2int x + 1

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat x = Succ (int2nat (x - 1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node y lf rf)
  | x == y = Node x lf rf
  | lf `compare` rf == GT = Node y lf (insert x rf)
  | otherwise             = Node y (insert x lf) rf

-- The polymorphic variant, to avoid name clashes with the above
data PTree a = PLeaf | PNode a (PTree a) (PTree a)
  deriving (Eq, Show, Read, Ord)

--pinsert :: FIXME  -- uncomment and replace with the proper type of pinsert
pinsert :: (Ord a) => a -> PTree a -> PTree a
pinsert x PLeaf = PNode x PLeaf PLeaf
pinsert x (PNode y lf rf)
  | x == y = PNode x lf rf
  | lf `compare` rf == GT = PNode y lf (pinsert x rf)
  | otherwise             = PNode y (pinsert x lf) rf