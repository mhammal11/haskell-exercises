{--
Michael Hammal
--}

{-# LANGUAGE TemplateHaskell #-}
module A4 where

import Control.Applicative
import Control.Monad (void)
import Test.QuickCheck
import Data.Functor.Identity
import Data.Functor.Const
import Text.Printf


-- More on Functor; Foldable; Traversable; and Applicative

-- Tree as Traversable
-- Identity, Const, and Accumulator as Applicative

-----------------------------------------------------------------------------
-- First define Tree with Traversable instance

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show,Eq)

-- Arbitrary instances for Tree --
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
  t_0 <- arbitrary
  t_1 <- arbitrary
  if m == 0
    then return (Node (Leaf t_0) (Leaf t_1))
    else do
      left <- arbitrarySizedTree (m `div` 4)
      right <- arbitrarySizedTree (m `div` 4)
      return (Node left right)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)

instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Node t1 t2) = foldr f (foldr f b t2) t1

instance Traversable Tree where
  traverse f (Leaf a) = pure Leaf <*> f a
  traverse f (Node t1 t2) = pure Node <*> (traverse f t1) <*> (traverse f t2)

-----------------------------------------------------------------------------
-- Identity, Const, and Accumulator with Applicative instances

{--
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure x = Identity x
    Identity f <*> Identity x = Identity (f x)
--}

{--
newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
    fmap _ (Const x) = Const x

instance Applicative (Const [a]) where
    pure _ = Const []
    Const xs <*> Const ys = Const (xs ++ ys)
--}

data Accumulator c a = Acc { accumulate :: c -> (c,a) }

instance Functor (Accumulator c) where
  fmap f (Acc acc) = Acc $ \c -> let (c',a) = acc c in (c', f a)

instance Applicative (Accumulator c) where
  pure a = Acc (\c -> (c,a))
  (Acc accf) <*> (Acc acca) = Acc $ \c -> let (c',g) = accf c
                                              (c'',b) = acca c'
                                          in (c'', g b)

-----------------------------------------------------------------------------
-- Helper for running traversals and some small example trees

runTraverseAcc :: (a -> Accumulator c b) -> Tree a -> c -> (c,Tree b)
runTraverseAcc f tr c = accumulate (traverse f tr) c

itr :: Tree Int
itr = Node (Node (Leaf 10) (Leaf 20)) (Leaf 30)

etr :: Tree Int
etr = Node (Node (Leaf 20) (Leaf 40)) (Leaf 60)

str :: Tree String
str  = Node (Node (Leaf "cat") (Leaf "dog")) (Leaf "horse")

ntr :: Tree (Int,String)
ntr = Node (Node (Leaf (1,"cat")) (Leaf (2,"dog"))) (Leaf (3,"horse"))

ptr :: Tree ()
ptr = Node (Node (Leaf ()) (Leaf ())) (Leaf ())

-----------------------------------------------------------------------------
-- Assignment Tasks

-- | `generateTrees` takes a bound and a function that makes a tree of a
--   certain shape. It creates a list of all possible trees of the given
--   shape with leaves in the range [0..bound]
-- TODO Task
generateTrees :: Int -> (Int -> Tree Int) -> [Tree Int]
generateTrees bound maker = traverse (\a -> [x | x <- [0..bound]]) (maker bound)
  
-- | `removeNumbering` takes a tree where each node has a number and removes
--   the number. This is essentially fmap but you must implement it using
--   traverse.
-- TODO Task
removeNumbering :: Tree (Int,a) -> Tree a
removeNumbering tr = runIdentity $ traverse (\(a,b) -> Identity b) tr

-- | `toBinary` takes a tree with integer leaves and returns a tree where
--   each integer is replaced by its binary representation (expressed as
--   string). Again this is essentially fmap but you must implement it
--   using traverse
-- TODO Task
toBinary :: Tree Int -> Tree String
toBinary tr = runIdentity $ traverse (Identity . printf "%b") tr

-- | `collectLeaves` takes a tree and returns a list of the leaves. This is
--   essentially fold but you must implement it using traverse. The order
--   of the leaves does not matter.
-- TODO Task
collectLeaves :: Tree a -> [a]
collectLeaves tr = getConst $ traverse (\a -> Const [a]) tr

-- | `countLeaves` takes a tree and counts the leaves. You must use traverse
--   to implement it.
-- TODO Task
countLeaves :: Tree a -> Int
countLeaves tr = fst (runTraverseAcc (\a -> Acc (\c -> (c+1,a))) tr 0)

-- | `collectMultiply` takes a tree with integer leaves and does two things:
--   it returns a lisst of the leaves and updates every leaf in the tree by
--   multiplying it by 10. You must use just one call to traverse to
--   perform both actions. The order of the leaves in the list does not
--   matter.
-- TODO Task
collectMultiply :: Tree Int -> ([Int],Tree Int)
collectMultiply tr = runTraverseAcc (\a -> Acc (\c -> (a:c,a*10))) tr []

-- | `collectSegments` takes a tree and does two things: it returns a list of
--   the leaves (in any order) and updates every leaf to be a list of all
--   the suffix of the list of the nodes accumulated so far in the
--   traversal.
-- TODO Task
collectSegments :: Tree a -> ([a],Tree [a])
collectSegments tr = runTraverseAcc (\a -> Acc (\c -> (a:c,a:c))) tr []

-- | `numberLeaves` takes a tree and returns a tree where all the leaves are
--   numbers starting from 1. The numbering reflects the traversal order.
-- TODO Task
numberLeaves :: Tree a -> Tree (Int,a)
numberLeaves tr = snd (runTraverseAcc (\a -> Acc (\c -> (c+1,(c+1,a)))) tr 0)

-- | generateTree takes a list of values and a tree shape and fills the
--   leaves with the given values. If there are more values in the list,
--   just ignore them.
-- TODO Task
generateTree :: [a] -> (Tree ()) -> Tree a
generateTree ns shape = snd (runTraverseAcc (\a -> Acc (\c -> (c+1,ns !! c))) shape 0)

-----------------------------------------------------------------------------
-- Implement your properties here.
-----------------------------------------------------------------------------
-- Property to check if generateTrees implementation with traverse gives 
-- the same result as an fmap and Applicative implementation 
prop_generateTrees :: Int -> Bool
prop_generateTrees bound = generateTrees bound (\a -> Node (Leaf a) (Leaf a)) == fmap (\(Node (Leaf a) (Leaf b)) -> Node (Leaf $ fst a) (Leaf $ snd b)) ((\a -> Node (Leaf a) (Leaf a)) <$> ((,) <$> [0..bound] <*> [0..bound]))

-- Property to check if removeNumbering implementation with traverse gives 
-- the same result as an fmap implementation 
prop_removeNumbering :: Tree (Int,String) -> Bool
prop_removeNumbering tr = removeNumbering tr == fmap snd tr

-- Property to check if toBinary implementation with traverse gives 
-- the same result as an fmap implementation 
prop_toBinary :: Tree Int -> Bool
prop_toBinary tr = toBinary tr == fmap (printf "%b") tr

-- Property to check if collectLeaves implementation with traverse gives 
-- the same result as a recursive implementation with pattern matching
prop_collectLeaves :: Tree Int -> Bool
prop_collectLeaves tr = collectLeaves tr == collect tr
  where collect (Leaf a) = [a]
        collect (Node (Leaf a) (Leaf b)) = a : [b]
        collect (Node l r) = collect l ++ collect r

-- Property to check if countLeaves implementation with traverse and an
-- accumulator gives the same result as a recursive implementation with 
-- pattern matching
prop_countLeaves :: Tree Int -> Bool
prop_countLeaves tr = countLeaves tr == count tr
  where count (Leaf a) = 1
        count (Node (Leaf a) (Leaf b)) = 2
        count (Node l r) = count l + count r

-- Property to check if collectMultiply implementation with traverse and an
-- accumulator gives the same result as combining a recursive implementation 
-- with pattern matching with an fmap implementation  
prop_collectMultiply :: Tree Int -> Bool
prop_collectMultiply tr = collectMultiply tr == (reverse $ collect tr, fmap (*10) tr)
  where collect (Leaf a) = [a]
        collect (Node (Leaf a) (Leaf b)) = a : [b]
        collect (Node l r) = collect l ++ collect r
-----------------------------------------------------------------------------
-- End of property implementation.
-----------------------------------------------------------------------------
return []
allProps = void $quickCheckAll
