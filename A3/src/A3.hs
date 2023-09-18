{--
Michael Hammal
--}

{-# LANGUAGE TemplateHaskell #-}
module A3 where

import Control.Applicative
import Control.Monad (void)
import Test.QuickCheck

------------------------------------------------------------------------------------------
-- Using applicative instance []
-- TODO : You can find all the details you need for implementation in `ReadMe.org`
{-
-- Apply a function to elements from two lists in order to generate a third list.
-- This function works by combining two lists with <*> and then applying the given
-- function as the glue that combines the elements from the two lists.
-}
applyAll :: (a -> b -> c) -> [a] -> [b] -> [c]
applyAll f xs ys = pure f <*> xs <*> ys

-- TODO : You can find all the details you need for implementation in `ReadMe.org`
{- 
-- Compute the cartesian product of elements from two lists
-- This function works by combining two lists with <*> and then applying the (,)
-- function as the glue that combines the elements from the two lists.
-}
cartesianProduct :: [a] -> [a] -> [(a,a)]
cartesianProduct xs ys = pure (,) <*> xs <*> ys

-- TODO : You can find all the details you need for implementation in `ReadMe.org`
{-
-- Multiply elements from two lists and generate a list of elements bigger than a given bound
-- This function works by combining two lists with <*> and then applying the (*)
-- function as the glue that combines the elements from the two lists, and then finally
-- filtering the list based on the elements that are greater than the given bound.
-}
generateLargeProducts :: Int -> [Int] -> [Int] -> [Int]
generateLargeProducts bound xs ys = filter (> bound) $ pure (*) <*> xs <*> ys

data Student =
  MkStudent { name :: String
            , classId :: Int
            , grade :: Int
            }
  deriving (Eq, Show)                       

-- TODO : You can find all the details you need for implementation in `ReadMe.org`
{-
-- Generate a students record from a list of student information.
-- This function works by combining three lists with <*> and then applying the 
-- record data type as the glue that combines the elements from the three lists.
-}
generateStudents :: [String] -> [Int] -> [Int] -> [Student]
generateStudents names classes grades = pure MkStudent <*> names <*> classes <*> grades

------------------------------------------------------------------------------------------
-- Applicative instance Tree
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

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

-- Sample trees
t0 :: Tree Int
t0 = Node (Leaf 1) (Leaf 3)

t1 :: Tree Int
t1 = Node (Node (Node (Leaf 3) (Node (Leaf 4) (Leaf 8))) (Leaf 7)) (Leaf 1)

-- TODO Implement the Functor instance for the tree we defined above.
-- Functor instance of the binary tree defined above
instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node tl tr) = Node (fmap f tl) (fmap f tr) 

-- Applicative instance of the binary tree defined above
-- TODO Implement the applicative instance for the tree we defined above.
instance Applicative Tree where
  pure a = Leaf a
  (Leaf a) <*> t = fmap a t
  (Node tl tr) <*> t = Node (tl <*> t) (tr <*> t) 

-- TODO : You can find all the details you need for implementation in `ReadMe.org`
{-
-- Apply two functions to the elements in a tree.
-- This function works by creating a tree with the two given functions as the
-- leaves of the tree, and then combining that tree with the given tree using
-- the <*> function to create a new tree after applying the two functions to
-- the elements of the tree.
-}
doubleTree :: (a -> b) -> (a -> b) -> Tree a -> Tree b
doubleTree f g tr = Node (pure f) (pure g) <*> tr

-- Foldable instance of the binary tree defined above
-- TODO Implement Foldable instance for the tree we defined above.
instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Node tl tr) = foldr f (foldr f b tr) tl

-- Traversable instance of the binary tree defined above
-- TODO Implement Traversable instance for the tree we defined above.
instance Traversable Tree where
  traverse f (Leaf a) = fmap Leaf (f a)
  traverse f (Node tl tr) = fmap Node (traverse f tl) <*> traverse f tr

-- TODO : You can find all the details you need for implementation in `ReadMe.org`
{-
-- Map a list of functions to elements in a tree, resulting in a list of trees.
-- This function works by traversing the given tree and then mapping each function
-- from the given list of functions to an element from the given tree.
-}
generateTreesF :: [Int -> Int] -> Tree Int -> [Tree Int]
generateTreesF funs tr = traverse (\a -> funs <*> pure a) tr

-- TODO : You can find all the details you need for implementation in `ReadMe.org`
{-
-- Convert elements from two lists into leaves of a new tree, resulting in a list of trees.
-- This function works by combining the two given lists with <*> and then applying the (,)
-- function as the glue that combines the elements from the two lists. This is done in order
-- to traverse that result and apply a function that takes the tuples created in the list and
-- creates a new tree with the elements of the tuple as the two leaves of the tree. This creates
-- a list of list of trees, and then the head of that is taken to ouput a list of trees. 
-}
generateTreesL :: [Int] -> [Int] -> [Tree Int]
generateTreesL xs ys = head (traverse (\(a, b) -> pure (Node (Leaf a) (Leaf b))) (pure (,) <*> xs <*> ys)) 
{- 
-- A different implementation relies on the Applicative instance to combine the 
-- two lists of integers and apply a function that turns a integer from each list 
-- into the two leaves of a tree, resulting in a list of trees. 
-- generateTreesL xs ys = pure (\a b -> Node (Leaf a) (Leaf b)) <*> xs <*> ys 
-}
------------------------------------------------------------------------------------------
-- QuickCheck Properties
-- Show instance to be able to compile the QuickCheck property for applyAll
instance Show (a -> b) where
  show a = "function"
-- Property to check if applyAll Applicative implementation gives the same result
-- as a list comprehension implementation 
prop_applyAll :: (Int -> Int -> Int) -> [Int] -> [Int] -> Bool
prop_applyAll f xs ys = applyAll f xs ys == [f x y | x <- xs, y <- ys]

-- Property to check if cartesianProduct Applicative implementation gives the same result
-- as a list comprehension implementation 
prop_cartesianProduct :: [Int] -> [Int] -> Bool
prop_cartesianProduct xs ys = cartesianProduct xs ys == [(x,y) | x <- xs, y <- ys] 

-- Property to check if generateLargeProducts Applicative implementation gives the same 
-- result as a list comprehension implementation 
prop_generateLargeProducts :: Int -> [Int] -> [Int] -> Bool
prop_generateLargeProducts bound xs ys = generateLargeProducts bound xs ys == [x*y | x <- xs, y <- ys, x*y > bound]

-- Property to check if generateTreesL traverse implementation gives the same result
-- as an applicative implementation
prop_generateTreesL :: [Int] -> [Int] -> Bool
prop_generateTreesL xs ys = generateTreesL xs ys == (pure (\a b -> Node (Leaf a) (Leaf b)) <*> xs <*> ys) 

-- Property to check if generateTreesL traverse implementation gives the same result
-- as a list comprehension implementation
prop_generateTreesL2 :: [Int] -> [Int] -> Bool
prop_generateTreesL2 xs ys = generateTreesL xs ys == [Node (Leaf x) (Leaf y) | x <- xs, y <- ys]

return []

allProps = void $quickCheckAll