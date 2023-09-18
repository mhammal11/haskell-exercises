{-# LANGUAGE FlexibleContexts #-}
module Mid1 where

import Prelude hiding (zip,foldl,(++))
import Test.QuickCheck
import Test.Hspec

------------------------------------------------------------------------------------
-- Trees; Foldable

data Tree a = Empty
            | Node a (Tree a) (Tree a) (Tree a)
  deriving (Eq,Show)

-- Example

leaf a = Node a Empty Empty Empty

t1 :: Tree Int
t1 = Node 1 
       (Node 2 (leaf 3) (leaf 4) (leaf 5))
       (leaf 6)
       (Node 7 
         (leaf 8) 
         (Node 9 (leaf 10) (leaf 11) (leaf 12))
         (leaf 13))

{--

                        1
                    /   |   \
                  2     6      7
               / | \         /  |  \
              3  4 5        8   9   13
                              / | \
                            10  11 12
--}

foldT :: (a -> b -> b -> b -> b) -> b -> Tree a -> b
foldT f b Empty = b
foldT f b (Node a t1 t2 t3) = f a (foldT f b t1) (foldT f b t2) (foldT f b t3)

countVertices :: Tree a -> Int
countVertices Empty = 0
countVertices (Node _ t1 t2 t3) = countVertices t1 + countVertices t2 + countVertices t3 + 1
              
countEdges :: Tree a -> Int
countEdges Empty = 0
countEdges (Node _ t1 t2 t3) = countEdges t1 + countEdges t2 + countEdges t3 + 3
           
height :: Tree a -> Int
height Empty = 0
height (Node _ t1 t2 t3) = height t1 `max` height t2 `max` height t3 + 1

countEmpty :: Tree a -> Int
countEmpty Empty = 1
countEmpty (Node _ t1 t2 t3) = countEmpty t1 + countEmpty t2 + countEmpty t3

-- Write countVertices, countEdges, height, and countEmpty using one call to foldT

-- TODO
{-
-- Compute the number of vertices in the tree using foldT along with a lambda function
-- that adds one to the result for every node encountered, with a base case of 0
-}
countVerticesF :: Tree a -> Int
countVerticesF = foldT (\a tl tm tr -> 1 + tl + tm + tr) 0

{-
-- Compute the number of edges in the tree using foldT along with a lambda function
-- that adds 3 to the result for every node encountered, with a base case of 0
-}
-- TODO
countEdgesF :: Tree a -> Int
countEdgesF = foldT (\a tl tm tr -> 3 + tl + tm + tr) 0

{-
-- Compute the height of the tree using foldT along with a lambda function 
-- that for every level of the tree, adds 1 to the result and adds the 
-- maximum of the left, middle, and right subtrees, with 0 as the base case
-}
-- TODO
heightF :: Tree a -> Int
heightF = foldT (\a hl hm hr -> 1 + hl `max` hm `max` hr) 0

{-
-- Compute the number of Empty nodes in the tree using foldT along with a lambda function
-- that adds the 3 child trees, with a base case of 1
-}
-- TODO
countEmptyF :: Tree a -> Int
countEmptyF = foldT (\a tl tm tr -> tl + tm + tr) 1

-- Use quickCheck to test that your version with foldT is equivalent to the original

-- Implement your quickCheck properties here ------

-- TODO
-- Define Arbitrary instance for our Tree
instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbitraryTree
    
arbitraryTree :: Arbitrary a => Int -> Gen (Tree a)
arbitraryTree size
  | size>0 = do a <- arbitrary
                l <- arbitraryTree (size `div` 2)
                m <- arbitraryTree (size `div` 2)
                r <- arbitraryTree (size `div` 2)
                return (Node a m l r)
  | otherwise = return Empty 

-- Property to check if the foldT implementation gives the same result as a recursive implementation
prop_countVerticesF_comparison :: Tree Int -> Bool
prop_countVerticesF_comparison t = countVerticesF t == countVertices t

-- Property to check if the foldT implementation gives the same result as a recursive implementation
prop_countEdgesF_comparison :: Tree Int -> Bool
prop_countEdgesF_comparison t = countEdgesF t == countEdges t

-- Property to check if the foldT implementation gives the same result as a recursive implementation
prop_heightF_comparison :: Tree Int -> Bool
prop_heightF_comparison t = heightF t == height t

-- Property to check if the foldT implementation gives the same result as a recursive implementation
prop_countEmptyF_comparison :: Tree Int -> Bool
prop_countEmptyF_comparison t = countEmptyF t == countEmpty t

-- Implementation of the concatenation function for the QuickCheck property below
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

-- Simple concatenation equality property. QuickCheck test for Proposition 1 to determine whether
-- reversing each list first or concatenating the lists and then reversing them has no effect on the result
prop_proposition1 :: [Int] -> [Int] -> Bool
prop_proposition1 xs ys = reverse (xs ++ ys) == reverse ys ++ reverse xs

-- Proofs:

{--

Given:

  [] ++ ys = ys (1)
  (x : xs) ++ ys = x : (xs ++ ys) (2)

  reverse [] = [] (3)
  reverse (x : xs) = reverse xs ++ [x] (4)

  Lemma: xs ++ [] = xs  (5)
  Lemma: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs (6)

Prove: reverse (xs ++ ys) = reverse ys ++ reverse xs

-}

-- TODO

-- Write your proof here.
{--
-- Before proving the proposition, we will be proving the two lemmas that we will need
   to prove our proposition

   Lemma: xs ++ [] = xs

-- We are going to prove this by structural induciton over (l=xs)

-- l ++ [] == l

-- Base case l = []

-- LHS : [] ++ [] {- By expansion of l -}
-- ==> [] {- By definition 1 -}

-- RHS: [] {-By expansion of l -}
-- Our base case holds since LHS = RHS !

----

-- Induction hypothesis l' = xs and assume that propostion holds for it
-- xs ++ [] = xs
-- Induction step l = (x:xs)
-- (x:xs) ++ [] ==  (x:xs) {- By expansion of l -}

-- ==> LHS: x : (xs ++ []) {- By definition 2 -}
-- ==> x:xs {- By induction hypothesis -} 

-- RHS: (x:xs)
-- Our induction step holds since LHS = RHS !

----------------------------------------

   Lemma: xs ++ (ys ++ zs) = (xs ++ ys) ++ zs

-- We are going to prove this by structural induciton over (l=xs)

-- l ++ (ys ++ zs) = (l ++ ys) ++ zs

-- Base case l = []

-- LHS : [] ++ (ys ++ zs) {- By expansion of l -}
-- ==> ys ++ zs {- By definition 1 -}

-- RHS: ([] ++ ys) ++ zs {-By expansion of l -}
-- ==> ys ++ zs {- By definition 1 -}
-- Our base case holds since LHS = RHS !

----

-- Induction hypothesis l' = xs and assume that propostion holds for it
-- xs ++ (ys ++ zs) = (xs ++ ys) ++ zs
-- Induction step l = (x:xs)
-- (x:xs) ++ (ys ++ zs) ==  ((x:xs) ++ ys) ++ zs {- By expansion of l -}

-- ==> LHS: x : (xs ++ (ys ++ zs)) {- By definition 2 -}
-- ==> x : (xs ++ ys) ++ zs {- By induction hypothesis -} 
-- ==> (x : (xs ++ ys)) ++ zs {- By definition 2 -} 
-- ==> ((x : xs) ++ ys) ++ zs {- By definition 2 -} 

-- RHS: ((x:xs) ++ ys) ++ zs
-- Our induction step holds since LHS = RHS !

------------------------------------------------------------
-- Propostion #1 : For all xs, ys  reverse (xs ++ ys) == reverse ys ++ reverse xs 

-- We are going to prove this by structural induciton over (l=xs)

-- reverse (l ++ ys) == reverse ys ++ reverse l

-- Base case l = []

-- LHS : reverse ([] ++ ys) {- By expansion of l -}
-- ==> reverse ys {- By definition 1 -}

-- RHS: reverse [] ++ reverse ys {-By expansion of l -}
-- ==> [] ++ reverse ys {- By definition 3 -}
-- ==> reverse ys {- By definition 1 -}
-- Our base case holds since LHS = RHS !

----

-- Induction hypothesis l' = xs and assume that propostion holds for it
-- reverse (xs ++ ys) = reverse ys ++ reverse xs
-- Induction step l = (x:xs)
-- reverse ((x:xs) ++ ys) ==  reverse ys ++ reverse (x:xs) {- By expansion of l -}

-- ==> LHS: reverse (x : (xs ++ ys)) {- By definition 2 -}
-- ==> reverse (xs ++ ys) ++ [x] {- By definition 4 -} 

-- RHS: reverse ys ++ reverse (x:xs)
-- => reverse ys ++ (reverse xs ++ [x]) {- By definition 4 -}
-- => (reverse ys ++ reverse xs) ++ [x] {- By definition 6 -}
-- => reverse (xs ++ ys) ++ [x] {- By induction hypothesis -}
-- Our induction step holds since LHS = RHS !
-}
-------------------------------------------------------

-- Lists; fmap; folds

average xs = sum xs / length xs
  where sum xs = foldr (+) 0 xs
        length xs = foldr (\_ r -> 1+r) 0 xs

-- TODO
{-
-- Computing the average of a list of numbers using a single foldr traversal of the list
-- which makes use of tuples as compound objects to combine the sum and length functions 
-- and return a result in a single traversal with foldr.
-}
average1 xs = sum / length
  where (sum, length) = foldr (\x (i, j) -> (x + i, j + 1)) (0, 0) xs

-- Challenge

-- TODO
{-
-- Implementation of foldl using foldr which makes use of the identity function
-- and a lambda function to take in the parameters and implement a foldl
-}
foldl f v xs = foldr (\x g v -> g (f v x)) id xs v