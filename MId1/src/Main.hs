{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Main where

import Test.QuickCheck
import Test.Hspec
import Mid1

-- Insert function to implement test cases
insert :: a -> Mid1.Tree a -> Mid1.Tree a
insert a Mid1.Empty = Mid1.leaf a
insert b (Node a t1 t2 t3) = Node a t3 (insert b t1) t2

-- Generate a sample tree by definition and by Integer values
sample_leaf :: Mid1.Tree Int
sample_leaf = Mid1.leaf 1

sample_Tree_1 :: Mid1.Tree Int
sample_Tree_1 = insert 2 sample_leaf

sample_Tree_2 :: Mid1.Tree Int
sample_Tree_2 = insert 8 sample_Tree_1

sample_Tree_3 :: Mid1.Tree Int
sample_Tree_3 = insert 10 sample_Tree_2

sample_Tree_4 :: Mid1.Tree Int
sample_Tree_4 = insert 12 sample_Tree_3

sample_Tree_5 :: Mid1.Tree Int
sample_Tree_5 = insert 4 sample_Tree_4

sample_Tree_6 :: Mid1.Tree Int
sample_Tree_6 = insert 7 sample_Tree_5

sample_Tree_7 :: Mid1.Tree Int
sample_Tree_7 = insert 20 sample_Tree_6

sample_Tree_8 :: Mid1.Tree Int
sample_Tree_8 = insert 9 sample_Tree_7

-- example property
addition_quick_check_prop:: Int -> Int -> Bool
addition_quick_check_prop x y = x + y == y + x

-- Uncomment the following main function 
-- You need to write your specify cases yourself just like example of line 17
main:: IO()
main = do
  -- Prepare the examples
  putStrLn $ show $ sample_leaf
  putStrLn $ show $ sample_Tree_1
  putStrLn $ show $ sample_Tree_2
  putStrLn $ show $ sample_Tree_3
  putStrLn $ show $ sample_Tree_4
  putStrLn $ show $ sample_Tree_5
  putStrLn $ show $ sample_Tree_6
  putStrLn $ show $ sample_Tree_7
  putStrLn $ show $ sample_Tree_8  
  hspec $ describe "Unit Tests for the functions:" $ do
      specify "Unit tests for countVerticesF" $ do
        Mid1.countVerticesF sample_leaf `shouldBe` 1
        Mid1.countVerticesF sample_Tree_1 `shouldBe` 2
        Mid1.countVerticesF sample_Tree_2 `shouldBe` 3
        Mid1.countVerticesF sample_Tree_3 `shouldBe` 4
        Mid1.countVerticesF sample_Tree_4 `shouldBe` 5
        Mid1.countVerticesF sample_Tree_5 `shouldBe` 6
        Mid1.countVerticesF sample_Tree_6 `shouldBe` 7
        Mid1.countVerticesF sample_Tree_7 `shouldBe` 8
        Mid1.countVerticesF sample_Tree_8 `shouldBe` 9
      specify "Unit tests for countEdgesF" $ do
        Mid1.countEdgesF sample_leaf `shouldBe` 3
        Mid1.countEdgesF sample_Tree_1 `shouldBe` 6
        Mid1.countEdgesF sample_Tree_2 `shouldBe` 9
        Mid1.countEdgesF sample_Tree_3 `shouldBe` 12
        Mid1.countEdgesF sample_Tree_4 `shouldBe` 15
        Mid1.countEdgesF sample_Tree_5 `shouldBe` 18
        Mid1.countEdgesF sample_Tree_6 `shouldBe` 21
        Mid1.countEdgesF sample_Tree_7 `shouldBe` 24
        Mid1.countEdgesF sample_Tree_8 `shouldBe` 27
      specify "Unit tests for heightF" $ do
        Mid1.heightF sample_leaf `shouldBe` 1
        Mid1.heightF sample_Tree_1 `shouldBe` 2
        Mid1.heightF sample_Tree_2 `shouldBe` 2
        Mid1.heightF sample_Tree_3 `shouldBe` 2
        Mid1.heightF sample_Tree_4 `shouldBe` 3
        Mid1.heightF sample_Tree_5 `shouldBe` 3
        Mid1.heightF sample_Tree_6 `shouldBe` 3
        Mid1.heightF sample_Tree_7 `shouldBe` 3
        Mid1.heightF sample_Tree_8 `shouldBe` 3
      specify "Unit tests for countEmptyF" $ do
        Mid1.countEmptyF sample_leaf `shouldBe` 3
        Mid1.countEmptyF sample_Tree_1 `shouldBe` 5
        Mid1.countEmptyF sample_Tree_2 `shouldBe` 7
        Mid1.countEmptyF sample_Tree_3 `shouldBe` 9
        Mid1.countEmptyF sample_Tree_4 `shouldBe` 11
        Mid1.countEmptyF sample_Tree_5 `shouldBe` 13
        Mid1.countEmptyF sample_Tree_6 `shouldBe` 15
        Mid1.countEmptyF sample_Tree_7 `shouldBe` 17
        Mid1.countEmptyF sample_Tree_8 `shouldBe` 19
      specify "Unit tests for foldl" $ do
        Mid1.foldl (+) 0 [1..5] `shouldBe` sum [1..5]
        Mid1.foldl (*) 1 [1..5] `shouldBe` product [1..5]
        Mid1.foldl max 1 [1..5] `shouldBe` foldr max 1 [1..5]        
  hspec $ describe "QuickCheck Properties test:" $ do
    specify "Proposition 1 property quickCheck" $ do quickCheck prop_proposition1
    specify "countVerticesF property quickCheck" $ do quickCheck prop_countVerticesF_comparison
    specify "countEdgesF property quickCheck" $ do quickCheck prop_countEdgesF_comparison
    specify "heightF property quickCheck" $ do quickCheck prop_heightF_comparison
    specify "countEmptyF property quickCheck" $ do quickCheck prop_countEmptyF_comparison