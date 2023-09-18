{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (void)
import Test.QuickCheck
import Test.Hspec
import A2


-- Implement your properties here --


-- End of property implementatoin block --

-- Generate a sample tree by definition and by Integer values
sample_leaf :: A2.Tree Int
sample_leaf = A2.leaf 1

sample_Tree_1 :: A2.Tree Int
sample_Tree_1 = A2.insert 2 sample_leaf

sample_Tree_2 :: A2.Tree Int
sample_Tree_2 = A2.insert 8 sample_Tree_1

sample_Tree_3 :: A2.Tree Int
sample_Tree_3 = A2.insert 10 sample_Tree_2

sample_Tree_4 :: A2.Tree Int
sample_Tree_4 = A2.insert 12 sample_Tree_3

sample_Tree_5 :: A2.Tree Int
sample_Tree_5 = A2.insert 4 sample_Tree_4

sample_Tree_6 :: A2.Tree Int
sample_Tree_6 = A2.insert 7 sample_Tree_5

sample_Tree_7 :: A2.Tree Int
sample_Tree_7 = A2.insert 20 sample_Tree_6

sample_Tree_8 :: A2.Tree Int
sample_Tree_8 = A2.insert 9 sample_Tree_7

-- Start a tree

sample_list = [5,7,8,9,12,45,67,96,45,33,56,18,22]


return []

allProps = void $quickCheckAll

main:: IO()
main = do
  -- Prepaire the examples
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
    specify "Unit tests for sum" $ do
      A2.sum sample_leaf `shouldBe` 1
      A2.sum sample_Tree_1 `shouldBe` 3
      A2.sum sample_Tree_2 `shouldBe` 11
      A2.sum sample_Tree_3 `shouldBe` 21
      A2.sum sample_Tree_4 `shouldBe` 33
      A2.sum sample_Tree_5 `shouldBe` 37
      A2.sum sample_Tree_6 `shouldBe` 44
      A2.sum sample_Tree_7 `shouldBe` 64
      A2.sum sample_Tree_8 `shouldBe` 73
    specify "Unit tests for toList" $ do
      A2.toList sample_leaf `shouldBe` [1]
      A2.toList sample_Tree_1 `shouldBe` [1,2]
      A2.toList sample_Tree_2 `shouldBe` [1,2,8]
      A2.toList sample_Tree_3 `shouldBe` [1,8,2,10]
      A2.toList sample_Tree_4 `shouldBe` [1,2,10,8,12]
      A2.toList sample_Tree_5 `shouldBe` [1,8,12,2,10,4]
      A2.toList sample_Tree_6 `shouldBe` [1,2,10,4,8,12,7]
      A2.toList sample_Tree_7 `shouldBe` [1,8,12,7,2,4,10,20]
      A2.toList sample_Tree_8 `shouldBe` [1,2,4,10,20,8,7,12,9]
    specify "Unit tests for fromList" $ do
      A2.fromList sample_list `shouldBe` Node 22 (Node 18 (Node 96 Empty (Node 9 Empty Empty)) (Node 33 (Node 45 Empty Empty) (Node 7 Empty Empty))) (Node 56 (Node 67 Empty (Node 8 Empty Empty)) (Node 45 (Node 12 Empty Empty) (Node 5 Empty Empty)))
    specify "Unit tests for height" $ do
      A2.height sample_leaf `shouldBe` 1
      A2.height sample_Tree_1 `shouldBe` 2
      A2.height sample_Tree_2 `shouldBe` 2
      A2.height sample_Tree_3 `shouldBe` 3
      A2.height sample_Tree_4 `shouldBe` 3
      A2.height sample_Tree_5 `shouldBe` 3
      A2.height sample_Tree_6 `shouldBe` 3
      A2.height sample_Tree_7 `shouldBe` 4
      A2.height sample_Tree_8 `shouldBe` 4
    specify "Unit tests for maxSum" $ do
      A2.maxSum sample_leaf `shouldBe` 1
      A2.maxSum sample_Tree_1 `shouldBe` 3
      A2.maxSum sample_Tree_2 `shouldBe` 9
      A2.maxSum sample_Tree_3 `shouldBe` 13
      A2.maxSum sample_Tree_4 `shouldBe` 21
      A2.maxSum sample_Tree_5 `shouldBe` 21
      A2.maxSum sample_Tree_6 `shouldBe` 21
      A2.maxSum sample_Tree_7 `shouldBe` 33
      A2.maxSum sample_Tree_8 `shouldBe` 33
    specify "Unit tests for mirror" $ do
      A2.mirror sample_leaf `shouldBe` Node 1 Empty Empty
      A2.mirror sample_Tree_1 `shouldBe` Node 1 (Node 2 Empty Empty) Empty
      A2.mirror sample_Tree_2 `shouldBe` Node 1 (Node 8 Empty Empty) (Node 2 Empty Empty)
      A2.mirror sample_Tree_3 `shouldBe` Node 1 (Node 2 (Node 10 Empty Empty) Empty) (Node 8 Empty Empty)
      A2.mirror sample_Tree_4 `shouldBe` Node 1 (Node 8 (Node 12 Empty Empty) Empty) (Node 2 (Node 10 Empty Empty) Empty)
      A2.mirror sample_Tree_5 `shouldBe` Node 1 (Node 2 (Node 4 Empty Empty) (Node 10 Empty Empty)) (Node 8 (Node 12 Empty Empty) Empty)
      A2.mirror sample_Tree_6 `shouldBe` Node 1 (Node 8 (Node 7 Empty Empty) (Node 12 Empty Empty)) (Node 2 (Node 4 Empty Empty) (Node 10 Empty Empty))
      A2.mirror sample_Tree_7 `shouldBe` Node 1 (Node 2 (Node 10 (Node 20 Empty Empty) Empty) (Node 4 Empty Empty)) (Node 8 (Node 7 Empty Empty) (Node 12 Empty Empty))
      A2.mirror sample_Tree_8 `shouldBe` Node 1 (Node 8 (Node 12 (Node 9 Empty Empty) Empty) (Node 7 Empty Empty)) (Node 2 (Node 10 (Node 20 Empty Empty) Empty) (Node 4 Empty Empty))
    specify "Unit tests for countAll" $ do
      A2.countAll sample_leaf `shouldBe` 1
      A2.countAll sample_Tree_1 `shouldBe` 2
      A2.countAll sample_Tree_2 `shouldBe` 3
      A2.countAll sample_Tree_3 `shouldBe` 4
      A2.countAll sample_Tree_4 `shouldBe` 5
      A2.countAll sample_Tree_5 `shouldBe` 6
      A2.countAll sample_Tree_6 `shouldBe` 7
      A2.countAll sample_Tree_7 `shouldBe` 8
      A2.countAll sample_Tree_8 `shouldBe` 9
    specify "Unit tests for countLeaves" $ do
      A2.countLeaves sample_leaf `shouldBe` 1
      A2.countLeaves sample_Tree_1 `shouldBe` 1
      A2.countLeaves sample_Tree_2 `shouldBe` 2
      A2.countLeaves sample_Tree_3 `shouldBe` 2
      A2.countLeaves sample_Tree_4 `shouldBe` 2
      A2.countLeaves sample_Tree_5 `shouldBe` 3
      A2.countLeaves sample_Tree_6 `shouldBe` 4
      A2.countLeaves sample_Tree_7 `shouldBe` 4
      A2.countLeaves sample_Tree_8 `shouldBe` 4
    specify "Unit tests for collectLeaves" $ do
      A2.collectLeaves sample_leaf `shouldBe` [1]
      A2.collectLeaves sample_Tree_1 `shouldBe` [2]
      A2.collectLeaves sample_Tree_2 `shouldBe` [2,8]
      A2.collectLeaves sample_Tree_3 `shouldBe` [8,10]
      A2.collectLeaves sample_Tree_4 `shouldBe` [10,12]
      A2.collectLeaves sample_Tree_5 `shouldBe` [12,10,4]
      A2.collectLeaves sample_Tree_6 `shouldBe` [10,4,12,7]
      A2.collectLeaves sample_Tree_7 `shouldBe` [12,7,4,20]
      A2.collectLeaves sample_Tree_8 `shouldBe` [4,20,7,9]
    specify "Unit tests for preorder" $ do
      A2.preorder sample_leaf `shouldBe` [1]
      A2.preorder sample_Tree_1 `shouldBe` [1, 2]
      A2.preorder sample_Tree_2 `shouldBe` [1,2,8]
      A2.preorder sample_Tree_3 `shouldBe` [1,8,2,10]
      A2.preorder sample_Tree_4 `shouldBe` [1,2,10,8,12]
      A2.preorder sample_Tree_5 `shouldBe` [1,8,12,2,10,4]
      A2.preorder sample_Tree_6 `shouldBe` [1,2,10,4,8,12,7]
      A2.preorder sample_Tree_7 `shouldBe` [1,8,12,7,2,4,10,20]
      A2.preorder sample_Tree_8 `shouldBe` [1,2,4,10,20,8,7,12,9]
    specify "Unit tests for inorder" $ do
      A2.inorder sample_leaf `shouldBe` [1]
      A2.inorder sample_Tree_1 `shouldBe` [1, 2]
      A2.inorder sample_Tree_2 `shouldBe` [2,1,8]
      A2.inorder sample_Tree_3 `shouldBe` [8,1,2,10]
      A2.inorder sample_Tree_4 `shouldBe` [2,10,1,8,12]
      A2.inorder sample_Tree_5 `shouldBe` [8,12,1,10,2,4]
      A2.inorder sample_Tree_6 `shouldBe` [10,2,4,1,12,8,7]
      A2.inorder sample_Tree_7 `shouldBe` [12,8,7,1,4,2,10,20]
      A2.inorder sample_Tree_8 `shouldBe` [4,2,10,20,1,7,8,12,9]
    specify "Unit tests for postorder" $ do
      A2.postorder sample_leaf `shouldBe` [1]
      A2.postorder sample_Tree_1 `shouldBe` [2,1]
      A2.postorder sample_Tree_2 `shouldBe` [2,8,1]
      A2.postorder sample_Tree_3 `shouldBe` [8,10,2,1]
      A2.postorder sample_Tree_4 `shouldBe` [10,2,12,8,1]
      A2.postorder sample_Tree_5 `shouldBe` [12,8,10,4,2,1]
      A2.postorder sample_Tree_6 `shouldBe` [10,4,2,12,7,8,1]
      A2.postorder sample_Tree_7 `shouldBe` [12,7,8,4,20,10,2,1]
      A2.postorder sample_Tree_8 `shouldBe` [4,20,10,2,7,9,12,8,1]
  hspec $ describe "QuickCheck Properties test:" $ do
    specify "QuickCheck properties" $ property allProps
    
  