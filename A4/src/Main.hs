module Main where

import Control.Monad (void)
import Test.QuickCheck
import Test.Hspec
import A4

main:: IO()
main = do
  hspec $ describe "Unit Tests:" $ do
    specify "Unit test for `generateTrees`:" $ do
      A4.generateTrees 2 (\a -> Node (Leaf a) (Leaf a)) `shouldBe` [Node (Leaf 0) (Leaf 0),
                                                                    Node (Leaf 0) (Leaf 1),
                                                                    Node (Leaf 0) (Leaf 2),
                                                                    Node (Leaf 1) (Leaf 0),
                                                                    Node (Leaf 1) (Leaf 1),
                                                                    Node (Leaf 1) (Leaf 2),
                                                                    Node (Leaf 2) (Leaf 0),
                                                                    Node (Leaf 2) (Leaf 1),
                                                                    Node (Leaf 2) (Leaf 2)] 
    specify "Unit test for `removeNumbering`:" $ do
      A4.removeNumbering ntr `shouldBe` Node (Node (Leaf "cat") (Leaf "dog")) (Leaf "horse")
    specify "Unit test for `toBinary`:" $ do
      A4.toBinary itr `shouldBe` Node (Node (Leaf "1010") (Leaf "10100")) (Leaf "11110")
    specify "Unit test for `collectLeaves`:" $ do
      A4.collectLeaves itr `shouldBe` [10,20,30]
    specify "Unit test for `countLeaves`:" $ do
      A4.countLeaves itr `shouldBe` 3
    specify "Unit test for `collectMultiply`:" $ do
      A4.collectMultiply itr `shouldBe` ([30,20,10],Node (Node (Leaf 100) (Leaf 200)) (Leaf 300))
    specify "Unit test for `collectSegments`:" $ do
      A4.collectSegments itr `shouldBe` ([30,20,10],Node (Node (Leaf [10]) (Leaf [20,10])) (Leaf [30,20,10]))
    specify "Unit test for `numberLeaves`:" $ do
      A4.numberLeaves itr `shouldBe` Node (Node (Leaf (1,10)) (Leaf (2,20))) (Leaf (3,30))
    specify "Unit test for `generateTree`:" $ do
      A4.generateTree [1..] ptr `shouldBe` Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
  hspec $ describe "QuickCheck Properties implemented by student:" $ do
    specify "QuickCheck properties" $ property A4.allProps
    
  