module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (void)
import Test.QuickCheck
import Test.Hspec
import A6

main:: IO()
main = do
  hspec $ describe "Properties implemented by student:" $ do
    specify "\n" $ property A6.allProps
  hspec $ describe "Unit Tests:" $ do
    specify "mapLeaves:" $ do
      A6.mapLeaves (+4) tr1 `shouldBe` Node (Node (Leaf 5) (Leaf 6)) (Leaf 7)
    specify "checkTree:" $ do
      A6.checkTree (<100) tr1 `shouldBe` Right (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3))
      A6.checkTree (==1) tr1 `shouldBe` Left 2
    specify "swapLeaves:" $ do
      swapLeaves tr1 tr2 `shouldBe` (Node (Node (Leaf 10) (Leaf 20)) (Leaf 30), Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))
    specify "swapTreeList:" $ do
      swapTreeList tr1 [1000,2000,3000] `shouldBe` (Node (Node (Leaf 1000) (Leaf 2000)) (Leaf 3000),[1,2,3])
    specify "sumLeaves:" $ do
      sumLeaves tr1 `shouldBe` 6
    specify "accumTree:" $ do
      accumTree tr1 `shouldBe` Node (Node (Leaf 1) (Leaf 3)) (Leaf 6)
    specify "yieldEven:" $ do
      yieldEven tr3 `shouldBe` [2,2,10,20,30]
    specify "searchTree:" $ do
      searchTree [L] tr2 `shouldBe` 10
      searchTree [L,L] tr1 `shouldBe` 1
      searchTree [R] tr1 `shouldBe` 3
      searchTree [R,L] tr2`shouldBe` 20
    specify "leftMost:" $ do
      leftMost tr3 `shouldBe` 1