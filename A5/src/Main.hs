module Main where


import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (void)
import Test.QuickCheck
import Test.Hspec
import A5

main:: IO()
main = do
  hspec $ describe "Unit Tests:" $ do
    specify "Problem 1:" $ do
      sum (evalState (traverse id [A5.ppi, A5.ipp, A5.ppi, A5.ppi, A5.ipp, A5.inci, A5.ipp]) 0) `shouldBe` 24
    specify "Problem 2:" $ do
      A5.zipTree A5.t1 A5.t2 `shouldBe` Just (Node (Node (Leaf (1,10)) (Leaf (2,20))) (Leaf (3,30)))
    specify "Problem 3:" $ do
      runReader (A5.depthAnnotate A5.t1) 0 `shouldBe` Node (Node (Leaf (1,2)) (Leaf (2,2))) (Leaf (3,1))
    specify "Problem 4:" $ do
      evalState (A5.uniqueLabelAnnotate A5.t2) 0 `shouldBe` Node (Node (Leaf (10,0)) (Leaf (20,1))) (Leaf (30,2))
    specify "Problem 5:" $ do
      execState  (A5.frequency A5.t3) [] `shouldBe` [(1,1),(2,1),(3,2)]
    specify "Problem 6:" $ do
      interp (A5.e1 1) `shouldBe` Just 160
      interp (e1 0) `shouldBe` Nothing
      interp (e2 0) `shouldBe` Just 1001
      interp (e2 1) `shouldBe` Just 161
