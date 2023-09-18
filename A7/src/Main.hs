module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (void)
import Test.QuickCheck
import Test.Hspec
import A7
import Test.Hspec (specify)

main:: IO()
main = do
  hspec $ describe "Properties implemented by student:" $ do
    specify "\n" $ property A7.allProps
  hspec $ describe "Unit Tests:" $ do
    specify "Problem II - depth-first search:" $ do
      A7.dfsDirect A7.tr2 `shouldBe`  [1,2,3,4,5,6,7,8,9]
    specify "Problem III: depth-first search" $ do
      A7.dfs A7.tr2 `shouldBe` [1,2,3,4,5,6,7,8,9]
    specify "Problem IV: bfs" $ do
      A7.bfs tr2 `shouldBe` [1,2,9,3,8,4,7,5,6]
    specify "Problem VI: dls" $ do
      A7.dls 0 A7.tr2 `shouldBe` [1]
      A7.dls 1 A7.tr2 `shouldBe` [1,2,9]
      A7.dls 2 A7.tr2 `shouldBe` [1,2,3,8,9]
      A7.dls 3 A7.tr2 `shouldBe` [1,2,3,4,7,8,9]
      A7.dls 4 A7.tr2 `shouldBe` [1,2,3,4,5,6,7,8,9]
      A7.dls 5 A7.tr2 `shouldBe` [1,2,3,4,5,6,7,8,9]
      A7.dls 100 A7.tr2 `shouldBe` [1,2,3,4,5,6,7,8,9]
    specify "Problem VII: iterativeDeepening" $ do
      A7.iterativeDeepening 3 tr2 `shouldBe` [1,1,2,9,1,2,3,8,9,1,2,3,4,7,8,9]