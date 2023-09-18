module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad (void)
import Test.QuickCheck
import Test.Hspec
import A8
import Test.Hspec (specify)

main:: IO()
main = do
  hspec $ describe "Properties implemented by student:" $ do
     specify "\n" $ property A8.allProps
  hspec $ describe "Unit Tests:" $ do
    specify "Problem I - leaves:" $ do
      A8.leaves A8.e1 `shouldBe` [1,2,3]
      A8.leaves A8.e2 `shouldBe` []
      A8.leaves A8.e3 `shouldBe` [1,2,3]
      A8.leaves A8.e4 `shouldBe` [1,2,3]
      A8.leaves A8.e5 `shouldBe` [1,2,3,10,20]
      A8.leaves A8.e6 `shouldBe` [1,2,3,10,20,100,200,300,400,-1,-2,1000,2000]
    specify "Problem II: triples:" $ do
      A8.leaves A8.triples `shouldBe` [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]
    specify "Problem III: expsIn:" $ do
      take 10 (A8.leaves (A8.reify (A8.expsIn []))) `shouldBe` [X,
                                                                X :+ X,
                                                                X :+ (X :+ X),
                                                                X :+ (X :+ (X :+ X)),
                                                                X :+ (X :+ (X :+ (X :+ X))),
                                                                X :+ (X :+ (X :+ (X :+ (X :+ X)))),
                                                                X :+ (X :+ (X :+ (X :+ (X :+ (X :+ X))))),
                                                                X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ X)))))),
                                                                X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ X))))))),
                                                                X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ X))))))))]
      take 10 (A8.leaves (A8.reify (A8.expsIn [1]))) `shouldBe` [K 1,
                                                                 X,
                                                                 K 1 :+ K 1,
                                                                 K 1 :+ X,
                                                                 K 1 :+ (K 1 :+ K 1),
                                                                 K 1 :+ (K 1 :+ X),
                                                                 K 1 :+ (K 1 :+ (K 1 :+ K 1)),
                                                                 K 1 :+ (K 1 :+ (K 1 :+ X)),
                                                                 K 1 :+ (K 1 :+ (K 1 :+ (K 1 :+ K 1))),
                                                                 K 1 :+ (K 1 :+ (K 1 :+ (K 1 :+ X)))]
      take 10 (A8.leaves (A8.reify (A8.expsIn [1,2]))) `shouldBe` [K 1,
                                                                   K 2,
                                                                   X,
                                                                   K 1 :+ K 1,
                                                                   K 1 :+ K 2,
                                                                   K 1 :+ X,
                                                                   K 1 :+ (K 1 :+ K 1),
                                                                   K 1 :+ (K 1 :+ K 2),
                                                                   K 1 :+ (K 1 :+ X),
                                                                   K 1 :+ (K 1 :+ (K 1 :+ K 1))] 
      take 10 (A8.leaves (A8.reify (A8.expsIn [1,2,3]))) `shouldBe` [K 1,
                                                                     K 2,
                                                                     K 3,
                                                                     X,
                                                                     K 1 :+ K 1,
                                                                     K 1 :+ K 2,
                                                                     K 1 :+ K 3,
                                                                     K 1 :+ X,
                                                                     K 1 :+ (K 1 :+ K 1),
                                                                     K 1 :+ (K 1 :+ K 2)]                                                                                                                                                                          
    specify "Problem IV: matchIn" $ do
      take 3 (A8.leaves (A8.matchIn [] [])) `shouldBe` [X,
                                                        X :+ X,

                                                        X :+ (X :+ X)]
      take 4 (A8.leaves (A8.matchIn [0..2] [(0,0),(1,1),(2,2)])) `shouldBe` [X,
                                                                             K 0 :+ X,
                                                                             K 0 :+ (K 0 :+ X),
                                                                             K 0 :+ (K 0 :+ (K 0 :+ X))]                                                
    specify "Problem VI: dfs" $ do
      A8.dfs e1 `shouldBe` [1,2,3]
      A8.dfs e2 `shouldBe` []
      A8.dfs e3 `shouldBe` [1,2,3]
      A8.dfs e4 `shouldBe` [1,2,3]
      A8.dfs e5 `shouldBe` [1,2,3,10,20]
      A8.dfs e6 `shouldBe` [1,2,3,10,20,100,200,300,400,-1,-2,1000,2000]
    specify "Problem VII: bfs" $ do
      A8.bfs e1 `shouldBe` [1,2,3]
      A8.bfs e2 `shouldBe` []
      A8.bfs e3 `shouldBe` [1,2,3]
      A8.bfs e4 `shouldBe` [1,2,3]
      A8.bfs e5 `shouldBe` [1,2,3,10,20]
      A8.bfs e6 `shouldBe` [1000,2000,1,2,3,10,20,100,200,300,400,-1,-2]
    specify "Problem VIII: iterativeDeepening" $ do
      take 1 (A8.iterativeDeepening 8 (A8.matchIn [-2..2] [(0,1),(1,1),(2,3)])) `shouldBe` []
      take 1 (A8.iterativeDeepening 9 (A8.matchIn [-2..2] [(0,1),(1,1),(2,3)])) `shouldBe` [K 1 :+ (X :* (K (-1) :+ X))]