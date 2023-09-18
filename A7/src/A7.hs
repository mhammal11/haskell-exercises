{--
Michael Hammal
--}
{-# LANGUAGE TemplateHaskell #-}
module A7 where

import Control.Monad.Cont
import Control.Monad.Random
import System.Random.Shuffle
import Test.QuickCheck
import Control.Monad.Fail
import Test.QuickCheck (Arbitrary(arbitrary))

------------------------------------------------------------------
-- Iterator

data Iterator i o r =
    Result r
  | Susp o (i -> Iterator i o r)

------------------------------------------------------------------
-- Yield monad
-- Wrapper over built-in continuation monad with special
-- iterator return type

newtype Yield i o r a = Yield { unY :: Cont (Iterator i o r) a }

instance Functor (Yield i o r) where
  fmap f (Yield m) = Yield (fmap f m)

instance Applicative (Yield i o r) where
  pure a = Yield (pure a)
  (Yield mf) <*> (Yield ma) = Yield (mf <*> ma)

instance Monad (Yield i o r) where
  return a = Yield (return a)
  (Yield m) >>= k = Yield (m >>= \a -> unY (k a))

instance MonadCont (Yield i o r) where
  callCC c = Yield (callCC (\k -> unY (c (\a -> Yield (k a)))))

runYield :: Yield i o r r -> Iterator i o r
runYield (Yield m) = runCont m Result

yield :: o -> Yield i o r i
yield o = callCC (\k -> Yield (cont (\_ -> Susp o (\i -> runYield (k i)))))

--------------------------------------------------------------------------------
-- Traversable trees

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

-- Arbitrary instances for Tree
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
  t_0 <- arbitrary
  if m == 0
    then return (Node t_0 Empty Empty)
    else do
      left <- arbitrarySizedTree (m `div` 4)
      right <- arbitrarySizedTree (m `div` 4)
      return (Node t_0 left right)  

leaf :: a -> Tree a
leaf a = Node a Empty Empty

tr1, tr2, tr3 :: Tree Int
tr1 = Node 1 (leaf 2) (Node 3 (leaf 4) (leaf 5))
tr2 = Node 1 (Node 2 (Node 3 (Node 4 (leaf 5) (leaf 6)) (leaf 7)) (leaf 8)) (leaf 9)
tr3 = Node 10 tr1 (Node 20 tr1 tr2)

{--
tr2

                  1
              /       \
            2           9
         /     \
        3       8
     /    \
    4      7
   / \
  5   6

--}

--------------------------------------------------------------------------------
-- Problem I:
-- Write Functor, Foldable, and Traversable instances for Tree

-- TODO
instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

-- TODO
instance Foldable Tree where
  foldr f b Empty = b
  foldr f b (Node a l r) = foldr f (f a (foldr f b r)) l

-- TODO
instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Node a l r) = Node <$> f a <*> traverse f l <*> traverse f r

--------------------------------------------------------------------------------
-- Problem II:
-- Complete the definition of loop to implement depth-first search
-- > dfsDirect tr2
-- [1,2,3,4,5,6,7,8,9]

{--
dfsDirect performs a depth-first search on a given tree.
The yield iterator is fed into a loop after traversing the tree using
traverse. The loop either sees a result or a suspension. In the case of 
a result, it just returns the empty list. Otherwise, it uses the colon 
operator to start building a list with the node value 'a' and the rest
from looping with the next element being traversed.
--}
-- TODO
dfsDirect :: Tree a -> [a]
dfsDirect tr = loop (runYield (traverse yield tr))
  where loop (Result _) = []
        loop (Susp a k) = a : loop (k ())

--------------------------------------------------------------------------------
-- Problem III: 
-- Given the special traversal below that yields each node and its subtrees,
-- complete the definition of loop to implement depth-first search again.
-- Hint: the first argument to loop should behave like a stack
-- > dfs tr2
-- [1,2,3,4,5,6,7,8,9]

traverseY :: Tree a -> Yield (Tree a) (a,Tree a,Tree a) r ()
traverseY Empty = return ()
traverseY (Node a t1 t2) = do t <- yield (a, t1, t2); traverseY t

{--
dfs performs a depth-first search on a given tree.
The yield iterator is fed into a loop after traversing the tree using
traverseY. The loop is also fed an empty listthat behaves like a stack 
to keep track of trees. The loop either sees a result or a suspension. 
In the case of a result, if the stack is empty it just returns the empty 
list, and if the stack is not empty it continues looping with the top 
element in the stack. Otherwise, it uses the colon operator to start 
building a list with the node value 'a' and the rest from looping with 
the left child encountered and it adds the right child to the stack.
--}
-- TODO
dfs :: Tree a -> [a]
dfs tr = loop [] (runYield (traverseY tr))
  where
    loop [] (Result ()) = []
    loop ((t,k) : ts) (Result ()) = loop ts (k t) 
    loop ts (Susp (a,t1,t2) k) = a : loop ((t2,k) : ts) (k t1)

--------------------------------------------------------------------------------
-- Problem IV: 
-- Given the special traversal that yields each node and its subtrees,
-- complete the definition of loop to implement breadth-first search.
-- Hint: the first argument to loop should behave like a queue
-- > bfs tr2
-- [1,2,9,3,8,4,7,5,6]

{--
bfs performs a breadth-first search on a given tree.
The yield iterator is fed into a loop after traversing the tree using
traverseY. The loop is also fed an empty list that behaves like a stack 
to keep track of trees. The loop either sees a result or a suspension. 
In the case of a result, if the stack is empty it just returns the empty 
list, and if the stack is not empty it continues looping with the top 
element in the stack. Otherwise, if it sees a suspension and the stack is 
not empty, it uses the colon operator to start building a list with the node 
value 'a' and the rest from looping with the top element in the stack and it 
adds the left and right children to the stack. Lastly, if the loop sees a 
suspension and the stack is empty, it uses the colon operator to start 
building a list with the node value 'a' and the rest from looping with the 
left child encountered and it adds the right child to the stack.
--}
-- TODO
bfs :: Tree a -> [a]
bfs tr = loop [] (runYield (traverseY tr))
  where
    loop [] (Result ()) = []
    loop ((k,t):ts) (Result ()) = loop ts (k t)
    loop ((k1,t):ts) (Susp (a, t1, t2) k) = a : loop (ts ++ [(k, t1)] ++ [(k, t2)]) (k1 t)
    loop ts (Susp (a, t1, t2) k) = a : loop (ts ++ [(k, t2)]) (k t1)

--------------------------------------------------------------------------------
-- Problem V: 
-- Given the special traversal that yields each node and its subtrees,
-- complete the definition of loop to implement a random search. 
-- Hint: the first argument to loop should be shuffled before selecting
-- a tree to process.
-- Use shuffleM in System.Random.Shuffle
-- > randomSearch tr2
-- [1,2,8,9,3,4,6,7,5]

{--
randomSearch performs a random search on a given tree.
The yield iterator is fed into a loop after traversing the tree using
traverseY. The loop is also fed an empty list that behaves like a stack 
to keep track of trees. The loop either sees a result or a suspension. 
In the case of a result, if the stack is empty it just returns the empty 
list, and if the stack is not empty it shuffles the stack and then it 
continues looping with the top element in the stack. 'return' is used to 
encapsulate the result with the monad. Otherwise, if it sees a suspension 
and the stack is not empty, it shuffles the stack and then it continues 
looping with the top element in the stack and it adds the left and right 
children to the stack. Then it uses the colon operator to start building a
list with the node value 'a' and the rest from the looping done in the previous
step. Lastly, if the loop sees a suspension and the stack is empty, it continues
looping with the left child encountered and it adds the right child to the 
stack. Again, it then uses the colon operator to start building a list with the 
node value 'a' and the rest from the looping done in the previous step. 
--}
-- TODO
randomSearch :: (MonadFail m, MonadRandom m) => Tree a -> m [a]
randomSearch tr = loop [] (runYield (traverseY tr))
  where
    loop [] (Result ()) = return []
    loop ts (Result ()) = do
      ((k,t):ts') <- shuffleM ts
      val <- loop ts' (k t)
      return val
    loop ((k1,t):ts) (Susp (a, t1, t2) k) = do
        ((k',t'):ts') <- shuffleM ((k1,t):ts)
        val <- loop (ts' ++ [(k, t1)] ++ [(k, t2)]) (k' t')
        return (a : val)
    loop ts (Susp (a, t1, t2) k) = do
        val <- loop ((k, t2):ts) (k t1)
        return (a : val)

--------------------------------------------------------------------------------
-- Problem VI: 
-- Given the special traversal below that yields each node, its depth, and its subtrees,
-- complete the definition of loop to implement a depth-first search with limited depth
-- Hint: this should be similar to dfs but terminates when the maximum depth is reached
-- > dls 0 tr2
-- [1]
-- > dls 1 tr2
-- [1,2,9]
-- > dls 2 tr2
-- [1,2,3,8,9]
-- > dls 3 tr2
-- [1,2,3,4,7,8,9]
-- > dls 4 tr2
-- [1,2,3,4,5,6,7,8,9]
-- > dls 5 tr2
-- [1,2,3,4,5,6,7,8,9]
-- > dls 100 tr2
-- [1,2,3,4,5,6,7,8,9]

traverseDY :: Tree a -> Yield (Tree a,Int) (a,Int,Tree a,Tree a) r ()
traverseDY tr = loop 0 tr
  where
    loop depth Empty = return ()
    loop depth (Node a t1 t2) = do (t,d) <- yield (a,depth,t1,t2)
                                   loop d t

{--
dls performs a depth-first search with limited depth on a given tree.
The yield iterator is fed into a loop after traversing the tree using
traverseDY. The loop is also fed an empty list that behaves like a stack 
to keep track of trees. The loop either sees a result or a suspension. 
In the case of a result, if the stack is empty it just returns the empty 
list, and if the stack is not empty it continues looping with the top 
element in the stack. Otherwise, it checks if the desired depth is reached 
and if so it uses the colon operator to start building a list with the node 
value 'a' and the rest from looping with the Empty element so that the next 
iteration sees a Result and it starts going through the trees in the stack.
If the depth is not yet reached, it uses the colon operator to start building 
a list with the node value 'a' and the rest from looping with the left child 
encountered and it increments the depth by 1 while also adding the right child 
to the stack.
--}                                  
-- TODO
dls :: Int -> Tree a -> [a]
dls depth tr = loop [] (runYield (traverseDY tr))
  where
    loop [] (Result ()) = []
    loop ((k,t,d):ts) (Result ()) = loop ts (k (t,d))
    loop ts (Susp (a, d, t1, t2) k) = if d == depth then a : loop ts (k (Empty, d)) else a : loop ((k, t2, d):ts) (k (t1,d+1))

--------------------------------------------------------------------------------
-- Problem VII:
-- Complete the definition of iterative deepening. The idea is to perform
-- a limited depth-first search with maximum depth 0, 1, ... maxDepth
-- > iterativeDeepening 3 tr2
-- [1,1,2,9,1,2,3,8,9,1,2,3,4,7,8,9]

{--
iterativeDeepening performs a limited depth-first search with a maximum depth
on a given tree.It works by taking advantage of the defined dls function above.
It takes in a tree and a desired maximum depth. If the maximum depth is 0, then
dls is called with that depth and the tree. Otherwise, a recursive call is made to
iterativeDeepening with the maximum depth decrement by one and another call is made
to dls with the maximum depth and the tree. Both these calls will output a list of 
tree node values and so they are concatenated to output a final list.
--}
-- TODO
iterativeDeepening :: Int -> Tree a -> [a]
iterativeDeepening maxDepth tr = if maxDepth == 0 then dls maxDepth tr else (iterativeDeepening (maxDepth - 1) tr) ++ (dls maxDepth tr)
--------------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Implement your properties here.
-----------------------------------------------------------------------------
-- Helper function to perfrom a fold on a tree.
foldT :: (a -> b -> b -> b) -> b -> Tree a -> b
foldT f b Empty = b
foldT f b (Node a t1 t2) = f a (foldT f b t1) (foldT f b t2)  

-- Property to check if dfsDirect implementation gives the same result as 
-- an fold implementation.
prop_dfsDirectFold :: Tree Int -> Bool
prop_dfsDirectFold tr = dfsDirect tr == foldT (\a tl tr -> [a] ++ tl ++ tr) [] tr

-- Property to check if dfs implementation gives the same result as 
-- an fold implementation.
prop_dfsFold :: Tree Int -> Bool
prop_dfsFold tr = dfs tr == foldT (\a tl tr -> [a] ++ tl ++ tr) [] tr

-- Property to check if dfs and dfsDriect give the same result.
prop_dfs :: Tree Int -> Bool
prop_dfs tr = dfs tr == dfsDirect tr

-- Helper function to perform dfs with pattern matching.
dfs2 :: Tree a -> [a]
dfs2 Empty = []
dfs2 (Node a Empty Empty) = [a]
dfs2 (Node a t1 t2) = [a] ++ dfs2 t1 ++ dfs2 t2

-- Property to check if dfsDirect implementation gives the same result as 
-- a pattern matching implementation.
prop_dfsDirect2 :: Tree Int -> Bool
prop_dfsDirect2 tr = dfsDirect tr == dfs2 tr

-- Property to check if dfs implementation gives the same result as 
-- a pattern matching implementation.
prop_dfs2 :: Tree Int -> Bool
prop_dfs2 tr = dfs tr == dfs2 tr

-- Helper function to perform bfs with pattern matching.
bfs2 :: Tree a -> [a]
bfs2 tr = bfsHelper [tr]
    where 
        bfsHelper :: [Tree a] -> [a]
        bfsHelper [] = []
        bfsHelper (Empty:ts) = bfsHelper ts
        bfsHelper (Node a t1 t2:ts) = a : bfsHelper (ts ++ [t1,t2])

-- Property to check if bfs implementation gives the same result as 
-- a pattern matching implementation.
prop_bfs :: Tree Int -> Bool
prop_bfs tr = bfs tr == bfs2 tr
-----------------------------------------------------------------------------
-- End of property implementation.
-----------------------------------------------------------------------------
return []
allProps = void $quickCheckAll