{--
Michael Hammal
--}
{-# LANGUAGE TemplateHaskell #-}
module A6 where

import Control.Monad.Cont
import Control.Monad (void)
import Test.QuickCheck
-- import the following to create QuickCheck properties
import Control.Monad.State.Lazy
import Control.Monad.State
import Data.Functor.Identity
--------------------------------------------------------------------------------
-- Traversable trees

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

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



tr1, tr2, tr3 :: Tree Int
tr1 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
tr2 = Node (Leaf 10) (Node (Leaf 20) (Leaf 30))
tr3 = Node tr1 (Node tr1 tr2)

instance Functor Tree where
  fmap f ( Leaf a) = Leaf (f a) 
  fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)

instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Node t1 t2) = foldr f (foldr f b t2) t1

instance Traversable Tree where
  traverse f (Leaf a) = pure Leaf <*> f a
  traverse f (Node t1 t2) = pure Node <*> traverse f t1 <*> traverse f t2

------------------------------------------------------------------
-- Yield monad
-- Wrapper over built-in continuation monad with special
-- iterator return type

data Iterator i o r =
    Result r
  | Susp o (i -> Iterator i o r)

toList :: Iterator () o () -> [o]
toList (Result _) = []
toList (Susp o k) = o : toList (k ())

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
-- Iterable structures

class Traversable t => Iterable t where
  iterator :: t o -> Iterator i o (t i)
  iterator os = runYield (traverse yield os)

instance Iterable []

instance Iterable Tree

--------------------------------------------------------------------------------
-- Traversing trees with Yield monad

todo = undefined

{--

This is a version of map using the tree iterator.
The iterator is fed into a loop that either sees a result or a
suspension. In the case of a result, it just returns that result.
Otherwise, it applies the function on the value of the suspension
and continues looping. 

*M> mapLeaves (+4) tr1
Node (Node (Leaf 5) (Leaf 6)) (Leaf 7)

--}

mapLeaves :: (o -> i) -> Tree o -> Tree i
mapLeaves f tr = loop (iterator tr)
  where loop (Result r) = r
        loop (Susp o k) = loop (k (f o))

{--

If all the leaves in the trees satisfy the given predicate, return the
tree unchanged. Otherwise, return the first node that does not satisfy
the predicate.

The iterator is fed into a loop that either sees a result or a
suspension. In the case of a result, it just returns that result with 
the Right (Either) wrapper. Otherwise, it tests the predicate on the 
value of the suspension and either continues looping or returns the value
with the Left (Either) wrapper and stops the loop. 

*M> checkTree (<100) tr1
Right (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3))
*M> checkTree (==1) tr1
Left 2

--}

checkTree :: (o -> Bool) -> Tree o -> Either o (Tree o)
checkTree f tr = loop (iterator tr)
  where loop (Result r) = Right r
        loop (Susp o k) = if (f o) then loop (k o) else Left o

{--

Takes two trees with the same number of leaves and swaps the leaves
leaving the tree structures unchanged.

Two iterators are fed into a loop for each tree that either sees two
results or two suspensions, assuming the trees depths are equal. 
In the case of a result, it just returns a tuple of the results.
Otherwise, it swaps the values of the suspensions and continues looping. 

*M> swapLeaves tr1 tr2
(Node (Node (Leaf 10) (Leaf 20)) (Leaf 30),
 Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))

--}

swapLeaves :: Tree o -> Tree o -> (Tree o, Tree o)
swapLeaves t1 t2 = loop (iterator t1) (iterator t2)
  where loop (Result r1) (Result r2) = (r1,r2)
        loop (Susp o1 k1) (Susp o2 k2) = loop (k1 o2) (k2 o1)

{--

Traverses a tree and a list and swaps the elements are they are encountered. 

Two iterators, one for a tree and one for a list, are fed into a loop 
that either sees two results or two suspensions, assuming the tree depth 
and the list length are equal.  In the case of a result, it just returns 
a tuple of the results. Otherwise, it swaps the values of the suspensions 
and continues looping. 

*M> swapTreeList tr1 [1000,2000,3000]
(Node (Node (Leaf 1000) (Leaf 2000)) (Leaf 3000),[1,2,3])

--}

swapTreeList :: Tree o -> [o] -> (Tree o, [o])
swapTreeList t1 t2 = loop (iterator t1) (iterator t2)
  where loop (Result r1) (Result r2) = (r1,r2)
        loop (Susp o1 k1) (Susp o2 k2) = loop (k1 o2) (k2 o1)

{--

Uses the tree iterator to sum the leaves

The iterator is fed into a loop that either sees a result or a
suspension. The loop is also fed a base value. In the case of a result, 
it just returns the base value that has been modified in the loop. 
Otherwise, it sums the base value and the value of the suspension 
continues looping with a new base value.

*M> sumLeaves tr1
6

--}

sumLeaves :: Tree Int -> Int
sumLeaves tr = loop 0 (iterator tr)
  where loop s (Result r) = s
        loop s (Susp o k) = loop (s+o) (k o)

{--

Generates a new tree where each leaf value is the sum of its value and
all the leaves before it in the traversal.

The iterator is fed into a loop that either sees a result or a
suspension. The loop is also fed a base value. In the case of a result, 
it just returns the result. Otherwise, it sums the base value and the 
value of the suspension continues looping with a new base value and a
new suspension value.

*M> accumTree tr1
Node (Node (Leaf 1) (Leaf 3)) (Leaf 6)

--}

accumTree :: Tree Int -> Tree Int
accumTree tr = loop 0 (iterator tr)
  where loop s (Result r) = r
        loop s (Susp o k) = loop (s+o) (k (s+o))

{--

Traverse a tree but only yield the even values.

Traverse a tree and in the case of a leaf, test the even predicate
on the value of the leaf. If the value is even, the value is passed on
to the yield function. In the case of a Node, we apply the same strategy
on the left child and the right child. The results are then passed to 
runYield to produce an iterator that can be passed to toList to produce
the final value. 

*M> yieldEven tr3
[2,2,10,20,30]

--}

yieldEven :: Tree Int -> [Int]
yieldEven tr = toList (runYield (f tr))
  where f :: Tree Int -> Yield () Int () ()
        f (Leaf a) = if even a then yield a else return ()
        f (Node t1 t2) = do t1' <- f t1
                            t2' <- f t2
                            return ()


{--

Traverses a tree, yielding every inner node. The traversal resumes
with the left or right subtree depending on the value used to resume
the search.

If we encounter a leaf, then we just return the leaf value.
If we encounter a node, we yield to get the value of the direction,
if that value is equal to L then we continue searching the left child,
otherwise we continue searching the right child. 

--}

data Dir = L | R deriving Eq

searchY :: Tree a -> Yield Dir () a a
searchY (Leaf a) = return a
searchY (Node t1 t2) = 
  do b <- yield ()
     if b == L then searchY t1 else searchY t2

{--

Takes a list of directions (L/R) that is guaranteed to reach a leaf
and returns the leaf at that position. 

The yield iterator is fed into a loop after searching the tree using
searchY. The loop is also fed a list of directions. It either sees a 
result or a suspension. In the case of a result, it just returns the 
result. Otherwise, it continues looping with the tail of the direction
list and and uses the head of the direction list as the suspension value
that will be used in searchY to traverse the tree when we yield.

*M> searchTree [L] tr2
10
*M> searchTree [L,L] tr1
1
*M> searchTree [R] tr1
3
*M> searchTree [R,L] tr2
20

--}

searchTree :: [Dir] -> Tree a -> a
searchTree ds tr = loop ds (runYield (searchY tr))
  where loop ds (Result r) = r 
        loop ds (Susp o k) = loop (tail ds) (k (head ds))

{--

Returns the leftmost node of an arbitrary tree.

leftMost uses a helper function to calculate the height of the left children
and produces a list of Left directions that are fed into the searchTree
function defined above to get the leftmost node.

*M> leftMost tr3
1

--}

leftMost :: Tree a -> a
leftMost tr = searchTree (replicate (heightLeft tr) L) tr
  where heightLeft (Leaf a) = 1
        heightLeft (Node t1 t2) = 1 + heightLeft t1
--------------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Implement your properties here.
-----------------------------------------------------------------------------
-- Property to check if mapLeaves implementation gives the same result as 
-- an fmap implementation.
prop_mapLeaves :: Tree Int -> Bool
prop_mapLeaves tr = mapLeaves (+1) tr == fmap (+1) tr

-- Property to check if mapLeaves implementation gives the same result as 
-- a traverse implementation with Identity.
prop_mapLeaves2 :: Tree Int -> Bool
prop_mapLeaves2 tr = mapLeaves (+1) tr == runIdentity (traverse (\a -> Identity (a+1)) tr)

-- Property to check if sumLeaves implementation gives the same result as 
-- an implementation with the built-in sum function.
prop_sumLeaves :: Tree Int -> Bool
prop_sumLeaves tr = sumLeaves tr == sum tr

-- Property to check if sumLeaves implementation gives the same result as 
-- a foldr implementation.
prop_sumLeaves2 :: Tree Int -> Bool
prop_sumLeaves2 tr = sumLeaves tr == foldr (+) 0 tr

-- Property to check if accumTree implementation gives the same result as 
-- a pattern matching implementation with the State Monad.
prop_accumTree :: Tree Int -> Bool 
prop_accumTree tr = accumTree tr == evalState (accumTreeState tr) 0

-- Function to generates a new tree where each leaf value is the sum of its 
-- value and all the leaves before it in the traversal.
accumTreeState :: Tree Int -> State Int (Tree Int)
accumTreeState (Leaf a) = do c <- get; put (c+a); return (Leaf (c+a))
accumTreeState (Node t1 t2) =
  do 
    t1' <- accumTreeState t1
    t2' <- accumTreeState t2
    return (Node t1' t2') 

-- Property to check if yieldEven implementation gives the same result as 
-- a regular pattern matching implementation. 
prop_yieldEven :: Tree Int -> Bool
prop_yieldEven tr = yieldEven tr == evenOnly tr
  where evenOnly (Leaf a) = if even a then [a] else []
        evenOnly (Node t1 t2) = evenOnly t1 ++ evenOnly t2
-----------------------------------------------------------------------------
-- End of property implementation.
-----------------------------------------------------------------------------
return []
allProps = void $quickCheckAll