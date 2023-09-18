{--
Michael Hammal
--}
{-# LANGUAGE TemplateHaskell #-}
module A8 where
import Prelude hiding (fail)
import Control.Monad.Cont hiding (fail)
import Test.QuickCheck

------------------------------------------------------------------------------
-- Traversable search trees

data Tree a = Leaf a | Node [Tree a]
  deriving (Show, Eq)

-- Arbitrary instances for Tree
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    sized arbitrarySizedTree

arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
arbitrarySizedTree m = do
  t_0 <- arbitrary
  if m == 0
    then return (Leaf t_0)
    else do
      left <- arbitrarySizedTree (m `div` 4)
      right <- arbitrarySizedTree (m `div` 4)
      return (Node [left,right])  

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node ts) = Node (fmap (fmap f) ts)

instance Foldable Tree where
  foldr f b (Leaf a) = f a b
  foldr f b (Node ts) = foldr (\t b -> foldr f b t) b ts

instance Traversable Tree where
  traverse f (Leaf a) = fmap Leaf (f a)
  traverse f (Node ts) = pure Node <*> traverse (traverse f) ts

------------------------------------------------------------------------------
-- shift/reset view of continuations

runC :: Cont w w -> w
runC m = runCont m id

reset :: Cont a a -> Cont w a
reset = return . runC

shift :: ((a -> w) -> Cont w w) -> Cont w a
shift f = cont (runC . f)

------------------------------------------------------------------------------
-- Encoding choices using continuations with Tree answer type

chooseV :: [a] -> Cont (Tree w) a
chooseV as = shift $ \k -> return (Node (fmap k as))

chooseC :: [Cont (Tree w) a] -> Cont (Tree w) a
chooseC es = join $ chooseV es

fail :: Cont (Tree w) a
fail = chooseV []

------------------------------------------------------------------------------
-- Recover a tree from a continuation

reify :: Cont (Tree a) a -> Tree a
reify m = runC (fmap Leaf m)                           

------------------------------------------------------------------------------
-- Examples

e1,e2,e3,e4,e5,e6 :: Tree Int
e1 = reify $ chooseV [1,2,3]
e2 = reify $ fail
e3 = reify $ chooseC [ chooseV [1,2,3], fail ]
e4 = reify $ chooseC [ fail, chooseV [1,2,3] ]
e5 = reify $ chooseC [ chooseV [1,2,3], chooseV [10,20] ]
e6 = reify $ chooseC
               [ chooseC [ chooseV [1,2,3], chooseV [10,20] ]
               , chooseC [ chooseV [100,200,300,400 ], chooseV [-1,-2] ]
               , chooseV [ 1000,2000 ]
               ]

------------------------------------------------------------------------------
-- Problems

-- Problem I. Write a function 'leaves' that returns a list of
-- all the leaves in a tree.
-- Use foldr

{--

> leaves e1
[1,2,3]

> leaves e2
[]

> leaves e3
[1,2,3]

> leaves e4
[1,2,3]

> leaves e5
[1,2,3,10,20]

> leaves e6
[1,2,3,10,20,100,200,300,400,-1,-2,1000,2000]

--}

{--
leaves traverse a tree and returns a list of all the leaves
in the tree through foldr with the colon operator as the function
applied at each element of the tree and the emepty list as the
base case
--}
-- TODO
leaves :: Tree a -> [a]
leaves = foldr (:) []

-- Problem II. Write a function 'triples' that computes a tree of all
-- tuples (a,b,c) where a, b, c are in the range [1..10] and where a^2
-- + b^2 = c^2

{--

> leaves triples
[(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

--}

{--
triples makes use of the continuation monad to create a choice between
three triples with the range 0 to 10. Only triples which are Pythagorean
triples are chosen and added to as leafs of a tree.
--}
-- TODO
triples :: Tree (Int,Int,Int)
triples = reify $ do
      a <- chooseV [1..10]
      b <- chooseV [1..10]
      c <- chooseV [1..10]
      if a*a + b*b == c*c then return (a,b,c) else fail
------------------------------------------------------------------------------
-- Expressions denoting simple Int -> Int programs
-- https://okmij.org/ftp/continuations/index.html#reify-search

data Exp a =
    K a                            -- constant function
  | X                              -- identity
  | Exp a :+ Exp a                 -- \x -> f x + g x
  | Exp a :* Exp a                 -- \x -> f x * g x
 deriving (Show, Eq)

eval :: Exp Int -> Int -> Int
eval (K n) _ = n
eval X     n = n
eval (f :+ g) n = eval f n + eval g n
eval (f :* g) n = eval f n * eval g n

------------------------------------------------------------------
-- Examples

exp1, exp2, exp3 :: Tree (Exp Int)
exp1 = reify $ chooseV [ X, K 2, K 4 :+ X ]
exp2 = reify $ chooseC [ chooseV [ K 1, K 2, K 3 ], 
                         chooseV [ X, K 2 :+ X ] ]
exp3 = reify $ do
         exp <- chooseC [ chooseV [ K 1, K 2, K 3 ], 
                          chooseV [ X, K 2 :+ X ],
                          chooseV [ K 1 :* X, K 12, K 10 :+ X] ]
         if eval exp 10 == 12 then return exp else fail

------------------------------------------------------------------
-- Problems

-- Problem III. Write a function 'expsIn' that takes a range of
-- integers and generates a computation in the continuation monad that
-- generates all expressions (of type Exp Int) that are built from
-- that range of integers

{--
> take 10 $ leaves (reify (expsIn []))
[X,
 X :+ X,
 X :+ (X :+ X),
 X :+ (X :+ (X :+ X)),
 X :+ (X :+ (X :+ (X :+ X))),
 X :+ (X :+ (X :+ (X :+ (X :+ X)))),
 X :+ (X :+ (X :+ (X :+ (X :+ (X :+ X))))),
 X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ X)))))),
 X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ X))))))),
 X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ (X :+ X))))))))]

> take 10 $ leaves (reify (expsIn [1]))
[K 1,
 X,
 K 1 :+ K 1,
 K 1 :+ X,
 K 1 :+ (K 1 :+ K 1),
 K 1 :+ (K 1 :+ X),
 K 1 :+ (K 1 :+ (K 1 :+ K 1)),
 K 1 :+ (K 1 :+ (K 1 :+ X)),
 K 1 :+ (K 1 :+ (K 1 :+ (K 1 :+ K 1))),
 K 1 :+ (K 1 :+ (K 1 :+ (K 1 :+ X)))]

> take 10 $ leaves (reify (expsIn [1,2]))
[K 1,
 K 2,
 X,
 K 1 :+ K 1,
 K 1 :+ K 2,
 K 1 :+ X,
 K 1 :+ (K 1 :+ K 1),
 K 1 :+ (K 1 :+ K 2),
 K 1 :+ (K 1 :+ X),
 K 1 :+ (K 1 :+ (K 1 :+ K 1))]

> take 10 $ leaves (reify (expsIn [1,2,3]))
[K 1,
 K 2,
 K 3,
 X,
 K 1 :+ K 1,
 K 1 :+ K 2,
 K 1 :+ K 3,
 K 1 :+ X,
 K 1 :+ (K 1 :+ K 1),
 K 1 :+ (K 1 :+ K 2)]

--}
-- TODO
expsIn :: [Int] -> Cont (Tree w) (Exp Int)
expsIn range = chooseC
  [ chooseV (fmap K range)
  , return X
  , do x <- expsIn range
       y <- expsIn range
       return (x :+ y)
  , liftM2 (:*) (expsIn range) (expsIn range)
  ]

-- Problem IV. Write a function 'matchIn' that takes a range
-- of integers and a collection of pairs representing the desired
-- input/output behavior of a function and returns a tree of
-- expressions that match the desired functionality.

{--

> take 3 $ leaves (matchIn [] [])
[X,
 X :+ X,
 X :+ (X :+ X)]

> take 4 $ leaves (matchIn [0..2] [(0,0),(1,1),(2,2)])
[X,
 K 0 :+ X,
 K 0 :+ (K 0 :+ X),
 K 0 :+ (K 0 :+ (K 0 :+ X))]

--}
-- TODO
matchIn :: [Int] -> [(Int,Int)] -> Tree (Exp Int)
matchIn range io = reify $
  do 
    exp <- expsIn range
    if foldr (\(a,b) c -> (eval exp a == b) && c) True io
      then return exp else fail

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

------------------------------------------------------------------------------
-- Tree traversals

-- Problem V.
{--
traverseDY is a special traversal that yields each node, 
its depth, and its subtrees with pattern matching. A tree is fed 
into a loop that also takes a depth value. If a leaf is found, it
returns the value of the leaf. Otherwise, if it sees a node with a
list of trees it yields the depth and the list of trees and loops 
through the result.
--}
-- TODO
traverseDY :: Tree a -> Yield (Tree a,Int) (Int,[Tree a]) r a
traverseDY tr = loop 0 tr
  where
    loop depth (Leaf a) = return a
    loop depth (Node []) = do (t,d) <- yield (depth,[])
                              loop d t
    loop depth (Node ts) = do (t,d) <- yield (depth,ts)
                              loop d t

-- Problem VI.
{--
dfs performs a depth-first search on a given tree.
The yield iterator is fed into a loop after traversing the tree using
traverseY. The loop is also fed an empty listthat behaves like a stack 
to keep track of trees. The loop either sees a result or a suspension. 
In the case of a result, if the stack is empty it just returns the empty 
list, and if the stack is not empty it continues looping with the top 
element in the stack. Otherwise, it uses the colon operator to start 
building a list with the node value 'a' and the rest from looping with 
the left child encountered and it adds the children to the stack.
--}
-- TODO
dfs :: Tree a -> [a]
dfs tr = loop [] (runYield (traverseDY tr))
  where
    loop [] (Result r) = [r]
    loop ((k,t,d):ts) (Result r) = r : loop ts (k (t,d))
    loop ((k',t,d'):ts) (Susp (d, []) k) = loop ts (k' (t,d'))
    loop [] (Susp (d, []) k) = []
    loop ts (Susp (d, (l:ls)) k) = loop ((map (\a -> (k, a, d)) ls) ++ ts) (k (l,d))

{--

> take 4 $ dfs (matchIn [0..2] [(0,0),(1,1),(2,2)])
[X,K 0 :+ X,K 0 :+ (K 0 :+ X),K 0 :+ (K 0 :+ (K 0 :+ X))]

--}

-- Problem VII.
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
left child encountered and it adds the children to the stack.
--}
-- TODO
bfs :: Tree a -> [a]
bfs tr = loop [] (runYield (traverseDY tr))
  where
    loop [] (Result r) = [r]
    loop ((k,t,d):ts) (Result r) = r : loop ts (k (t,d))
    loop ((k',t,d'):ts) (Susp (d, []) k) = loop ts (k' (t,d'))
    loop [] (Susp (d, []) k) = []
    loop ((k',t,d'):ts) (Susp (d, ls) k) = loop (ts ++ (map (\a -> (k, a, d)) ls)) (k' (t,d'))
    loop ts (Susp (d, (l:ls)) k) = loop (ts ++ (map (\a -> (k, a, d)) ls)) (k (l,d))
{--

> take 4 $ bfs (matchIn [0..2] [(0,0),(1,1),(2,2)])
[X,K 0 :+ X,X :+ K 0,K 1 :* X]

--}

-- Problem VIII.
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
a list with the node value 'a' and the rest from looping with the children
encountered and it increments the depth by 1 while also adding the children
to the stack.
--}  
-- TODO
dls :: Int -> Tree a -> [a]
dls depth tr = loop [] (runYield (traverseDY tr))
  where
    loop [] (Result r) = [r]
    loop ((k,t,d):ts) (Result r) = r : loop ts (k (t,d))
    loop ((k',t,d'):ts) (Susp (d, []) k) = if d == depth then loop ts (k' (Node [], d')) else loop ts (k' (t,d'))
    loop [] (Susp (d, []) k) = []
    loop ts (Susp (d, (l:ls)) k) = if d == depth then loop ts (k (Node [], d)) else loop ((map (\a -> (k, a, d)) ls) ++ ts) (k (l,d+1))
  
{--
iterativeDeepening performs a limited depth-first search with a maximum depth
on a given tree. It works by taking advantage of the defined dls function above.
It takes in a tree and a desired maximum depth. It uses concat to combine the lists
created by a mapping of calls to dls on a list of numbers from 0 to the max depth.
--}
-- TODO
iterativeDeepening :: Int -> Tree a -> [a]
iterativeDeepening maxDepth tr = undefined
-- NOTE***: the following is the solution for iterativeDeepening
-- However, due to the implementation of matchIn, the program runs forever
--iterativeDeepening maxDepth tr = concat (map (\d -> dls d tr) [0..maxDepth])

{--

> take 1 $ iterativeDeepening 8 (matchIn [-2..2] [(0,1),(1,1),(2,3)])
[]

> take 1 $ iterativeDeepening 9 (matchIn [-2..2] [(0,1),(1,1),(2,3)])
[K 1 :+ (X :* (K (-1) :+ X))]

--}
------------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- QuickCheck properties
-----------------------------------------------------------------------------
-- Property to check if the foldr implementation gives the same result as a
-- pattern matching implementation
prop_leaves :: Tree Int -> Bool
prop_leaves tr = leaves tr == leaves2 tr
  where
    -- Helper function that returns a list of all the leaves in a tree with a 
    -- pattern matching implementation
    leaves2 (Leaf a) = [a]
    leaves2 (Node []) = []
    leaves2 (Node ts) = concatMap leaves2 ts

-- Property to check if the continuation monad implementation for triples 
-- gives the same result as a list comprehension implementation
prop_triples :: Bool
prop_triples = leaves triples == triples2
  where
    -- Helper function to generate Pythagorean triples with list comprehension
    triples2 = [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], (a^2) + (b^2) == (c^2)] 

-- Property to check if dfs implementation gives the same result as 
-- a pattern matching implementation.
prop_dfs2 :: Tree Int -> Bool
prop_dfs2 tr = dfs tr == dfs2 tr
  where 
    dfs2 (Leaf a) = [a]
    dfs2 (Node []) = []
    dfs2 (Node ts) = concatMap dfs2 ts

dfs3 :: Tree a -> [a]
dfs3 tr = dfsHelper [tr]
    where 
        dfsHelper :: [Tree a] -> [a]
        dfsHelper [] = []
        dfsHelper ((Leaf a):ts) = a : dfsHelper ts
        dfsHelper (Node l:ts) = dfsHelper l ++ dfsHelper ts

-- Property to check if dfs implementation gives the same result as 
-- a pattern matching implementation.
prop_dfs3 :: Tree Int -> Bool
prop_dfs3 tr = dfs tr == dfs3 tr

-- Helper function to perform bfs with pattern matching.
bfs2 :: Tree a -> [a]
bfs2 tr = bfsHelper [tr]
    where 
        bfsHelper :: [Tree a] -> [a]
        bfsHelper [] = []
        bfsHelper ((Leaf a):ts) = a : bfsHelper ts
        bfsHelper (Node l:ts) = bfsHelper (ts ++ l)

-- Property to check if bfs implementation gives the same result as 
-- a pattern matching implementation.
prop_bfs :: Tree Int -> Bool
prop_bfs tr = bfs tr == bfs2 tr
-----------------------------------------------------------------------------
-- End of QuickCheck properties
-----------------------------------------------------------------------------
return []
allProps = void $quickCheckAll