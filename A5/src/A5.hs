{--
Michael Hammal
--}
module A5 where

import Control.Monad.State.Lazy
import Control.Monad.Reader
import Data.Maybe
import Control.Exception
todo = undefined

{--

Problem 1. 

Assume we have a state that holds the value of i. Specify the
semantics of ++i, i++, and i+1 using three expressions of type State
Int Int.

If your semantics is correct 

> sum $ evalState (traverse id [ppi, ipp, ppi, ppi, ipp, inci, ipp]) 0

should evaluate to 24
--}

ppi, ipp, inci :: State Int Int
ppi = do c <- get; put (c+1); return (c+1)
ipp = do c <- get; put (c+1); return c
inci = do c <- get; put c; return (c+1)

{-- 

Below is the tree definition we have been using in class and some
small example trees

--}

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show, Eq)

instance Functor Tree where 
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t1 t2) = Node (fmap f t1) (fmap f t2)
  
t1, t2, t3 :: Tree Int
t1 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
t2 = Node (Node (Leaf 10) (Leaf 20)) (Leaf 30)
t3 = Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 3)
t4 = Node t3 (Node t1 t2)
t5 :: Tree String
t5 = Node (Node (Leaf "a") (Leaf "b")) (Leaf "c")


{--

Problem 2. Write a function zipTree that takes two trees with the same
structure and produces a tree off pairs which the first component
comes from the first tree and the second component comes from the
second tree. If the tree structures do not match return Nothing.

Use the Maybe monad. 

--}

zipTree :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree (Leaf a) (Leaf b) = Just (Leaf (a,b))
zipTree (Leaf a) _ = Nothing
zipTree _ (Leaf b) = Nothing
zipTree (Node t1 t2) (Node t3 t4) = Just (Node (fromJust (zipTree t1 t3)) (fromJust (zipTree t2 t4)))
{--

Problem 3. Take a tree and return an annotated version where every
leaf is annotated with its depth. The depth of the root is 0 and the
depth increases at each level.

Use the Reader monad.

--}

depthAnnotate :: Tree a -> Reader Int (Tree (a,Int))
depthAnnotate (Leaf a) = do v <- ask; return (Leaf (a, v))
depthAnnotate (Node t1 t2) = 
  do 
    t1' <- local (+1) (do depthAnnotate t1)
    t2' <- local (+1) (do depthAnnotate t2)
    return (Node t1' t2')

{--

Problem 4. Take a tree and return an annotated version where each leaf
is annotated with a unique number. The leftmost leaf should have label
0 and the labels should increase from left to right.

Use the State monad.

--}

uniqueLabelAnnotate :: Tree a -> State Int (Tree (a,Int))
uniqueLabelAnnotate (Leaf a) = do c <- get; put (c+1); return (Leaf (a,c))
uniqueLabelAnnotate (Node t1 t2) =
  do 
    t1' <- uniqueLabelAnnotate t1
    t2' <- uniqueLabelAnnotate t2
    return (Node t1' t2') 

{--

Here is a small function that maintains a table of values of type a
and their count. When updateTable is called with a new value, it is
inserted in the table with a count of 1. If updateTable is called
again with the same value, its count is incremented.

--}

updateTable :: Eq a => [(a,Int)] -> a -> [(a,Int)]
updateTable [] a = [(a,1)]
updateTable ((b,n):table) a
  | a == b = (b,n+1) : table
  | otherwise = (b,n) : updateTable table a          

{--

Problem 5. Write a function that takes a tree and returns a table that
maintains the frequency of each value occuring in the tree.

Use the State monad

--}

frequency :: Eq a => Tree a -> State [(a,Int)] ()
frequency (Leaf a) = do c <- get; put (updateTable c a)
frequency (Node t1 t2) = 
  do 
    t1' <- frequency t1
    t2' <- frequency t2
    return ()

{-- 

Below are trees representing arithmetic expressions. The trees also
include an operation Catch that can be used to recover from errors
such as division by zero. The semantics of

  Catch e1 e2 

is to first evaluate e1. If that evaluation does not raise an error,
its value is returned. If the evaluation of e1 raises an error, then
e2 is evaluated instead.

--}

data Exp =
    Const Int
  | BinOp String Exp Exp
  | Catch Exp Exp 
  deriving Show

e1 n = BinOp "+" (BinOp "*" (Const 3) (Const 50)) (BinOp "/" (Const 10) (Const n))

e2 n = BinOp "+" (Const 1) $
       Catch
         (BinOp "+" (BinOp "*" (Const 3) (Const 50)) (BinOp "/" (Const 10) (Const n)))
         (Const 1000)

{--

A few pre-defined primitive operations for testing:

--}

apply :: String -> Int -> Int -> Int
apply "+" = (+)
apply "*" = (*)
apply "/" = div

{--

Problem 6. Write the function interp that takes an expression tree and returns
its value or an error.

Use the Maybe monad.

--}

interp :: Exp -> Maybe Int
interp (Const a) = Just a
interp (BinOp s e1 e2) = 
  do
    a <- interp e1
    b <- interp e2
    case s of
      "/" ->
        case b of 
          0 -> Nothing
          _ -> return ((apply s) a b)
      _ -> return ((apply s) a b)
interp (Catch e1 e2) = 
  case interp e1 of
    Nothing -> interp e2
    Just m -> Just m