module A2 where

{--
Michael Hammal
--}

data Tree a =
     Empty
  |  Node a (Tree a) (Tree a)
  deriving (Eq,Show)

leaf :: a -> Tree a 
leaf a = Node a Empty Empty

insert :: a -> Tree a -> Tree a
insert a Empty = leaf a
insert b (Node a t1 t2) = Node a t2 (insert b t1)

-- TODO
-- Implement `Functor` instance based on our binary tree definition.
instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Node a t1 t2) = Node (f a) (fmap f t1) (fmap f t2) 

-- TODO
-- Implement `Foldable` instance based on our binary tree definition.
instance Foldable Tree where
  foldr f b Empty = b
  foldr f b (Node a t1 t2) = f a (foldr f (foldr f b t2) t1)

-- Implementing a fold function that preserves the structure of our binary tree.
-- TODO foldT
foldT :: (a -> b -> b -> b) -> b -> Tree a -> b
foldT f b Empty = b
foldT f b (Node a t1 t2) = f a (foldT f b t1) (foldT f b t2)  

{-
-- Compute the sum of all the nodes in the tree using foldr with the sum function,
-- with a base case of 0
-}
-- TODO sum
sum :: Tree Int -> Int
sum = foldr (+) 0

{-
-- Convert a tree to a list using foldr along with the cons operator to concatenate 
-- every node in the tree into a list with an empty list as the base case
-}
-- TODO toList
toList :: Tree a -> [a] 
toList = foldr (:) [] 

{-
-- Given a list, convert the list to a tree using foldr along with the insert 
-- function defined above with the Empty tree as the base case
-}
-- TODO fromList
fromList :: [a] -> Tree a
fromList = foldr insert Empty

{-
-- Compute the height of the tree using foldT along with a lambda function 
-- that for every level of the tree, adds 1 to the result and adds the 
-- maximum of the left and right subtrees,
-- with 0 as the base case
-}
-- TODO height
height :: Tree a -> Int
height = foldT (\a hl hr -> 1 + max hl hr) 0

{-
-- Compute the maximum sum along a path in the tree using foldT along with a 
-- lambda function that starting from the root, adds the node value to the 
-- result and then adds the maximum of the left and right subtrees, with 0 
-- as the base case
-}
-- TODO maxSum
maxSum :: Tree Int -> Int
maxSum = foldT (\a tl tr -> a + max tl tr) 0

{-
-- Compute the mirror tree using foldT along with a lambda function that 
-- switches the right and left subtrees with the Empty tree as the base case
-}
-- TODO mirror
mirror :: Tree a -> Tree a
mirror = foldT (\a tl tr -> Node a tr tl) Empty 

{-
-- Compute the number of nodes in the tree using foldr along with a lambda function
-- that adds one to the result for every node encountered, with a base case of 0
-}
-- TODO countAll
countAll :: Tree a -> Int
countAll = foldr (\_ n -> n+1) 0

{-
-- Compute the number of leaves in the tree using foldR along with a lambda function
-- that adds one to the result if subtrees are empty, and otherwise keeps going down 
-- the trees, with a base case of 0
-}
-- TODO countLeaves
countLeaves :: Tree a -> Int
countLeaves = foldT (\a tl tr -> if tl == 0 && tr == 0 then 1 else tl + tr) 0

{-
-- Similarly to above, collect all the leaves of the tree in a list using foldT along
-- with a lambda function that adds the node to the list ouput if the subtrees are empty,
-- and otherwise keeps going down the trees, with the empty list as the base case
-}
-- TODO collectLeaves 
collectLeaves :: Tree a -> [a]
collectLeaves = foldT (\a tl tr -> if null tl && null tr then [a] else tl ++ tr) []

{-
-- Convert the tree to a list using foldT along with a lambda function that adds the node 
-- element first and then the left subtree and then the right subtree last, performing 
-- a preorder traversal of the tree, with the empty list as the base case
-}
-- TODO preorder
preorder :: Tree a -> [a]
preorder = foldT (\a tl tr -> [a] ++ tl ++  tr) []

{-
-- Convert the tree to a list using foldT along with a lambda function that adds the left 
-- subtree first and then the node element and then the right subtree last, performing 
-- an inorder traversal of the tree, with the empty list as the base case
-}
-- TODO inorder
inorder :: Tree a -> [a]
inorder = foldT (\a tl tr -> tl ++ [a] ++ tr) []

{-
-- Convert the tree to a list using foldT along with a lambda function that adds the left 
-- subtree first and then the right subtree and then the node element last, performing 
-- a postorder traversal of the tree, with the empty list as the base case
-}
-- TODO postorder
postorder :: Tree a -> [a]
postorder = foldT (\a tl tr -> tl ++ tr ++ [a]) []