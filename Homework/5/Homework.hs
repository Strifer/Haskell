------------------------------------------------------------------------------
--- PROBLEM 1 ----------------------------------------------------------------
------------------------------------------------------------------------------
data Expr = Val Bool | And Expr Expr | Or Expr Expr | Not Expr
eval :: Expr -> Bool
eval (Val bool) = bool
eval (And e1 e2) = (eval e1) && (eval e2)
eval (Or e1 e2) = (eval e1) || (eval e2)
eval (Not e) = not (eval e)

expr = And (Or (Val True) (Not (Val True))) (Not (And (Val True) (Val False)))
------------------------------------------------------------------------------
--- PROBLEM 2 ----------------------------------------------------------------
------------------------------------------------------------------------------
data Tree a = Leaf | Node a (Tree a) (Tree a) deriving (Eq, Show)
testTree = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)
testTree2 = Node 1 (Node 2 Leaf Leaf) (Node 3 (Node 4 Leaf Leaf) Leaf)

treeFilter :: (a -> Bool) -> Tree a -> Tree a
treeFilter predicate Leaf = Leaf
treeFilter predicate (Node x left right)
    | predicate x = Node x (treeFilter predicate left) (treeFilter predicate right)
    | otherwise = Leaf

levelMap :: (Int -> a -> b) -> Tree a -> Tree b
levelMap f (Node x left right) = levelAccum f 0 (Node x left right)
    where levelAccum f depth (Node x left right) = Node (f depth x) (levelAccum f (depth+1) left) (levelAccum f (depth+1) right)
          levelAccum _ _ Leaf = Leaf

isSubtree :: Eq a => Tree a -> Tree a -> Bool
isSubtree Leaf _ = True
isSubtree _ Leaf = False
isSubtree subTree m@(Node x left right) = (subTree==m) || (isSubtree subTree left) || (isSubtree subTree right)
------------------------------------------------------------------------------
--- PROBLEM 3 ----------------------------------------------------------------
------------------------------------------------------------------------------
data DiffList a = DiffList {undiff :: ([a] -> [a])}
foo x = [1,2]++[3]++x
dl = DiffList {undiff = foo}

empty :: DiffList a
empty = DiffList (\xs -> xs)

fromList :: [a] -> DiffList a
fromList xs = DiffList (\b->xs++b)

toList :: DiffList a -> [a]
toList (DiffList a) = a []

append :: DiffList a -> DiffList a -> DiffList a
append (DiffList a1) (DiffList a2) = DiffList (a1 . a2) 

instance Monoid (DiffList a) where
    mempty  = empty
    mappend = (append)