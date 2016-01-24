import Data.Char
import Data.Functor
import Data.Foldable
import Prelude hiding (foldr,foldl,foldr1)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Tree as T

--- INTRO -----------------------------------------------------------------------------------------
-- So we supposedly learned all about recursive data types, lists and tress.
-- Today we look into custom type classes, which is a way of defining interfaces for data types.
-- We use type classes when we want to capture similar operations over different structures.

-- This is different from polymorphism which captures cases of similar structures over different values.

--- CUSTOM TYPE CLASSES ---------------------------------------------------------------------------

-- Remember how the person data type went?

data Sex = Male | Female deriving (Show, Read, Eq, Ord)
data Person = Person {
    idNumber :: String,
    forename :: String,
    surname  :: String,
    sex      :: Sex,
    age      :: Int,
    partner  :: Maybe Person,
    children :: [Person]
} deriving (Show, Read, Eq, Ord)

-- So far we've only been defining data types, type constructors and type instances.
-- Now it's time to do our very own type class.

class Ageing a where
    currentAge  :: a -> Int
    maxAge      :: a -> Int
    makeOlder   :: a -> a

-- A 'Person' can be an instance of this type class.
instance Ageing Person where
    currentAge = age
    makeOlder p = p { age = age p +1}
    maxAge    _ = 123

--- Max age doesn't depend on any value, it is the same for all values of 'Person'.

-- MOAR TYPES

data Breed = Beagle | Husky | Pekingese deriving (Eq, Ord, Show , Read)
data Dog = Dog {
    dogName  :: String,
    dogBreed :: Breed,
    dogAge   :: Int
    } deriving (Eq, Ord, Show, Read)

-- Let's make breeds an instance of our custom type class
instance Ageing Dog where
    currentAge  = dogAge
    makeOlder d = d {dogAge = dogAge d + 1}
    maxAge d    = case dogBreed d of
                    Husky -> 29
                    _     -> 20

-- We can now define a function that can be applied to all values of our custom type class.

veryOld :: Ageing a => a -> Bool
veryOld x = 10 * currentAge x >= 8 * maxAge x

toto = Dog "Toto" Beagle 16
ines = Person "2692" "Ines" "Seni" Female 16 Nothing []
dado = Person "2692" "Dado" "Dadić" Male 105 Nothing []

b1 = veryOld toto
b2 = veryOld ines
b3 = veryOld dado
---------------------------------------------------------------------------------------------------
--- EXERCISE 1
---------------------------------------------------------------------------------------------------
-- 1.1
-- Compares the relative ages to maximum ages, for example a 10 year old dog is older than a 20 year old human.
compareRelativeAge :: (Ageing a, Ageing b) => a -> b -> Ordering
compareRelativeAge a b = compare (distanceToMaxAge a) (distanceToMaxAge b)
    where distanceToMaxAge x = maxAge x - currentAge x

-- 1.2
-- This type class defines a naming function.
class Nameable a where
    name :: a -> String

instance Nameable Dog where
    name d = dogName d ++ " the Dog"

instance Nameable Person where
    name p = forename p ++ " " ++ surname p
---------------------------------------------------------------------------------------------------
-- Type classes are often used to define an interface for a data structure.
-- For example we can define a type class for all types in which one can insert and rom which one can
-- take something out.

class Pushable t where
    push    :: a -> t a -> t a
    peek    :: t a -> a
    pop     :: t a -> t a

-- Notice that this class has a constructor t it's kind is * -> *
-- Instances of pushable class will have to be type constructors, not actual types.
-- List type constructors for example can implement this

instance Pushable [] where
    push x xs   = x:xs
    peek (x:_)  = x
    peek []     = error "Empty list"
    pop (_:xs)  = xs
    pop []      = error "Empty list"

-- A type constructor for a list is '[]' its kind is '*->*'. This means that a list
-- of integers has a type '[] Int', Strings are '[]' Char, general lists are '[] a'.
--- The notation [Int], [Char] is just syntactic sugar.

-- Our list 'MyList' can be made a member of the 'Pushable' class:

data MyList a = Empty | Cons a (MyList a) deriving (Show, Read, Eq, Ord)

instance Pushable MyList where
    push x xs         = x `Cons` xs
    peek (x `Cons` _) = x
    peek Empty        = error "Empty MyList"
    pop (_ `Cons` xs) = xs
    pop  Empty        = error "Empty MyList"

-- The same can be done to a binary tree.

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show, Eq)

instance Pushable Tree where
    push x t          = Node x t Null
    peek (Node x _ _) = x
    peek Null         = error "Empty Tree"
    pop  (Node _ t _) = t
    pop  Null         = error "Empty Tree"
---------------------------------------------------------------------------------------------------
--- EXERCISE 2
---------------------------------------------------------------------------------------------------
-- 2.1
coolTree = Node 'l' (Node 'o' (Node 'c' Null Null) (Node 'o' Null Null)) Null
class Takeable t where
    takeSome :: Int -> t a -> [a]

treeToList Null = []
treeToList (Node a Null Null) = [a]
treeToList (Node a l r) = treeToList l ++ [a] ++ treeToList r

instance Takeable [] where
    takeSome = take

instance Takeable Tree where
    takeSome n t = take n (treeToList t)

-- 2.2
class Headed t where
    headOf  :: t a -> a      --takes the head of the structure
    headOff :: t a -> t a    --removes the head of the structure

instance Headed [] where
    headOf []      = error "Empty list"
    headOf (x:xs)  = x
    headOff []     = error "Empty List"
    headOff (x:xs) = xs

instance Headed Tree where
    headOf   Null        = error "Empty Tree"
    headOf  (Node x _ _) = x
    headOff  Null        = error "Empty Tree"
    headOff (Node _ t _) = t

instance Headed Maybe where
    headOf  Nothing    = error "No value"
    headOf  (Just x)   = x
    headOff Nothing    = Nothing
    headOff (Just x)   = Nothing
---------------------------------------------------------------------------------------------------
--- FUNCTOR
---------------------------------------------------------------------------------------------------
-- Some operations on data structures are very common so it makes sense
-- to have special type classes for these operations.

-- For example, the 'map' operation. For a list we have:

--  map :: (a -> b) -> [a] -> [b]
--  map _ []     = []
--  map f (x:xs) = f x : map f xs

-- We can define map for a tree
treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _ Null         = Null
treeMap f (Node x l r) = Node (f x) (treeMap f l) (treeMap f r)

-- Or for a Maybe type
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just $ f x



-- This operation is abstracted by a functor type class

-- class Functor f where
--  fmap :: (a -> b) -> f a -> f b

instance Functor Tree where
   fmap _ Null         = Null
   fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)


-- Instances of this type class are types that can be mapped over.
-- Fmap is a generalization of map which only works for lists
---------------------------------------------------------------------------------------------------
--- EXERCISE 3
---------------------------------------------------------------------------------------------------
-- 3.1
tr = Node (Just 1) (Node (Just 2) Null Null) (Node Nothing Null Null)
mapOnTreeMaybe f tree = fmap (fmap (f)) tree

-- 3.2
data RoseTree a = RoseEmpty | RoseTree a [RoseTree a] deriving (Show, Eq)
instance Functor RoseTree where
    fmap _ RoseEmpty = RoseEmpty
    fmap f (RoseTree a xs) = RoseTree (f a) (map (fmap f) xs)
---------------------------------------------------------------------------------------------------
--- FOLDABLE
---------------------------------------------------------------------------------------------------
-- Another common type class is 'Foldable'. This is for types that can be folded over.
-- The simplified definition is :
-- class Foldable t where
--    foldr  :: (a -> b -> b) -> b -> t a -> b
--    foldl  :: (a -> b -> a) -> a -> t b -> a
--    foldr1 :: (a -> a -> a) -> t a -> a
--    foldl1 :: (a -> a -> a) -> t a -> a

-- The minimal complete definition is foldr.
-- For lists this went
-- instance Foldable [] where
--    foldr f z []     = z
--    foldr f z (x:xs) = f x (foldr f z xs)

-- For our binary tree the definition would be
instance Foldable Tree where
    foldr f z Null      = z
    foldr f z (Node x l r) = x `f` (foldr f (foldr f z r) l)
-- For ternary trees the definition would be
data Tree3 a = Null3 | Node3 a (Tree3 a) (Tree3 a) (Tree3 a) 
  deriving (Show,Eq)

instance Foldable Tree3 where
  foldr f z Null3           = z
  foldr f z (Node3 x l m r) = f x (foldr f (foldr f (foldr f z r) m) l)


---------------------------------------------------------------------------------------------------
--- EXERCISE 4
---------------------------------------------------------------------------------------------------
-- 4.1
sumPositive :: (Foldable t, Num a, Ord a) => t a -> a
sumPositive a = foldr (helper) 0 a
    where helper x y
               | x > 0 = x + y
               | otherwise = y

-- 4.2
size :: Foldable t => t a -> Int
size a = foldr (\x y -> y+1) 0 a

-- 4.3
eqElems :: (Foldable t, Eq a) => t a -> Bool
eqElems a = all (==first) a
    where first = head $ toList a

-- 4.4
roseTree = RoseTree 5 [RoseTree 4 [RoseTree 4 [RoseEmpty], RoseTree 0 [RoseEmpty]], RoseTree 0 [RoseEmpty]]
instance Foldable RoseTree where
    foldr f z RoseEmpty         = z
    foldr f z (RoseTree el treeList) = f el (foo f (reverse treeList))
        where foo f (x:[]) = foldr f z x
              foo f (x:xs) = foldr f (foo f xs) x

---------------------------------------------------------------------------------------------------
--- STANDARD DATA TYPES
---------------------------------------------------------------------------------------------------
-- Haskell comes with lots of useful data types like Data.Map and Data.Set

---------------------------------------------------------------------------------------------------
--- EXERCISE 5
---------------------------------------------------------------------------------------------------
-- 5.1
toSet :: (Foldable t, Ord a) => t a -> S.Set a
toSet a = S.fromList (toList a)

-- 5.2
indexWords :: String -> M.Map String [Int]
indexWords s = foo (words s) (M.fromList []) 0
    where foo  [] m _ = m
          foo (x:xs) m index = foo xs (M.insertWith (++) x [index] m) (index+1)