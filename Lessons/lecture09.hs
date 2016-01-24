-- Custom data types part duo

import Data.List
import Control.Monad

-- Last week we learned about custom data types.
-- Today we extend this and look into recursive data types like lists and trees.


------------------------------------------------------------------------------------------------------------------
-- RECURSIVE DATA STRUCTURES
------------------------------------------------------------------------------------------------------------------

-- Data can be recursive, in fact this is the most useful kind of structure.

data Sex = Male | Female deriving (Show,Read,Ord,Eq)

data Person = Person {
   idNumber :: String,
   forename :: String,
   surname  :: String,
   sex      :: Sex,
   age      :: Int,
   partner  :: Maybe Person,
   children :: [Person] } deriving (Show,Read)

-- A person has an id, a name and surname their sex age partner and children.
-- Sex is a data type, which can be either male or female
-- Partner is a "Maybe Person" it can be empty.
-- Children is a list of persons.

-- Let's look at one example: Pero and Ana are Marko's parents, Marko and Maja are dating.

pero = Person "2323" "Pero" "Perić" Male 45 (Just ana) [marko]
ana   = Person "3244" "Ana"   "AniÄ‡"  Female 43 (Just pero)  [marko,iva]
marko = Person "4341" "Marko" "PeriÄ‡" Male   22 (Just maja)  []
maja  = Person "7420" "Maja"  "MajiÄ‡" Female 20 (Just marko) []
iva   = Person "4642" "Iva"   "IviÄ‡"  Female 16 Nothing      []

-- If we try to print out pero or ana we enter into a recursion because they
-- are mutually defined through each other.

-- Also if we try to test for equality we run into some strange things.

-- pero == pero will not terminate
-- ana == pero will terminate as false, all it takes is one unequal field to prove it

-- let's write a function that returns the name of someone's partner.

partnersForename :: Person -> Maybe String
partnersForename p = case partner p of
    Just p  -> Just $ forename p
    Nothing -> Nothing


-- or with using fmap
partnersForename2 :: Person -> Maybe String
partnersForename2 = fmap forename . partner

-- Let's write a function that gives children of both partners.
pairsChildren :: Person -> [Person]
pairsChildren p = nub $ children p ++ case partner p of 
  Nothing -> []
  Just r  -> children r

-- This will not terminate because nub cuts out duplicates by using the equality
-- operator. And the equality operator never terminates.

pairsChildren' :: Person -> [Person]
pairsChildren' p = nub $ children p ++ case partner p of 
  Nothing -> []
  Just r  -> children r

-- New data

data Person2 = Person2 {
  personId2 :: String,
  forename2 :: String,
  surname2  :: String,
  sex2      :: Sex,   --- data Sex = Male | Female deriving (Show,Read,Eq,Ord)
  mother2   :: Maybe Person2,
  father2   :: Maybe Person2,
  partner2  :: Maybe Person2,
  children2 :: [Person2] } deriving (Show,Read,Eq,Ord)

john = Person2 "123" "John" "Doe" Male Nothing Nothing (Just jane) []
jane = Person2 "623" "Jane" "Fox" Female (Just ann) Nothing (Just john) []
ann  = Person2 "343" "Ann"  "Doe" Female Nothing Nothing Nothing [jane]

------------------------------------------------------------------------------------------------------------------
-- EXERCISE 1
------------------------------------------------------------------------------------------------------------------
-- 1.2
parentCheck :: Person2 -> Bool
parentCheck p = (forename2 p) `elem` (map forename2 (childrenM ++ childrenF))
    where childrenM = maybe [] children2 (mother2 p)
          childrenF = maybe [] children2 (father2 p)

------------------------------------------------------------------------------------------------------------------
-- POLYMORPHIC DATA STRUCTURES
------------------------------------------------------------------------------------------------------------------
-- A list is a recursive data structure. It is also parametrized.
-- Lets define our own list.

data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord,Eq)

-- Our list is defined as an element followed by a list.
-- Recursively this will boil down to a sequence of elements.
l1 = 1 `Cons` Empty
l2 = 1 `Cons` (2 `Cons` (3 `Cons` Empty))

-- We can define our own infix operator.
-- We need a right associative operator so we use the 'infixr' function.
infixr 5 -+-
(-+-) = Cons

-- Now we can write...
l3 = 1 -+- 2 -+- 3 -+- Empty
------------------------------------------------------------------------------------------------------------------
-- EXERCISE 2
------------------------------------------------------------------------------------------------------------------
--2.1
listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (a `Cons` list) = Just a

--2.2
listMap :: (a -> b) -> MyList a -> MyList b
listMap f Empty = Empty
listMap f (a `Cons` list) = (f a) `Cons` (listMap f list)
------------------------------------------------------------------------------------------------------------------
-- TREE
------------------------------------------------------------------------------------------------------------------
-- A prototypical example of a recursive data structure is a tree.

-- Let's define a binary tree that stores things.

data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)
-- Null indicates terminating leaves

-- A binary tree of integers
intTree :: Tree Int
intTree = Node 3 (Node 2 Null Null) (Node 1 Null Null)

--           1
--      2          3
--  Null Null   Null Null

-- A function that sums the elements in a binary tree of integers:

sumTree :: Tree Int -> Int
sumTree Null                = 0
sumTree (Node x left right) = x + sumTree left + sumTree right

-- A function that tests whether an element is contained in a tree:

treeElem :: Eq a => a -> Tree a -> Bool
treeElem _ Null = False
treeElem x (Node y left right) = (x == y) || (treeElem x left || treeElem x right)
------------------------------------------------------------------------------------------------------------------
-- EXERCISE 3
------------------------------------------------------------------------------------------------------------------
--3.1 Needs work
treeMax :: Ord a => Tree a -> a
treeMax Null = error "Empty tree"
treeMax (Node elem Null Null)  = elem
treeMax (Node elem left right) = max elem (max (treeMax left) (treeMax right))

--3.2
treeToList :: Ord a => Tree a -> [a]
treeToList Null = []
treeToList (Node elem Null Null)  = [elem]
treeToList (Node elem left right) = treeToList left ++ [elem] ++ treeToList right
------------------------------------------------------------------------------------------------------------------
-- BINARY SEARCH TREE
------------------------------------------------------------------------------------------------------------------
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r) 
   | x < y     = Node y (treeInsert x l) r
   | x > y     = Node y l (treeInsert x r)
   | otherwise = t
------------------------------------------------------------------------------------------------------------------
-- EXERCISE 4
------------------------------------------------------------------------------------------------------------------
--4.1
listToTree :: Ord a => [a] -> Tree a
listToTree xs = foldl (\t x -> treeInsert  x t) Null xs

------------------------------------------------------------------------------------------------------------------
-- DERIVING TYPE CLASSES
------------------------------------------------------------------------------------------------------------------
data Weekday = 
  Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show,Enum)

yesterday :: Weekday -> Weekday
yesterday = pred

dayAfterYesterday :: Weekday -> Weekday
dayAfterYesterday = succ . pred

workDays = [Monday .. Friday]
------------------------------------------------------------------------------------------------------------------
-- DEFINING TYPE CLASS INSTANCES
------------------------------------------------------------------------------------------------------------------
-- We'd like to create our own equality function for persons. We'd like to consider two people identical
-- if they have the same id.

-- class Eq a where
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool

--    x == y = not (x /= y)
--    x /= y = not (x == y)

-- There's the name of the type class, a type variable, and a list of functions
-- that must be defined for this type, in this case functions (==) and (/=). The
-- definitions themselves are not given here, only the type signatures.

-- We then can (but most not) have default definitions of functions, like it is
-- done in this case.

-- Let's look now at how we can define that a type is an instance of the 'Eq' type
-- class:


instance Eq Weekday where
  Monday    == Monday    = True
  Tuesday   == Tuesday   = True
  Wednesday == Wednesday = True
  Thursday  == Thursday  = True
  Friday    == Friday    = True
  Saturday  == Saturday  = True
  Sunday    == Sunday    = True
  _         == _         = False

-- This is tedious, it can be done automatically (by deriving I guess)

-- Let's make a more specific Eq function for Person.

instance Eq Person where
   p1 == p2 = idNumber p1 == idNumber p2

instance Ord Person where
    p1 <= p2 = idNumber p1 <= idNumber p2
------------------------------------------------------------------------------------------------------------------
-- EXERCISE 5
------------------------------------------------------------------------------------------------------------------
---
------------------------------------------------------------------------------------------------------------------
-- Parametrized types
------------------------------------------------------------------------------------------------------------------
-- What if we want an instance of a parametrized type
-- That's not a problem. We simply have to provide a type variable together with
-- the type constructor. E.g.:

-- instance Eq (Maybe a) 
--    Just x  == Just y   = x == y
--    Nothing == Nothing  = True
--    _       == _        = False

-- DATA CONSTRUCTORS
-- TYPE CONSTRUCTORS
-- kind -> type -> value
