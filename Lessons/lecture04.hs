import Data.Char
import Data.List

--- Pattern matching ---
--Instead of using guards, we can write out two CLAUSES with PATTERN MATCHING:

magicNumber2 :: Int -> String
magicNumber2 42 = "Yeah!"
magicNumber2 x  = "Nope, try again."

--If a variable is not used in the definition, we can anonymize it:

magicNumber3 :: Int -> String
magicNumber3 42 = "Yeah!"
magicNumber3 _  = "Nope, try again."

-- We often do pattern matching on tuples:

fst' :: (a,b) -> a
fst' (x,_) = x

snd' :: (a,b) -> b
snd' (_,y) = y

--Pattern matching on arguments
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

mallcolmInTheMiddle :: (a,b,c) -> b
mallcolmInTheMiddle (x,y,z) = y

-- If we have a binary tree represented as a pair of pairs of depth two, this function
-- collects the leaves.
leaves :: ((a, a), (a, a)) -> [a]
leaves ((x, y), (z, w)) = [x, y, z, w]

-- All these patterns are safe in the sense that they cannot fail, they are irrefutable.
-- Some patterns can fail, those patterns are called refutable.

goo (x,1) = x+1

-- We can also pattern match on lists. For example, we can split up right away a
-- list into its head and tail:

-- The : operator signifies the head and the tail of the list
-- It's useful to use pattern matching with them when you want
-- to extract things from the list.

head' :: [a] -> a
head' []    = error "No head to behead"
head' (x:_) = x

tail' :: [a] -> [a]
tail' []     = []
tail' (_:xs) = xs

-- Pattern matching is all about covering cases. The _ operator designates an anonymous value.
-- There is no way of using pattern matching to take the last element in a single line.
-- The list in haskell is defined as a chained tree. We need to go through every element
-- to get to the last.

-- Patterns are flexible

--Swaps the second and first elements of two pairs.
partnerSwap :: (a,b) -> (c,d) -> ((a,c),(b,d))
partnerSwap (x,y) (z,w) = ((x,z),(y,w))

-- Takes two lists and swaps their heads.
headSwap :: [a] -> [a] -> ([a],[a])
headSwap (x:xs) (y:ys) = (y:xs, x:ys)

-- Also, we can have many different patterns:
-- By using patterns we've defined the second element y as the one we want to operate with.
-- We need to add additional clauses to the pattern in case where the pattern may break.

foo :: [a] -> [a] -> [a]
foo (x:xs) (_:y:_) = x:y:xs
foo (x:_)  [y]     = [x,y]
foo xs     []      = xs

-- Patterns can be nested

headOfHead :: [[a]] -> a
headOfHead ((x:_):_) = x
headOfHead _         = error "No head of head"

-- You must be careful how you order the definitions. The most general case should
-- come at the end:

rhymeMe :: String -> String
rhymeMe "letters" = "matters"
rhymeMe "love"    = "glove"
rhymeMe "pain"    = "rain"
rhymeMe (x:xs)    = succ x : xs
rhymeMe _         = "huh?"

-- In particular, you should distinguish '[x,y]' from '(x:y:_)':

describeList :: Show a => [a] -> String
describeList []      = "This thing is empty"
describeList [x]     = "This list has only one element: " ++ show x
describeList [x,y]   = "This list has elements " ++ show x ++ " and " ++ show y
describeList (x:y:_) = 
    "This list has 3 or more elements, of which the first two are " ++
    show x ++ " i " ++ show y

-- Now, let's say we'd also like to print out the length of the list in the above
-- function. For this we need the complete list. So, we need both the complete
-- list and its first two elements. We can do that with an AS-PATTERN:

-- AS-PATTERN = variable @ [as] (pattern)

describeList' xs@(x:y:_) = 
    "This list has " ++ show (length xs) ++ " elements, " ++
    "of which the first two are " ++ show x ++ " and " ++ show y
describeList' _          = "This list has two or less elements"

-- Another example of an AS-PATTERN in action:

spellFirst :: String -> String
spellFirst w@(c:_) = toUpper c : " as in " ++ w

-- Let's write a function that tests whether a matrix (here represented as a list
-- of lists) contains a row with identical elements. First shot:

rowWithEqualElements :: Eq a => [[a]] -> Bool
rowWithEqualElements m = 
    or [ and [ e == head row | e <- row] | row <- m]


-- Important: All patterns within a list comprehension are irrefutable in the
-- sense that they cannot cause an error. If an element doesn't match a pattern,
-- it is simply skipped.

-- Takes only rows with one element
singletonElems :: [[a]] -> [a]
singletonElems xs = [x | [x] <- xs]

-- Another thing to remember is that you cannot test for equality by using a
-- variable multiple times. This will not work:

-- sameElems :: (a,a) -> Bool
-- sameElems (x,x) = True
-- sameElems _     = False

-- Nor will this:

-- headsEqual :: [a] -> [a] -> Bool
-- headsEqual (x:_) (x:_) = True
-- headsEqual _     _     = False

-- (The reason why this doesn't work is because Haskell is doing PATTERN MATCHING,
-- and not variable UNIFICATION. Prolog does unification, so there it would work.
-- But luckily we don't program in Prolog here.)

-- The correct way of doing this is to use different variables and then explicitly
-- check for equality:

sameElems :: Eq a => (a,a) -> Bool
sameElems (x,y) = x == y

headsEqual :: Eq a => [a] -> [a] -> Bool
headsEqual (x:_) (y:_) = x == y
headsEqual _     _     = False

------------------ EXERCISE 1 ------------------------
headHunter :: [[a]] -> a
headHunter ((x:_):_) = x
headHunter (([]):(x:_):_)=x
headHunter (([]):([]):(x:_):_) = x
headHunter _ = error "first three elements are empty"



---------------------------------------------------------

--- LOCAL DEFINITIONS ---
-- We should avoid calculating the same thing over and over again.
-- In haskell we avoid this by locally defining functions and variables
-- We use the where operator.

triangleArea :: Double -> Double -> Double -> Double
triangleArea a b c = sqrt $ s * (s-a) * (s-b) * (s-c)
    where s = (a + b + c) / 2

pairsSumTo100 = [(x,y) | x <- xs, y <- xs, x+y==100]
    where xs = [1..100]

-- The scope of 'where' declarations extends over guards:

tripCost :: Int -> Double -> Double -> String
tripCost days hotel travel
    | cost <= 100 = "Extremely cheap vacation"
    | cost <= 500 = "Cheap vacation"
    | otherwise   = "Expensive vacation"
    where cost = hotel * realToFrac days + travel

-- We can have many 'where' declarations:

tripCost' :: Int -> Double -> Double -> String
tripCost' days hotel travel
    | cost <= 100 = "Extremely cheap vacation"
    | cost <= 500 = "Cheap vacation"
    | otherwise   = "Expensive vacation"
    where hotelCost = hotel * realToFrac days
          cost      = hotelCost + travel

-- A median of a list of numbers:
-- The median is the middle element in an ordered lit of elements.
-- In the case of an even number of elements, we find the average of the middle two elements.

median :: (Integral a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
    | odd l     = realToFrac $ ys !! h
    | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
    where l  = length xs
          h  = l `div` 2
          ys = sort xs

-- Where blocks are private to clauses. Only one clause can see them.

--Another example of a bad style:

initials :: String -> String -> String
initials first last = f : ". " ++ l : "."
     where (f:_) = first
           (l:_) = last

--Do this instead:

initials' (f:_) (l:_) = f : ". " ++ l : "."

-- How can the above definition be extended to cover the cases when the first/last
-- name is an empty string?
-- Just enumerate all the possible cases.

-- WHEN TO USE WHERE?
-- Where blocks should be used whenever an expression is repeated locally.
-- Global definitions should be used whenever values or functions are used in more funcctions.
-- If the whale block is nested in more than two levels.

-- EXERCISE 2 ----------------------
pad :: String -> String -> (String, String)
pad x y
    | lx < ly = (x++(take (ly - lx) spaces), y)
    | ly < lx = (x,y++(take (lx - ly) spaces))
    | otherwise = (x,y)
    where lx     = length x
          ly     = length y
          spaces = repeat ' '
          capitalize [] = ""
          capitalize xs@(x:_) = [toLower c | c <- xs] 
-----------------------------------------------------


------------ LET ------------------------------------
-- A let-in block is similar to where.
-- Where is declared at the end of a function and it only applies to one specific clause.
-- let can be declared anywhere and is by itself an expression
-- let x in y defines x only in that expression

triangleArea' :: Double -> Double -> Double -> Double
triangleArea' a b c = 
    let s = (a + b + c) / 2 in sqrt $ s * (s-a) * (s-b) * (s-c)


-- Let should be sued in very local expressions (only one line, list comprehensions for example)
-- Where should be used in more general expressions (guards branching and stuff)

-- Let is visible in list comprehensions, where is not visible.

bigTriangles :: [((Double, Double, Double), Double)]
bigTriangles = [ ((x,y,z), a) | 
    x <- ns, y <- ns, z <- ns, 
    let a = triangleArea x y z, x + y >= z, x < y, a >= 100]
    where ns = [1..100]


--------------- CASE -----------------------------------
-- Cases are very powerful. They enable pattern matching anywhere in the code.
magicNumber4 :: Int -> String
magicNumber4 x = case x of
    42 -> "Yeah!"
    _  -> "Nope, try again."

-- Case itself is an expression.
describeList2 xs =
    "This is a list that " ++ case xs of
      []  -> "is empty"
      [x] -> "has one element"
      xs  -> "has " ++ show (length xs) ++ " elements"
-- EXERCISE 4-------
caseFunction :: (a,b) -> [c] -> String
caseFunction (a,b) [c] = 
    "The pair contains" ++ case (a,b) of
        (1,1) -> "two ones"
        (1,_) -> "one one"
        (_,1) -> "one one"
        (_,_) -> "no ones"
    ++ "and the second element of the list is "++ case [c] of
        [_:x:_]




