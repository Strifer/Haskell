import Data.Char
import Prelude hiding (map,filter)
import Data.List hiding (map,filter)

----------------------------------------------------------------------------------------------------
--- CURRIED FORM
----------------------------------------------------------------------------------------------------
-- We know that functions in Haskell can take many arguments. For example:

number :: Int -> Int -> Int
number x y = x*10 + y

-- f :: Int -> (Int -> Int) == f :: Int -> Int -> Int
-- We take one integer, and return a new function that takes the second argument and returns a third integer.
-- We go from the domain of integers to the codomain of functions that take one argument and give a result.
-- Once we have our function we take that function and provide it its arguments and return the value.

-- Each function in the chain takes one argument and returns a function that takes the next argument. The last function
-- in the chain returns the final result.

-- This is called currying.

-- By definition the operator (->) is RIGHT ASSOCIATIVE which means the parentheses are implied
-- a -> (b -> c) = a -> b -> c

-- The function application is LEFT ASSOCIATIVE
-- (((number 1) 2) 3) = number 1 2 3

----------------------------------------------------------------------------------------------------
--- PARTIAL APPLICATION
----------------------------------------------------------------------------------------------------
foo = number 5

-- What happens if we give too few arguments to a curried function ?
-- Thus, by partially applying a function we obtain a new function that expects
-- the next argument. This makes it possible to define many specific functions
-- using one more generic function:

fifty = number 5
sixty = number 6
atLeast100 = max 100

-- We could have defined fifty like this
-- fifty x = number 5 x

-- However in haskell this is discouraged
-- Generally we follow this pattern
-- foo(x)=f(x)
-- foo = f

-- This is called eta reduction. Keep in mind that the order of the arguments is important.
-- The first arguments should be those that vary the least.

-- The constant of 50 is defined into the function itself, this is called clojure.
----------------------------------------------------------------------------------------------------
--- SECTIONS
----------------------------------------------------------------------------------------------------
-- When we wish to partially apply infix functions we use sections.
-- Infix functions are functions that come between arguments.

addTwo = (+2)
halve  = (/2)
is42   = (==42)
lessThanTwo = (<2)
aMillionOrMore = (>=1000000)

finishSentence :: String -> String
finishSentence = (++".")

prependZero :: [Integer] -> [Integer]
prependZero = (0:)

prepend123 :: [Integer] -> [Integer]
prepend123 = ([1,2,3]++)

isUpperLetter :: Char -> Bool
isUpperLetter = (`elem` ['A'..'Z'])
----------------------------------------------------------------------------------------------------
--- EXERCISE 1
----------------------------------------------------------------------------------------------------
--- 1.1
takeThree = take 3
dropThree = drop 3
hundredTimes = take 100 . repeat

--- 1.2
index = zip [0..]
index' = (`zip` [0..])

--- 1.3
divider = (`take` repeat '=')
----------------------------------------------------------------------------------------------------
--- HIGHER ORDER FUNCTIONS
----------------------------------------------------------------------------------------------------
-- A higher order function is a function that takes another function as an argument, or returns
-- a function as an argument.

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- We've already defined some of higher-order functions. Here is a couple more:


-- Applying some function to a pair (sort of like a map).
applyToPair :: (a -> b) -> (a, a) -> (b, b)
applyToPair f (x,y) = (f x,f y)

-- Applying first function to the first element and second function to the second element.
applyToPair2 :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
applyToPair2 f g (x,y) = (f x,g y)

-- Takes some function applies two both arguments and checks if the result is the same.
equalBy :: Eq b => (a -> b) -> a -> a -> Bool
equalBy p x y = p x == p y

-- What about functions that return functions?

-- Recall: because of currying, every function of two or more arguments is in fact
-- returning a function! For example:
addThree :: Num a => a -> a -> a -> a   -- or: a -> (a -> a -> a)
addThree x y z = x + y + z
----------------------------------------------------------------------------------------------------
--- EXERCISE 2
----------------------------------------------------------------------------------------------------
--- 2.1
applyOnLast f xs ys = f (last xs) (last ys)
lastTwoPlus100 xs ys = applyOnLast (addThree 100) xs ys
-- 2.2

----------------------------------------------------------------------------------------------------
--- MAP
----------------------------------------------------------------------------------------------------
-- Adds 1 to every element
incList :: Num a => [a] -> [a]
incList []     = []
incList (x:xs) = x + 1 : incList xs

-- Turns every element inot uppercase
uppercaseString :: String -> String
uppercaseString []     = []
uppercaseString (x:xs) = toUpper x : uppercaseString xs

-- Adds n to every element list
addToList :: Num a => a -> [a] -> [a]
addToList _ []     = []
addToList n (x:xs) = x + n : addToList n xs

-- Map takes some function and applies it to every element in the list.
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

incList' = map (+1)
uppercaseString' = map (toUpper)
addToList' n = map (+n)

-- Maps are kewl.

lengths :: [[a]] -> [Int]
lengths xss = [length xs | xs <- xss]

lengths' xs = map (length) xs

camelCase :: String -> String
camelCase s = concat [toUpper h : t | (h:t) <- words s]

camelCase2 s = concat $ map up (words s)
    where up (h:t) = toUpper h : t

----------------------------------------------------------------------------------------------------
--- EXERCISE 3
----------------------------------------------------------------------------------------------------
--- 3.1
listifyList :: [a] -> [[a]]
listifyList = map (:[])

--- 3.2
cutoff :: Int -> [Int] -> [Int]
cutoff n = map (`min` n)
----------------------------------------------------------------------------------------------------
--- FILTER
----------------------------------------------------------------------------------------------------
evenNumbers :: Integral a => [a] -> [a] 
evenNumbers [] = []
evenNumbers (x:xs)
   | even x    = x : evenNumbers xs
   | otherwise = evenNumbers xs

-- By generalizing this function, we obtain the 'filter' higher-order function,
-- the second important functional idiom:

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
   | p x       = x : filter p xs
   | otherwise = filter p xs

-- For example:

threeOrMore = filter (>=3)
digits = filter (`elem` ['0'..'9'])

-- By combining 'map' and 'filter', we can now implement all functions that we've
-- previously defined using list comprehensions. For example:

caesarCode :: String -> String
caesarCode s = [succ c | c <- s, c /= ' ']

caesarCode' :: String -> String
caesarCode' s = map succ $ filter (/=' ') s
----------------------------------------------------------------------------------------------------
--- EXERCISE 4
----------------------------------------------------------------------------------------------------
--- 4.1
sumEvenSquares :: [Integer] -> Integer
sumEvenSquares xs = sum $ map (^2) $ filter (even) xs

--- 4.2
freq :: Eq a => a -> [a] -> Int
freq c xs = length $ filter (==c) xs

--- 4.3
freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (predicate) xs
    where predicate c = (freq c xs) >= n
----------------------------------------------------------------------------------------------------
--- LAMBDA EXPRESSIONS
----------------------------------------------------------------------------------------------------
-- Lambda expressions are the foundation of lambda calculus and functional programming.
-- They should be used only for very short specific functions because they are not very readable.

-- They are used to shortly define anonymous functions that are only used once in the code.

-- A function that adds 1 to each element in a list and then squares it:

foo2 :: [Int] -> [Int]
foo2 = map (\x -> (x + 1)^2)

-- We can also use pattern matching. For example
addPairs :: [(Int, Int)] -> [Int]
addPairs = map (\(x,y) -> x+y)

-- Lambda expressions are neat and all but they make code less readable.
-- Only use them for very explicit simple functions. For more complex functions functions should
-- be defined in where or let blocks.

-- In a functional languages pretty much all functions can be written using lambda expressions.

addThree' = \x y z -> x + y + z
----------------------------------------------------------------------------------------------------
--- EXERCISE 5
----------------------------------------------------------------------------------------------------
--- 5.1
withinInterval :: Ord a => a -> a -> [a] -> [a]
withinInterval n m xs = filter (\x -> x>=n && x<=m) xs

--- 5.2
sndColumn :: [[a]] -> [a]
sndColumn m = map (\x -> x!!1 ) m

--- 5.3
canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs xs = map (\(x,y) -> if x>y then (y,x) else (x,y)) $ filter (\(x,y) -> x/=y) xs

