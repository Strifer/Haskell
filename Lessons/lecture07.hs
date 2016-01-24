import Data.Char
import Prelude hiding (foldr,foldl,flip,curry,uncurry)
import Data.List hiding (foldr,foldl)
import Control.Monad

----------------------------------------------------------------------------------------------------
--- COMPOSITION
----------------------------------------------------------------------------------------------------
-- A composition of functions: (f . g) (x) = f[g(x)]
-- In haskell we have the cool '.' operator which is defined as follows
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = \x -> f  (g x)

-- E.g., the successor of the first element of a pair:

succOfFst :: (Int,Int) -> Int
succOfFst p = (succ . fst) p

-- Due to eta-reduction, this boils down to:

succOfFst' :: (Int,Int) -> Int
succOfFst' = succ . fst

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyTwice' :: (a -> a) -> a -> a
applyTwice' f = f. f

-- Recall the 'caesarCode' function. Let's look at three ways how to define this
-- function:

caesarCode1 :: String -> String
caesarCode1 s = [succ c | c <- s, c /= ' ']

caesarCode2 :: String -> String
caesarCode2 s = map succ $ filter (/=' ') s

caesarCode3 :: String -> String
caesarCode3 = map succ . filter (/=' ')

-- So compositions enable us to eta reduce, how convenient.
-- Composiiton is right associative
-- (f . g . h) (x) = f (g (h (x)))

wordSort :: String -> String
wordSort = unwords . sort . words

-- If we want a composition of functions that don't have the same number
-- of arguments we partially apply the functions so that each expects
-- only one argument.

initials :: String -> String
initials = map toUpper . map head . words

tokenize :: String -> [String]
tokenize = filter (\w -> length w >=3) . words . map toUpper

tokenize' = filter ((>=3) . length) . words . map toUpper

-- This is a bit complex tbh.

foo :: Ord a => [a] -> [(a,Int)]
foo = map (\xs@(x:_) -> (x, length xs)) . group . sort

-- This style is called a point free style because it allows us to focus
-- only on the functionality of the functions. Arguments and variables are not
-- mentioned. Pointfree style is usually desirable but it should never impede on readability.

