import Data.Char
import Data.List

--- EXERCISE 1 ----------------------------------------------------------------------------------------------------
--- 1.1
headHunter :: [[a]] -> a
headHunter (([]):([]):([]):_) = error "First three entries are empty"
headHunter (([]):([]):xs:_)   = head xs
headHunter (([]):(xs):_)      = head xs
headHunter ((xs):_)           = head xs

--- 1.2
firstColumn :: [[Int]] -> [Int]
firstColumn m = [x | (x:_) <- m]

--- 1.3
shoutOutLoud :: String -> String
shoutOutLoud []     = []
shoutOutLoud (x:xs) = [x,x,x]++xs

--- EXERCISE 2 ----------------------------------------------------------------------------------------------------
--- 2.1
pad :: String -> String -> (String, String)
pad x y
    | lx < ly = (capitalize $ x++(take (ly - lx) spaces), capitalize y)
    | ly < lx = (capitalize x, capitalize $ y++(take (lx - ly) spaces))
    | otherwise = (x,y)
    where lx     = length x
          ly     = length y
          spaces = repeat ' '
          capitalize [] = ""
          capitalize word@(x:xs) = [toUpper x] ++ [toLower c | c <- xs]

--- 2.2
median :: (Real a, Fractional b) => [a] -> b
median [] = error "median: Empty list"
median xs 
    | odd l     = realToFrac $ ys !! h
    | otherwise = realToFrac (ys !! h + ys !! (h-1)) / 2
    where l  = length xs
          h  = l `div` 2
          ys = sort xs
--- 2.3
quartiles :: (Real a, Fractional a, Fractional b, Fractional c) => [a] -> (b,a,c)
quartiles [x] = (0,x,0)
quartiles xs
    | odd l = (median $ fst halves, sndQrt, median $ tail $ snd halves)
    | otherwise = (median $ fst halves, sndQrt, median $ snd halves)
    where l      = length xs
          ys     = sort xs
          sndQrt = median ys
          halves = splitAt (div l 2) ys

--- EXERCISE 3 ----------------------------------------------------------------------------------------------------

--- I found this one a bit confusing because I don't know how to push all my
--- helper functions and expressions in a single let - in expression. If it's not a problem could you maybe
--- just paste me some examples when reviewing this homework? Thanks in advance.

--- EXERCISE 4 ----------------------------------------------------------------------------------------------------
--- 4.1
--caseFunction :: (a,b) -> [c] -> String
caseFunction (a,b) xs = 
    "The pair contains " ++ case (a,b) of
        (1,1) -> "two ones"
        (1,_) -> "one one"
        (_,1) -> "one one"
        (_,_) -> "no ones"
    ++ " and the second element of the list is " ++ case xs of
        (head:x:_) -> show x
        (head:[]) -> "not present because the list only has one element."
        ([]) -> "not present because the list is empty."