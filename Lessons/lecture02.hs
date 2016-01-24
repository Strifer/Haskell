import Data.Char
import Data.List

-- the ':' operator 
-- it expects an element on the left side and a list ont he right side

l1   = [1,2,3]
l1'  = (1:(2:(3:[])))
l1'' = 1:2:3:[]

-- list concatenation
l2 = [1,2,3] ++ [3,4,5]
myConcat l1 l2 = l1 ++ l2

--turning elements into singleton lists
listify  x = x:[]
listify' x = [x]

-- Head, init the beginning of a list
-- Tail, last the ending of a list

-- We can drop elements of a list

l3 = take 3 [9,2,10,3,4]
l4 = drop 4 [1,2,3,4,5,6]

-- Strings are a list of characters.

l6 = "This is a string"
l7 = head l6
l8 = 'H':"askell"

isPalindrome s = s == reverse s

-- Lists cannot be heterogenous, they must contain the same type

-- Repeat creates an infinite list
l9 = repeat 'a'
-- Cycle takes a complete list and continously repeats it
l10 = cycle [1,2,3]
-- Replicate continously concatenates with given time
l11 = replicate 10 'a'

replicate' n x = take n $ repeat x

-- intervals from the first number inclusively, to the last number inclusively
l12 = [1..1000]
l13 = [1,3..999]
l14 = take 10 [1,3..100]

-- laziness in action, it only take what it needs
l15 = take 10 [1..]

-- this takes an infinite amount of time
l16 = tail [1..]
n = length [1..]

-- trims the first and last characters in a list
trim  l = tail (init l)
trim' l = init $ tail l

--pads lists with spaces if they are shorter than 10
blanks = repeat ' '
padTo10 s = s ++ take (10 - length s) blanks

--List of lists
l17 = [[1,2,3],[4,5,6],[7,8,9]]
l18 = ["red","green","blue"]

-- Minimum and maximum of a list:

m1 = minimum [4,1,2,3]
m2 = maximum "Haskell for the win!"

-- Indexing lists
e17 = [1..] !! 17
e17' = l17 !! 1 !! 0

--EXERCISES 

--2.1
dropper x
    | length x <4 = error "Must be at least 4 characters long"
    | otherwise = tail (reverse (drop 3 (reverse x)))

--2.2
initials x y = [head (x)] ++ ". " ++ [head(y)] ++"."

--2.3
specialConcat x y
    | length x < length y = y ++ x
    | otherwise = x ++ y

--2.4
safeHead x
    | null x = []
    | otherwise = [head x]

--2.5
hasDuplicates x = length x /= length (nub (x))

-- LIST COMPREHENSIONS

-- predicates are cool
doubles = [x*2 | x <- [1..10]]
doublesFromTo a b = [x*2 | x <- [a..b]]

sums1 = [x + y | x <- [1..10], y <- [1..10]]
sums2 = [x + y | x <- [1..10], y <- [1..10], x < y]
sums3 = [x + y | x <- [1..10], y <- [1..10], x < y, odd x || even y]

-- List of sublist lengths:

lengths xss = [length xs | xs <- xss]

totalLength xss = sum $ lengths xss

--Combining Strings (o_o)?


-- List comprehension with strings (since they're lists lol)
codes = [ [c1,c2] | c1 <- "abc", c2 <- "123"]
caesarCode s = [succ c | c <- s, c /= ' ']
onlyDigits s = [c | c <- s, isDigit c]
upperCase s = [toUpper c | c <- s]

doublesFromTo' a b
    | a<b = [x*2 | x <- [a..b]]
    | otherwise = [x*2 | x <- [b..a]]

caesarCode' s n = [chr (ord c + n) | c <- s, c /= ' ', c >= 'a', c <= 'z']

--Breaking up a string into tokens:

ws = words "I think we agree, the past is over."

--Breaking up a string into lines:

ls = lines "First line\nSecond line"

--Concatenating lines into a single string:

stream = unlines ls

--Filtering words with initial uppercase letter:

capitalized s = [w | w <- words s, isUpper $ head w]

--We split a string into words and fill it into w.
--Then we capitalize the first character of every word and add it to tail.
camelCase s = concat [toUpper (head w) : tail w | w <- words s]

--Or, more succinctly, using pattern matching:

camelCase' s = concat [toUpper g : r | (g:r) <- words s]

letterCount s = totalLength [w | w <- words s, length w > 2]

-- =======TUPLES============
-- tuples are pairs, triplets and n-tuples in general they can be of mixed types

pair1 = (1,2)
pair2 = ("Vatroslav","Lisinski")

triplet1 = (10,1,8)
triplet2 = ("Vatroslav","Lisinski", 1819)

e3 = pair1 == (1,2)

-- Values to tuples
(a,b) = (1,2)

-- First and second values
p = fst (1,2)
d = snd (5,6)

-- List of pairs
pairsUpTo100 = [(x,y) | x <- [0..100], y <- [0..100]]

-- Pairs of numbers that sum up to 100:
pairsSumTo100 = [(x,y) | x <- [0..100], y <- [0..100], x+y==100]

-- Pythagorean triplets
pythagoreanTriplets = 
    [(a,b,c) | a <- [1..10], b <- [1..10], c <- [1..10], a^2 + b^2 == c^2]

-- Optimized triplets
pythagoreanTriplets' = 
    [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2]


-- The zip function takes two lists and binds pairs up their elements
pairs2 = zip [1,2,3] "abc"
pairs3 = zip [1,2,3,4] "abc"
pairs4 = zip [1..] "abc"
pairs5 = zip [1..100] [2..100]

inCircle r x y = [(a,b) | a <- [-10..10], b <- [-10..10], (a-x)^2+(b-y^2)==r^2]
steps xs = zip (xs) (tail (xs))

--Indexing elements of a list:

-- pairs up a number with each element
index xs = zip [0..] xs

--Filtering every second list element:
--Takes only even indices, which amounts to every element
evenElems xs = [ snd ix | ix <- index xs, even (fst ix)]

--The same thing, but using pattern matching:

evenElems' xs = [ x | (i,x) <- index xs, even i]

--Zip3:
--triplet zip
triplets2 = zip3 [0..10] ['A'..] [100..]

indices x xs = [ fst ix | ix <- index xs, x== snd ix]