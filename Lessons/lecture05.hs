-- Recursive functions, yay

-- INTRO -------------------------------------------------------------------------------------------
-- Recursive functions call themselves, and process until they reach a base case.
-- In functional programming many problems are solved with recursion.
-- The main idea is to divide a problem into subproblems, and those subproblems into more subproblems until
-- some sort of base case is reached. That is the simplest case.
----------------------------------------------------------------------------------------------------


fact :: (Eq a, Num a) => a -> a
fact x = if x==0 then 1 else x*fact(x-1)

fact' :: (Eq a, Num a) => a -> a
fact' 0 = 1
fact' x = x*fact'(x-1)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- This is O(2^n) complexity, wow don't do this.

-- People created Haskell just to write a small quicksort.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort ys ++ [x] ++ quicksort zs
  where ys = [y | y <- xs, y <= x]
        zs = [z | z <- xs, z  > x]

-- The base case is an empty list, it is simply returned once reached.
-- The general principle is that we have a pivot element with which we divide
-- The list into all element smaller and bigger than the pivot.
-- Recursively we then divide those sublists using the same principle.

-- Recursion over data structures typically involves lists or trees.
-- Such a recursion is called structural recursion because we are processing
-- over a certai structure. Since there are no loops in haskell we must recurse down
-- over data structures, decomposing them into substructures and manipulating them.

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

-- Recursive sum, base case is an empty list.
-- General case is that we take the head of a list, and sum up the tail (which will recurse down to the base case).
-- This is a standard structure recursion, we recurse down to the base case
-- And build up the result from there.

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

-- Recursive length, bas case is an empty list.
-- General case is that we remove the head of the list, length 1
-- The tail is sent into further recursion, removing bit by bit and summing everything up

incList :: Num a => [a] -> [a]
incList []     = []
incList (x:xs) = x + 1 : incList xs

-- Recursive increment. Bae case is an empty list.
-- We take each head and adding one to it and combining it with the tail
-- The tail is sent into further recursion
-- We are decomposing one list while composing a new one.

-- Same thing with list comprehension.
incList' :: Num a => [a] -> [a]
incList' xs = [x+1 | x <- xs]

concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

-- The base case is an empty list once again.
-- We take the head and concate the tail which is sent into further recursion.

maximum' :: Ord a => [a] -> a
maximum' [x]    = x
maximum' (x:xs) = x `max` maximum' xs

-- This one is a bit more odd. The base case is a list where we only have 1 element left (the maximum)
-- We take the head and compare it with the tail, the tail is recursed more and more.
-- The end result is that we are actually sequentially comparing all the element until we only have one left.

-- For lists of length 'n', the time complexity is O(n).

-- Notice that in the above functions there is a recurring pattern:

-- foo ...                             <-- base case
-- foo (x:xs) = f x `operator` foo xs  <-- general case

-- EXERCISE 1 --------------------------------------------------------------------------------------
--1.1
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

--1.2
headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf (xs:xss) = head xs : headsOf xss
----------------------------------------------------------------------------------------------------

-- A recursive function can have many arguments. Argument that remain unchanged serve to store states.

addToList :: Num a => a -> [a] -> [a]
addToList _ []     = []
addToList n (x:xs) = x + n : addToList n xs

-- n remains unchanged and is added to every element

incIncList :: Num a => a -> [a] -> [a]
incIncList _ []     = []
incIncList n (x:xs) = x + n : incIncList (n+1) xs
-- n is increased upon every recursive call, we are adding n+length xs to the lat element

-- What if we wanted to define a function that increments the first element by 0,
-- the second by 1, etc.?

-- incIncList' [3,2,1] => [3,3,3]

-- We don't want the user to always provide 0 as the argument. Instead, we define
-- a WRAPPER FUNCTION:

incIncList' :: Num a => [a] -> [a]
incIncList' xs = inc 0 xs
   where inc _ []     = []
         inc n (x:xs) = x + n : inc (n+1) xs

-- The wrapper function is a general adding function and then we specify that we only want the argument 0.

-- EXERCISE 2 --------------------------------------------------------------------------------------
--2.1
modMult :: Integral a => a -> a -> [a] -> [a]
modMult _ _ [] = []
modMult n m (x:xs) = ((n `mod` m)*x) : (modMult n m xs)

--2.2
addPredecessor :: Num a => [a] -> [a]
addPredecessor [] = []
addPredecessor xs = add 0 xs
   where add _ [] = []
         add p (x:xs) = p + x : add x xs
----------------------------------------------------------------------------------------------------
-- In the recursive case we can test additional conditions and act based on these.
-- This is how we can implement the filtering of a list:

numPositives :: (Num a, Ord a) => [a] -> Int
numPositives []     = 0
numPositives (x:xs) | x > 0     = 1 + numPositives xs
                    | otherwise = numPositives xs

-- We only count positive elements, we skip negative numbers.
-- EXERCISE 3 --------------------------------------------------------------------------------------
--3.1
equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets []    = []
equalTriplets ((a,b,c):xs) |  (a == b) && (b == c) = (a,b,c) : equalTriplets xs
                           |  otherwise            = equalTriplets xs

--3.2
replicate' 1 xs = [xs]
replicate' n xs
    | n<0       = error "wtf"
    | otherwise = xs : (replicate' (n-1) xs)
-----------------------------------------------------------------------------------------------------
-- Let's define 'take':

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

-- We have two base cases, when we're taking 0 elements and an empty list.
-- The general case is that we take one element and decrease our state of how many we will take in the future.


-- Does this work as expected if n<0 (does it return an unaltered list)?
    -- It does because we reach the base case of an empty list.

-- How can we extend the above definition so that, if n > length xs, the last
-- element of the list gets repeated?
-- take'' 5 [1,2,3] => [1,2,3,3,3]
    --

-- How would you define the same function using standard functions from Prelude?
-- EXERCISE 4 --------------------------------------------------------------------------------------
--4.1
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs)
    | n<0 = reverse $ drop'' n (x:xs)
    | otherwise = drop' (n-1) xs
    where drop'' n xs = drop' (-1*n) (reverse xs)

--4.2
-----------------------------------------------------------------------------------------------------
-- Here's how 'zip' function is defined:

--zip' :: [a] -> [b] -> [(a,b)]
zip' []     _      = []
zip' _      []     = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
--        | x==y = (x,y) : zip' xs ys
--        | otherwise = zip' xs ys

--How can we extend this so that it only pairs up (x,y) for which x==y?

-- We don't always need to process the elements one by one. For example, a
-- function that takes a list and pairs up the consecutive elements would be
-- defined like this:

pairUp :: [a] -> [(a,a)]
pairUp (x:y:xs) = (x,y) : pairUp xs
pairUp _        = []
-- EXERCISE 5 --------------------------------------------------------------------------------------
--5.1
eachThird :: [a] -> [a]
eachThird (x:y:z:xs) = z : eachThird xs
eachThird _ = []

--5.2

-- ACCUMULATORS -------------------------------------------------------------------------------------
-- Let's look at the definition of the factorial function again:

fact1 :: (Eq a, Num a) => a -> a
fact1 0 = 1
fact1 x = x * fact1 (x-1)

-- This function is executed as follows: we go down until we "hit" the base case,
-- and then build up the result incrementally as we return from the recursive
-- calls. Actually, the result is built up while on our way back.

-- But another solution is possible, one in which we recurse down and "accumulate"
-- the solution incrementally as we descend. When we "hit" the base case, we
-- simply return the solution accumulated thus far. There is no need to go back,
-- because there is nothing left to be done on the way back. We can simply "jump
-- out" of the recursion.

fact2 :: (Eq a, Num a) => a -> a -> a   -- the second arg. is the accumulator
fact2 0 n = n
fact2 x n = fact2 (x-1) (x*n)

-- n is our accumulator, we're building up the result starting from the beginning, until we hit the bae case
-- after that we return the entire result, no need to work backwards

-- this is our main function that set the accumulator to 1
fact3 :: (Eq a, Num a) => a -> a
fact3 x = fact2 x 1

-- Why are accumulator better? Well they have a smaller space complexity.

-- General recurion works by building up a bigger and bigger expression with each recursive call.
-- When we reach the base case the expresion can be evaluated

-- On the other hand in accumulators if we are building up our result each call we don't need to build up
-- a bigger expression. We are just sending the parameters away because we don't need to go back to anything.
-- This is a tail recursion, the compiler recognizes these functions and optimizes their space complexity to be O(1)

sum1 :: Num a => [a] -> a
sum1 []     = 0
sum1 (x:xs) = x + sum1 xs

sum2 :: Num a => [a] -> a
sum2 xs = sum xs 0
   where sum []     s = s              -- 's' is the accumulator
         sum (x:xs) s = sum xs (x+s)

reverse2 :: [a] -> [a]
reverse2 xs = rev xs []
  where rev []     ys = ys
        rev (x:xs) ys = rev xs (x:ys)
-- EXERCISE 6 --------------------------------------------------------------------------------------
--6.1
length'' :: [a] -> Int
length'' xs = length2 xs 0
    where length2 []     s = s
          length2 (x:xs) s = length2 xs (s+1)

--6.2

----------------------------------------------------------------------------------------------------
--- GUARDED RECURSION
----------------------------------------------------------------------------------------------------
-- Sometimes, using an accumulator doesn't even make sense to begin with. For
-- example, if we do structural recursion on a list and modify each element in
-- turn. Look at the 'incList' function:

incList1 :: Num a => [a] -> [a]
incList1 []     = []
incList1 (x:xs) = x + 1 : incList1 xs

-- The space complexity of this function is O(1). (The list that is being
-- constructed is not counted in the space complexity. Only additional memory
-- allocated during the computation is counted, but here no extra memory is being
-- allocated.)

-- We might give it a shot with an accumulator-style version:

incList2 :: Num a => [a] -> [a]
incList2 xs = inc xs []
   where inc []     ys = ys
         inc (x:xs) ys = inc xs ((x+1):ys)

-- Another example is 'unzip'. We may give it a try with accumulators (two in this
-- case):

unzip' :: [(a,b)] -> ([a],[b])
unzip' zs = unz zs [] []
   where unz []         xs ys = (xs,ys)
         unz ((x,y):zs) xs ys = unz zs (x:xs) (y:ys)

-- But this is again not good for the same reason as above: we end up with lists
-- in reverse order. We might first reverse the input list, but that would require
-- two list traversals (one for the reversal and one for unzipping).

-- Hence in this case too we should resort to standard recursion:

unzip'' :: [(a,b)] -> ([a],[b])
unzip'' []         = ([],[])
unzip'' ((x,y):zs) = (x:xs,y:ys)
   where (xs,ys) = unzip'' zs

-- These two functions have one thing in common, we're constructing a list incrementaly, element by element.
-- Haskell is lazy it only does what it needs at the given moment the first part of the list is consumed 
-- and the rest is just sent for further manipulation. This is called a GUARDED RECURSION because of laziness
-- we're not allocating memory during each recursive call however it is still not a tail recursion because

-- Inclist1 is not tail recursive because we are doing something besides just making a recursive call and passing variables.

-- Tail recursion reduces space complexity, it's achieved by accumulators but only if we need a single summed up result.
-- If we don't need the whole output at once guarded recursion is better.
----------------------------------------------------------------------------------------------------
--- CORECURSION
----------------------------------------------------------------------------------------------------
-- Corecursion is a dual concept to recursion, instead of decomposing a structure we compose a structure.
-- In corecursion the recursive caller is applied on a structure larger than the input structure.

ones :: [Integer]
ones = 1 : ones

nats :: [Integer]
nats = 1 : next nats
   where next (x:xs) = x + 1 : next xs

-- A bit more complex: a list of Fibonacci Numbers:

fibs :: [Integer]
fibs = 0 : 1 : next fibs
   where next (x:ys@(y:_)) = (x+y) : next ys
