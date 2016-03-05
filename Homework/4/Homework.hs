import Data.List
import Data.Char
---------------------------------------------------------------------------------------------------
--- PROBLEM 1
---------------------------------------------------------------------------------------------------
-- Cyclically maps a list of function fs over the list xs.
cycleMap :: [a -> b] -> [a] -> [b]
cycleMap []  _     = []
cycleMap _  []     = []
cycleMap fs (x:xs) =  cycleHelp (cycle fs) (x:xs)
                where cycleHelp (y:ys) []     = []
                      cycleHelp (y:ys) (x:xs) = (y x):(cycleHelp ys xs)

---------------------------------------------------------------------------------------------------
--- PROBLEM 2
---------------------------------------------------------------------------------------------------
-- Reduces a list of elements to a single element using a binary reducing function and an initial
-- seed value. Reduction is performed in a left associative way.
reduce :: (a -> b -> a) -> a -> [b] -> a
reduce f seed []     = seed
reduce f seed (x:xs) = reduce f (f seed x) xs

-- Reduces a list of elements to a single element using a binary reducing function and the first
-- element in the list as an initial seed value. Reduction is performed in a left associative way.
reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 f []     = error "reduce1 got an empty list"
reduce1 f (x:xs) = reduce f x xs

-- Reduces a list of elements to a single element and puts every intermediate step in a list from
-- left to right.
scan :: (a -> b -> a) -> a -> [b] -> [a]
scan f seed []      = [seed]
scan f seed (x:xs)  = (seed):scan f (f seed x) xs

-- Reduces a list of elements to a single element using a binary reducing function and an initial
-- seed value. Reduction is performed in a right associative way.
rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce f seed []     = seed
rreduce f seed (x:xs) = f x (rreduce f seed xs)

-- Reduces a list of elements to a single element using a binary reducing function and the first
-- element in the list as an initial seed value. Reduction is performed in a right associative way.
rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 _ []     = error "rreduce1 got an empty list"
rreduce1 f [x]    = x
rreduce1 f (x:xs) = f x (rreduce1 f xs)

-- Reduces a list of elements to a single element and puts every intermediate step in a list from
-- right to left.
rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan f seed []   = [seed]
rscan f seed list = reverse $ helper f seed (reverse list)
        where helper f seed []     = [seed]
              helper f seed (x:xs) = (seed):helper f (f x seed) xs

---------------------------------------------------------------------------------------------------
--- PROBLEM 3
---------------------------------------------------------------------------------------------------
type Tolerance = Double

-- Computes the square of a given number using newton's method by checking a provided tolerance value.
-- The initial guess of the result is set to be 1.
newton :: Tolerance -> Double -> Double
newton tol num
    | num<0     = error "can't get sqrt of a negative number"
    | otherwise = helper num tol 1
    where helper num tol guess
            | (satisfyingGuess (improveGuess num guess) guess tol) = guess
            | otherwise = helper num tol (improveGuess num guess)

-- Takes the initial value and current guess and improves the guess of the square.
improveGuess :: Double -> Double -> Double
improveGuess x y = (y+x/y)/2

-- Checks if two guessess are within some tolerance level of each other.
satisfyingGuess :: Double -> Double -> Tolerance -> Bool
satisfyingGuess guess x tolerance = (abs $ x-guess) <= tolerance

-- A very simplistic way of calculating the derivative of some single variable function by assuming the dx
-- is some very small number 1e-5
deriv f x = (f(x+dx) - f(x))/dx
    where dx = 1e-5
---------------------------------------------------------------------------------------------------
--- PROBLEM 4
---------------------------------------------------------------------------------------------------
type Operators = [(Char, Int -> Int -> Int)]

basic :: [(Char, Int->Int->Int)]
basic = [ ('+', (+))
        , ('-', (-))]
standard :: [(Char, Int->Int->Int)]
standard = [ ('+', (+))
           , ('-', (-))
           , ('*', (*))
           , ('/', div)
           , ('^', (^))]

-- Given a name and a list of tuples containing single character names and the actual binary function,
-- this function attempts to locate and retrieve the function in the list witch matches the provided name.
-- The list of function should contain unique elements.
-- Returns an error if no such function name is found in the list. 
getFunction :: Char -> [(Char, Int -> Int -> Int)] -> (Int -> Int -> Int)
getFunction operator operators = getFunction index operators
            where index                          = elemIndex operator (map (fst) operators)
                  getFunction Nothing  _         = error $ "Invalid symbol "++[operator,'.']
                  getFunction (Just x) operators = snd $ operators!!x

-- Given a string representation of a reverse polish notation mathematical expression this function calculates
-- the result of said expression. The second argument is a list of supported binary operators in the expression.
-- If the expression contains an undefined operator the user is notified.
-- If the expression is malformed the user is notified.
-- This function only works on single digit operands and single character operator names.
rpnCalc :: String -> Operators -> Int
rpnCalc ""     operators = 0
rpnCalc string operators = head $ reduce function [] string
                    where function xs@(x:y:ys) c
                            | isDigit c = (digitToInt c):xs
                            | otherwise = (getFunction c operators y x):ys
                          function xs c
                            | isDigit c = (digitToInt c):xs
                            | otherwise = error "Malformed RPN expression"
---------------------------------------------------------------------------------------------------
--- PROBLEM 4
---------------------------------------------------------------------------------------------------
-- Recursively calculates the minimum number of steps from the frog game using the simple recurrence relation
-- f(0) = 0
-- f(1) = 2
-- f(n) = 3f(n-1)+2, n>=0
frogJumps :: Int -> Integer
frogJumps n = if n<0 then error "Negative input is not allowed" else frogJumps' 0 n
    where frogJumps' s 0 = s
          frogJumps' s n = frogJumps' (3*s+2) (n-1) 

-- Explicitily calculates the minimum number of steps from the frog game using the solution of the recurrence relation
-- given that the initial conditions are f(1) = 2.
frogJumpExplicit :: Integer -> Integer
frogJumpExplicit n = if (n<0) then error "Negative input is not allowed" else 3^n-1
                  