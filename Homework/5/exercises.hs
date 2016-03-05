import Data.Char
import Data.List
import Control.Monad

---------------------------- LESSON 6 -----------------------------
--- Exercise 1.1
takeThree = take 3
dropThree = drop 3
hundredTimes = take 100 . repeat

--- Exercise 1.2
index = zip [0..]
index' = (`zip` [0..])

--- Exercise 1.3
divider :: Int -> [Char]
divider = (`take` repeat '=')
--------------------------------------------------------------------
--- Exercise 2.1
applyOnLast :: (a -> b -> c) -> [a] -> [b] -> c
applyOnLast f xs ys = f (last xs) (last ys)


addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

lastTwoPlus100 :: [Integer] -> [Integer] -> Integer
lastTwoPlus100 xs ys = addThree 100 0 (applyOnLast (+) xs ys)

--- Exercise 2.2
applyManyTimes :: (Num a, Ord a) => a -> (b -> b) -> b -> b
applyManyTimes n f x
    | n <= 0 = x
    | otherwise = applyManyTimes (n-1) f (f x)

applyTwice = applyManyTimes 2
-------------------------------------------------------------------
--- Exercise 3.1
listifyList :: [a] -> [[a]]
listifyList = map (listify)
    where listify x = [x]

--- Exercise 3.2
cutoff :: Int -> [Int] -> [Int]
cutoff n xs = map (cut n) xs
    where cut n x
              | x > n = n
              | otherwise = x
-------------------------------------------------------------------
--- Exercise 4.1
sumEvenSquares :: [Integer] -> Integer
sumEvenSquares = sum . map (^2) . filter (even)

--- Exercise 4.2
freq :: Eq a => a -> [a] -> Int
freq x xs = length $ filter (==x) xs

--- Exercise 4.3
freqFilter :: Eq a => Int -> [a] -> [a]
freqFilter n xs = filter (`condition` xs) xs
    where condition x xs = freq x xs == n
-------------------------------------------------------------------
--- Exercise 5.1
withinInterval n m xs = filter (\x -> x>=n && x<=m) xs

--- Exercise 5.2
sndColumn :: [[a]] -> [a]
sndColumn = map (\x -> x!!1)

--- Exercise 5.3
canonicalizePairs :: Ord a => [(a, a)] -> [(a, a)]
canonicalizePairs = map (\(x,y) -> if (x>y) then (y,x) else (x,y)) . filter (\(x,y)->x/=y)


---------------------------- LESSON 7 -----------------------------
--- Exercise 1.1
sumEven :: [Integer] -> Integer
sumEven = sum . map snd . filter (even . fst) . zip [0..]

--- Exercise 1.2
filterWords :: [String] -> String -> String
filterWords ws = unwords . filter (`notElem` ws) . words

--- Exercise 1.3
initials3 :: String -> (String -> Bool) -> String -> String
initials3 d p = concat . map (:d) . map (toUpper . head) . filter p . words

initials' = initials3 "." (\x -> x == x)
-------------------------------------------------------------------
--- Exercise 2.1
maxDiff :: [Int] -> Int
maxDiff xs = maximum . map (uncurry (-)) $ zip xs (tail xs)

minDiff :: [Int] -> Int
minDiff xs = minimum . map (uncurry (-)) $ zip xs (tail xs)

maxMinDif :: [Int] -> (Int, Int)
maxMinDif xs = (minDiff xs, maxDiff xs)

--- Exercise 2.2
studentsPassed xs   = map fst $ filter (predicate) xs
    where maxScore  = maximum $ map snd xs
          predicate x =  (snd x) > 0.5*maxScore
-------------------------------------------------------------------
--- Exercise 3.1
isTitleCased :: String -> Bool
isTitleCased s = all (isCapital . head) (words s)
    where isCapital x = toUpper x == x


--- Exercise 3.2
sortPairs xs = sortBy f xs
    where f (x1,y1) (x2,y2) = compare y1 y2

--- Exercise 3.3
filename :: String -> String
filename s = drop (1 + (last $ findIndices (=='/') s)) s

--- Exercise 3.4
maxElemIndices :: Ord a => [a] -> [Int]
maxElemIndices [] = []
maxElemIndices xs = findIndices (== maximum xs) xs
-------------------------------------------------------------------
--- Exercise 4.1
elem' a = foldr (||) False . map (==a)

--- Exercise 4.2
reverse' :: [a] -> [a]
reverse' = flip (foldr (flip (.) . (:)) id) []

--- Exercise 4.3
nubRuns :: Eq a => [a] -> [a]
nubRuns xs = foldr (\a b -> if a == (head b) then b else a:b) [last xs] xs
--------------------------------------------------------------------
--- Exercise 5.1
reverse'' :: [a] -> [a]
reverse'' xs = foldl (\acc x-> x : acc) [] xs

--- Exercise 5.2
sumEven' :: [Integer] -> Integer
sumEven' = foldl (\a (x,y) -> if (even x) then (a+y) else (a+0)) 0 . zip [0..]

--- Exercise 5.3
maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip [] = error "empty list"
maxUnzip xs  = foldl (\(x1,y1) (x2,y2) -> ((max x1 x2),(max y1 y2))) (minBound,minBound) xs

---------------------------- LESSON 8 -----------------------------
--- Exercise 1.1
data Date = Date Int Int Int deriving Show
showDate :: Date -> String
showDate (Date d m y) = show d ++ ['.'] ++ show m ++ ['.'] ++ show y ++ ['.']

--- Exercise 1.2
data Point = Point Double Double deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point deriving Show

translate :: Point -> Shape2 -> Shape2
translate (Point x1 y1) (Circle2 (Point x y) r) = Circle2 (Point (x1+x) (y1+y)) r
translate (Point x1 y1) (Rectangle2 (Point x y) (Point x' y')) = Rectangle2 (Point (x1+x) (y1+y)) (Point (x1+x') (y1+y'))

--- Exercise 1.3
inShape :: Shape2 -> Point -> Bool
inShape (Circle2 (Point cx cy) r) (Point x y) = (x-cx)^2 + (y-cy)^2 <= r^2
inShape (Rectangle2 (Point x1 y1) (Point x2 y2)) (Point x y) = x >= minx && x <= maxx && y >= miny && y <= maxy
    where minx = min x1 x2
          maxx = max x1 x2
          maxy = max y1 y2
          miny = min y1 y2

inShapes :: [Shape2] -> Point -> Bool
inShapes xs p = any (`inShape` p) xs

--- Exercise 1.4
data Vehicle = Car String Double | Truck String Double | Motorcycle String Double | Bicycle deriving Show
totalHorsepower :: [Vehicle] -> Double
totalHorsepower = sum . map horsepower
    where horsepower (Bicycle) = 0.2
          horsepower (Car _ power) = power
          horsepower (Truck _ power) = power
          horsepower (Motorcycle _ power) = power
-------------------------------------------------------------------
data Level    = Bachelor | Master | PhD deriving (Show,Eq)
data Student = Student
  { firstName  :: String
  , lastName   :: String
  , studentId  :: String
  , level      :: Level
  , avgGrade   :: Double } deriving Show


--- Exercise 2.1
improveStudent :: Student -> Student
improveStudent s@(Student {avgGrade = g}) = s{avgGrade = min 5 (g+1)}

--- Exercise 2.2
avgGradePerLevels :: [Student] -> (Double,Double,Double)
avgGradePerLevels xs = (avgGradeLevel Bachelor xs, avgGradeLevel Master xs, avgGradeLevel PhD xs)
average xs = realToFrac (sum xs) / genericLength xs
avgGradeLevel lev studentList = average $ map (avgGrade) (filter ((==lev) . level) studentList)

--- Exercise 2.3
rankedStudents :: Level -> [Student] -> [String]
rankedStudents lev = map (studentId) . sortBy (compareStudent) . filter ((==lev) . level)
compareStudent s1@(Student {avgGrade = g1}) s2@(Student {avgGrade = g2}) = compare g2 g1

--- Exercise 2.4
addStudent :: Student -> [Student] -> [Student]
addStudent s@(Student {studentId = sid}) xs
    | elem sid $ map studentId xs = error "student already exists"
    | otherwise = s:xs
-------------------------------------------------------------------
--- Exercise 3.1
data MyTriplet a b c = MyTriplet
    { first  :: a
    , second :: b
    , third  :: c} deriving Show
toTriplet :: MyTriplet a b c -> (a,b,c)
toTriplet trip@(MyTriplet a b c) = (first trip, second trip, third trip) 

--- Exercise 3.2
data Employee = Employee
   { name   :: String
   , salary :: Maybe Double } deriving Show
totalSalaries :: [Employee] -> Double
totalSalaries employees = sum $ map (safeSalary . salary) employees
    where safeSalary (Just x) = x
          safeSalary _        = 0

--- Exercise 3.3
addStudent2 :: Student -> [Student] -> Maybe [Student]
addStudent2 s@(Student {studentId = sid}) xs
    | elem sid $ map studentId xs = Nothing
    | otherwise = Just (s:xs)

addStudent3 :: Student -> [Student] -> Either String [Student]
addStudent3 s@(Student {studentId = sid}) xs
    | elem sid $ map studentId xs = Left "student already exists"
    | otherwise = Right (s:xs)

---------------------------- LESSON 9 -----------------------------
--- Exercise 1.2
data Sex = Male | Female deriving (Show,Read,Ord,Eq)
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

parentCheck :: Person2 -> Bool
parentCheck p = (forename2 p) `elem` (map forename2 (childrenM ++ childrenF))
    where childrenM = maybe [] children2 (mother2 p)
          childrenF = maybe [] children2 (father2 p)

------------------------------------------------------------------------
data MyList a = Empty | Cons a (MyList a) deriving (Show,Read,Ord,Eq)

--- Exercise 2.1
listHead :: MyList a -> Maybe a
listHead Empty = Nothing
listHead (a `Cons` list) = Just a

--- Exercise 2.2
listMap :: (a -> b) -> MyList a -> MyList b
listMap f Empty = Empty
listMap f (a `Cons` list) = (f a) `Cons` (listMap f list)
-------------------------------------------------------------------------
data Tree a = Null | Node a (Tree a) (Tree a) deriving (Show,Eq)

intTree :: Tree Int
intTree = Node 88 (Node 2 Null Null) (Node 3 Null Null)

--- Exercise 3.1
treeMax :: Ord a => Tree a -> a
treeMax (Node a Null Null)  = a
treeMax (Node a left right) = max a (max (treeMax left) (treeMax right))

--- Exercise 3.2
treeToList :: Ord a => Tree a -> [a]
treeToList Null = []
treeToList (Node elem Null Null)  = [elem]
treeToList (Node elem left right) = treeToList left ++ [elem] ++ treeToList right

--- Exercise 3.3
levelCut :: Int -> Tree a -> Tree a
levelCut level tree = treeAcc 0 tree
    where treeAcc lev tree@(Node x left right)
            | lev==level = Node x Null Null
            | otherwise  = Node x (treeAcc (lev+1) left) (treeAcc (lev+1) right)
-----------------------------------------------------------------------------
treeInsert :: Ord a => a -> Tree a -> Tree a
treeInsert x Null = Node x Null Null
treeInsert x t@(Node y l r) 
   | x < y     = Node y (treeInsert x l) r
   | x > y     = Node y l (treeInsert x r)
   | otherwise = t

--- Exercise 4.1
listToTree :: Ord a => [a] -> Tree a
listToTree xs = foldl (\t x -> treeInsert  x t) Null xs