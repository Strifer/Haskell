-- EXERCISE 1 --------------------------------------------------------------------------------------

--1.1
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

--1.2
headsOf :: [[a]] -> [a]
headsOf [] = []
headsOf (xs:xss) = head xs : headsOf xss

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

-- EXERCISE 3 --------------------------------------------------------------------------------------

--3.1
equalTriplets :: Eq a => [(a,a,a)] -> [(a,a,a)]
equalTriplets []    = []
equalTriplets ((a,b,c):xs) |  (a == b) && (b == c) = (a,b,c) : equalTriplets xs
                           |  otherwise            = equalTriplets xs

--3.2
replicate' 1 xs = [xs]
replicate' n xs
    | n<0       = []
    | otherwise = xs : (replicate' (n-1) xs)

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
-- I implemented it so that the indices are counted from 1 with 1 being the first element.
takeFromTo :: Int -> Int -> [a] -> [a]
takeFromTo n1 n2 list@(x:xs)
    | n1 < 0 || n2 < 0 = error "Indices must be positive"
    | n1 > n2          = reverse $ takeFromTo' n2 n1 1 list
    | otherwise = takeFromTo' n1 n2 1 list
        where takeFromTo' _ _ count [] = []
              takeFromTo' n1 n2 count (x:xs)
                    | count < n1 = takeFromTo' n1 n2 (count+1) xs
                    | (count >= n1) && (count < n2) = x : (takeFromTo' n1 n2 (count+1) xs)
                    | count == n2 = [x]
                    | otherwise = []

-- EXERCISE 5 --------------------------------------------------------------------------------------

--5.1
eachThird :: [a] -> [a]
eachThird (x:y:z:xs) = z : eachThird xs
eachThird _ = []

--5.2
crossZip :: [a] -> [a] -> [(a,a)]
crossZip  []         _         = []
crossZip  _          []        = []
crossZip (x1:[])     _         = []
crossZip  _         (y1:[])    = []
crossZip (x1:x2:xs) (y1:y2:ys) = (x1,y2):((x2,y1):(crossZip xs ys))

-- EXERCISE 6 --------------------------------------------------------------------------------------

--6.1
length' :: [a] -> Int
length' xs = length2 xs 0
    where length2 []     s = s
          length2 (x:xs) s = length2 xs (s+1)


--6.2
maxUnzip :: [(Int,Int)] -> (Int,Int)
maxUnzip    []  = error "empty list"
maxUnzip (x@(n1,n2):xs) = maxAcum (n1, n2) xs
        where maxAcum (x,y)  []               = (x,y)
              maxAcum (x,y) (pair@(n1,n2):xs) = maxAcum (max x n1, max y n2) xs

maxUnzip' :: [(Int,Int)] -> (Int,Int)
maxUnzip' []        = error "empty list"
maxUnzip'  [(n1,n2)] = (n1,n2)
maxUnzip'  (x@(n1,n2):xs)    = (n1 `max` (fst $ maxUnzip' xs), n2 `max` (snd $ maxUnzip' xs))


