import CSVUtils
---------------------------------------------------------------------------------------------------
--- PROBLEM 1
---------------------------------------------------------------------------------------------------
type Probability = Double
type DiscreteRandVar = [(Int, Double)]

-- Calculates the mean (the expected value) of a given random variable using normal recursion.
mean :: DiscreteRandVar -> Probability
mean [] = 0.0
mean ((x,p):xs) = (fromIntegral x)*p + mean xs

-- Calculates the mean (the expected value) of a given random variable using accumulator style recursion.
mean' :: DiscreteRandVar -> Double
mean' xs = mean1 xs 0
    where mean1 []                s = s
          mean1 (head@(x,p):tail) s = mean1 tail (s + p * (fromIntegral x))

-- Calculates the variance of a given random variable using normal recursion.
variance :: DiscreteRandVar -> Double
variance [] = 0.0
variance xs = varianceWrap xs (mean xs)
    where varianceWrap             [] meanx = 0.0 
          varianceWrap var@((x,p):xs) meanx = p*(fromIntegral x - meanx)^2 + varianceWrap xs meanx

-- Calculates the variance of a given random variable using accumulator style recursion
variance' :: DiscreteRandVar -> Double
variance' xs = variance1 xs (mean xs) 0
    where variance1             [] meanx s = s
          variance1 var@((x,p):xs) meanx s = variance1 xs meanx (s+p*(fromIntegral x - meanx)^2)

-- Takes a random variable and a probability and returns all values that have a greater or equal probability using normal recursion.
probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter _ [] = []
probabilityFilter a (v@(x,p):xs)
    | p>=a = x : (probabilityFilter a xs)
    | otherwise = probabilityFilter a xs

-- Takes a random variable and a probability and returns all values that have a greater or equal probability using accumulator style recursion.
probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' a xd = reverse $ probabilityFilter1 a xd []
    where   probabilityFilter1 a []         s = s
            probabilityFilter1 a ((x,p):xs) s
                | p>=a      = probabilityFilter1 a xs (x:s)
                | otherwise = probabilityFilter1 a xs s
---------------------------------------------------------------------------------------------------
--- PROBLEM 3
---------------------------------------------------------------------------------------------------
type Set a = [a]

-- Takes a list and makes a set by removing all duplicate elements.
setify :: Eq a => [a] -> Set a
setify [] = []
setify xs = setify1 xs []
    where setify1 [] s = s
          setify1 (x:xs) s
              | x `elem` s = setify1 xs s
              | otherwise = setify1 xs (x:s)

-- Inserts an element into a set.
insert' :: Eq a => Set a -> a -> Set a
insert' xs a = setify $ a:xs

-- Makes an union of two sets.
union' :: Eq a => Set a -> Set a -> Set a
union' xs ys = xs ++ [a | a <- ys, not $ a `elem` xs]

-- Makes an intersection of two sets.
intersection :: Eq a => Set a -> Set a -> Set a
intersection [] ys = []
intersection xs [] = []
intersection xs ys = [a | a<-xs, a `elem` ys]

-- Makes a difference of two sets.
difference :: Eq a => Set a -> Set a -> Set a
difference [] _ = []
difference xs [] = xs
difference xs ys = filter (predicate) xs
    where predicate x = not $ x `elem` ys
---------------------------------------------------------------------------------------------------
--- PROBLEM 4
---------------------------------------------------------------------------------------------------
-- Counts the number of times a given field occurs in the csv file.
countOccurrence :: (Eq a) => [a] -> a -> Int
countOccurrence csv field = length [x | x <- csv, field == x]

-- Drops the element whose index matches the provided index.
dropIndex :: (Enum b, Eq b, Num b) => b -> [a] -> [a]
dropIndex index xs = [fst x | x <- (zip xs [1..]), snd x /= index]

-- Calculates the apriori probabilities of "Yes" and "No" from the given csv file.
-- The result is returned as a tuple (P("Yes"), P("No"))
aPrioriProbabilities :: CSV -> (Double, Double)
aPrioriProbabilities csv = (fromIntegral (countOccurrence list "Yes") / l, fromIntegral (countOccurrence list "No") / l)
              where list = drop 1 (colFields 3 csv)
                    l    = fromIntegral $ length csv - 1

-- Calculates the conditional probabilities for a given field from the given csv file.
-- The result is returned as a tuple (P(field|"Yes"), P(field|"No"))
conditionalProbabilities :: (Fractional a) => CSV -> Field -> (a, a)
conditionalProbabilities csv field = (yesCondition / fromIntegral yesNumber, noCondition / fromIntegral noNumber)
                where yesCondition = fromIntegral $ length [x | x<-(drop 1 csv), let x' = dropIndex 4 x, field `elem` x', x!!3=="Yes"]
                      noCondition = fromIntegral $ length [x | x<-(drop 1 csv), let x' = dropIndex 4 x, field `elem` x', x!!3=="No"]
                      yesNumber = countOccurrence (colFields 3 csv) "Yes"
                      noNumber = ((length csv - 1) - yesNumber)

type Decision = String

-- Performs a naive bayes decision based on a provided csv data file and a given feature vector.
nbDecide :: CSV -> [String] -> Decision
nbDecide csv vector = if maxProbability==condVectorYes then "Yes" else "No"
            where condVectorYes = (fst $ aPrioriProbabilities csv) * product [fst $ conditionalProbabilities csv x | x <- vector]
                  condVectorNo  = (snd $ aPrioriProbabilities csv) * product [snd $ conditionalProbabilities csv x | x <- vector]
                  maxProbability = max condVectorYes condVectorNo

-- Performs naive bayes decision on each feature vector in provided list based on a provided csv data file.
nbDecideAll :: CSV -> [[String]] -> [Decision]
nbDecideAll csv vectors = [nbDecide csv vector | vector <- vectors] 