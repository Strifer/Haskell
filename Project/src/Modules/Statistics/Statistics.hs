module Modules.Statistics.Statistics ( Bucket (Bucket, rangeMin, rangeMax, count)
                                     , Statistics (Statistics, minPossible, maxPossible, mean, median, minAchieved, maxAchieved, histogram)
                                     , Score (Score, points, passed)
                                     , UserScore (UserScore, identifier, userScore)
                                     , stats
                                     , typeStats
                                     , assignmentStats
                                     , score
                                     , typeScore
                                     , assignmentScore
                                     , ranked
                                     , typeRanked
                                     , assignmentRanked
                                     ) where

import Modules.AssignmentsAndSubmissions.AssignmentsAndSubmissions
import Modules.Reviews.Reviews
import Data.List

-- | Assignment constant used for testing
testAssignment = Assignment 2015 Homework 5

-- | A list of user scores constant used for testing
x = [UserScore {identifier = "Branko", userScore = Score {points = 39.5, passed = True}},UserScore {identifier = "Ivana", userScore = Score {points = 36.2, passed = False}},UserScore {identifier = "Stipe", userScore = Score {points = 30.0, passed = False}}]


---------------------------------------------------------------------------------------------------------------------------------------------------------
-- | This data structure represents a single bucket inside a histogram.
data Bucket = Bucket { rangeMin :: Double -- left border of the buckets range
                     , rangeMax :: Double -- right border of the buckets range
                     , count :: Int       -- how many elements inside the bucket
                     } deriving Show

-- | This data structures represents statistical data built from assignments scores.
data Statistics = Statistics { minPossible :: Double -- minimal possible score
                             , maxPossible :: Double -- maximal possible score
                             , mean :: Double        -- arithmetic mean of the scores
                             , median :: Double      -- median score
                             , minAchieved :: Double -- minimal achieved score
                             , maxAchieved :: Double -- maximum achieved score
                             , histogram :: [Bucket] -- histogram data consisting of buckets
                             } deriving Show

-- | This record represents a single Score achievable by students.
data Score = Score { points :: Double -- points achieved by some student
                   , passed :: Bool   -- marks if the student has passed the class
                   } deriving (Eq, Ord, Show)

-- | This record represents a score tied to some user. Score data is  built from reviews.
data UserScore = UserScore { identifier :: UserIdentifier -- name of the user
                           , userScore :: Score           -- user's score
                           } deriving (Eq, Ord, Show)

---------------------------------------------------------------------------------------------------------------------------------------------------------


-- | Averages up a list of numbers
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs


-- | Calculates the number of buckets inside a histogram based on sturge's formula
calculateBuckets :: Num a => [a1] -> a
calculateBuckets xs = fromInteger $ ceiling $ (logBase 2 (genericLength xs)) + 1

-- | Counts all unique elements inside a list of doubles.
countUniqueElements :: [(Double, Double)] -> [((Double, Double), Int)]
countUniqueElements = map (\xs@(x:_) -> (x, length xs)) . group . sort


-- | Makes a histogram based on a list of user scores.
makeHistogramBuckets :: [UserScore] -> [Bucket]
makeHistogramBuckets userScores   = map bucketMaker $ countUniqueElements $ map (foo . points . userScore) userScores
    where   numOfBuckets          = calculateBuckets userScores
            p                     = calculateMinAndMaxAchieved userScores
            minX                  = fromInteger $ floor $ fst p
            maxX                  = fromInteger $ ceiling $ (snd p + 1)
            ranges                = [minX, (minX + (maxX - minX)/numOfBuckets) .. maxX]
            rangesPairs           = zip ranges $ tail ranges
            foo z = head $ filter (\(x,y) -> z>=x && z<=y) rangesPairs
            bucketMaker ((m,bigM),c) = Bucket m bigM c


-- | Calculates the median value in a list of numbers.
median' :: (Fractional a, Real a) => [a] -> a
median' xs
    | odd genLength  = sorted !! halfLength
    | otherwise      = average $ (sorted !! (halfLength-1)):(sorted !! (halfLength)):[]
    where sorted     = sort xs
          genLength  = genericLength xs
          halfLength = genLength `div` 2

-- | Makes statistics on an entire academic year.
stats :: Integer -> IO Statistics
stats y = do
    assignments <- assignmentsByYear y
    users       <- mapM listSubmissions assignments
    confs       <- mapM getConfiguration assignments
    userScores  <- mapM (score y) (nub $ concat users)
    let possibles = calculateMinAndMaxPossible confs
    let achieved  = calculateMinAndMaxAchieved userScores
    return $ Statistics (fst possibles) (snd possibles) (calculateMean userScores) (calculateMedian userScores) (fst achieved) (snd achieved) (makeHistogramBuckets userScores)

-- | Makes statistics on all assignments of a certain type in an academic year.
typeStats :: Integer -> Type -> IO Statistics
typeStats y t = do
    assignments <- assignmentsByYearAndType y t
    users       <- listSubmissions $ head assignments
    confs       <- mapM getConfiguration assignments
    userScores  <- mapM (typeScore y t) users
    let possibles = calculateMinAndMaxPossible confs
    let achieved  = calculateMinAndMaxAchieved userScores
    return $ Statistics (fst possibles) (snd possibles) (calculateMean userScores) (calculateMedian userScores) (fst achieved) (snd achieved) (makeHistogramBuckets userScores)

-- | Makes statistics on a certain assignment.
assignmentStats :: Assignment -> IO Statistics
assignmentStats a = do
    users      <- listSubmissions a
    userScores <- mapM (assignmentScore a) users
    conf       <- getConfiguration a
    let possibles = calculateMinAndMaxPossible [conf]
    let achieved  = calculateMinAndMaxAchieved userScores
    return $ Statistics (fst possibles) (snd possibles) (calculateMean userScores) (calculateMedian userScores) (fst achieved) (snd achieved) (makeHistogramBuckets userScores)

-- | Calculates minimal and maximum possible scores based on a list of configurations.
calculateMinAndMaxPossible :: [Configuration] -> (Double, Double)
calculateMinAndMaxPossible configs = (sum $ map minScore configs, sum $ map maxScore configs)

-- | Calculates minimal and maximum achieved scores based on a list of scores.
calculateMinAndMaxAchieved :: [UserScore] -> (Double, Double)
calculateMinAndMaxAchieved userScores = (minimum $ map (points . userScore) userScores, maximum $ map (points .userScore) userScores)

-- | Calculates the mean score.
calculateMean :: Fractional a => [UserScore] -> a
calculateMean userScores = average $ map (points . userScore) userScores

-- | Calculates the median score.
calculateMedian :: [UserScore] -> Double
calculateMedian userScores = median' $ map (points . userScore) userScores

-- | Calculates a user score based on all the assignments he participated in.
calculateScore :: [Assignment] -> UserIdentifier -> IO UserScore
calculateScore assignments uid = do
    scores      <- mapM (flipAssScore uid) assignments
    let totalScore = sum $ map (points . userScore) scores
    let passed     = foldl (&&) True (map (getPassed) scores)
    return $    UserScore uid (Score totalScore passed)
    where flipAssScore uid a = assignmentScore a uid
          getPassed = passed . userScore

-- | Calculates the score for the entire year for a single user. (e.g. total score in 2015)
score :: Integer -> UserIdentifier -> IO UserScore
score y uid = do
    assignments <- assignmentsByYear y
    calculateScore assignments uid

-- | Calculates the score for a single assignment type in a year for a single user. (e.g. all homeworks in 2015)
typeScore :: Integer -> Type -> UserIdentifier -> IO UserScore
typeScore y t uid = do
    assignments <- assignmentsByYearAndType y t
    calculateScore assignments uid


-- | Calculates the score for a single assignment in a year for a single user. (e.g. Homework5 in 2015)
assignmentScore :: Assignment -> UserIdentifier -> IO UserScore
assignmentScore a uid = do
    conf <- getConfiguration a
    let reqScore = required conf
    revs <- reviewsForUserByRole a uid Staff
    case revs of
        []      -> error "No available staff reviews for this user"
        x:[]     -> do
                    let passed = revScore x >= reqScore
                    return $ UserScore uid (Score (revScore x) passed)
        x:xs  -> error "Multiple staff reviews found. Might not be allowed"

-- | Compares two scores
compareScores :: UserScore -> UserScore -> Ordering
compareScores s1 s2 = compare (points $ userScore s2) (points $ userScore s1)

-- | Fetches the ranked users for a specific assignment, sorted
-- | in descending order by score.
assignmentRanked :: Assignment -> IO [UserScore]
assignmentRanked a = do
    users      <- listSubmissions a
    userScores <- mapM (assignmentScore a) users
    return $ sortBy compareScores userScores

-- | Fetches the ranked users for an assignment type in AY,
-- | sorted in descending order by score.
typeRanked :: Integer -> Type -> IO [UserScore]
typeRanked y t = do
    assignments    <- assignmentsByYearAndType y t
    users          <- listSubmissions $ head assignments
    userScores     <- mapM (typeScore y t) users
    return $ sortBy compareScores userScores

-- | Fetches the ranked users for an entire AY, sorted in
-- | descending order by score.
ranked :: Integer -> IO [UserScore]
ranked y = do
    assignments    <- assignmentsByYear y
    users          <- listSubmissions $ head assignments
    userScores     <- mapM (score y) users
    return $ sortBy compareScores userScores