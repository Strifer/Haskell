
import Data.Char
import Data.List


---------------------------------------------------------------------------------------------------
-- 1ST PROBLEM
---------------------------------------------------------------------------------------------------
norm (x, y) = sqrt (x*x+y*y)

normalize (x, y)
    | norm (x,y) == 0 = error "Cannot normalize null vector"
    | otherwise = (x / norm (x,y), y / norm (x,y))

scalarMult a (x, y) = (a*x, a*y)

dot (x, y) (x', y') = x*x' + y*y'

cos' (x, y) (x', y')
    | norm (x, y)   == 0 = error "Null vector given"
    | norm (x', y') == 0 = error "Null vector given"
    | otherwise          = dot (x, y) (x', y') / ( norm(x, y) * norm(x', y') )

floatEq x y = abs (x-y) <= 1E-15

areParallel (x, y) (x', y') = cos' (x, y) (x', y') `floatEq` 1
---------------------------------------------------------------------------------------------------
-- 2ND PROBLEM
---------------------------------------------------------------------------------------------------

--Returns -1 if the first pair < second pair
--         1 if the first pair > second pair
--         0 if the first pair == second pair
compareTuple (name1, score1) (name2, score2)
    | score1>score2 = 1
    | score1==score2 = if name1==name2 
                            then 0 
                            else if name2<name1 
                                then (-1) 
                                else (1)
    | score1<score2 = (-1)

--Finds the smaller of two pairs.
minTuple x y
    | x `compareTuple` y ==   1  = y
    | x `compareTuple` y == (-1) = x
    | otherwise = x

--Finds the bigger of two pairs.
maxTuple x y
    | x `compareTuple` y ==   1  = x
    | x `compareTuple` y == (-1) = y
    | otherwise = x


--Finds the biggest of three pairs
biggestPair x y z = x `maxTuple` y `maxTuple` z

--Finds the middle of three pairs
middlePair x y z
    | maxTuple x y `compareTuple` z == -1 = maxTuple x y
    | maxTuple x z `compareTuple` y == -1 = maxTuple x z
    | maxTuple y z `compareTuple` x == -1 = maxTuple y z

--Finds the smallest of three pairs
smallestPair x y z = x `minTuple` y `minTuple` z

--Orders three pairs by their ranking, and names if they have equal ranking.
finalRanking x y z = [fst $ biggestPair x y z, fst $ middlePair x y z, fst $ smallestPair x y z]
---------------------------------------------------------------------------------------------------
--3RD PROBLEM
---------------------------------------------------------------------------------------------------

-- Counts the number of extensions inside a list of files.
countExtension files ext = length [w | w <- words files, ext `isSuffixOf` w]

--Creates a list representation of a folder grouped by extension.
--The list is in the format [number of mp3s, number of images, number of videos].
sortFilesByExtension files = [countExtension files ".mp3", countExtension files ".png", countExtension files ".mp4"]

--Sums all indices in a list except for the specified index (the first element is indexed as 1 in this implementation)
sumAllButIndex list index = sum [fst x | x <- zip list [1..], snd x /= index] 

--Calculates how many moves it would take to sort the files inside the folders if they are designated as defined in the permutation.
calculateMoves matrix permutation =  sum [sumAllButIndex (fst couple) (snd couple) | couple <- zip matrix permutation]

--Calculates how many moves it takes to sort our files for each possible designated name combination.
--Finds the minimum number of moves and takes that permutation as the new names for the folders.
--Returns a tuple in the form of ([moved file in each folder], [permutation which will signify the new folder names])
bruteForce matrix = minimum [(calculateMoves matrix permutation, permutation) | permutation <- permutations [1,2,3]]

--Decides the folder name based on its designated index.
folderName indices
    | indices == 1 = "audio"
    | indices == 2 = "image"
    | indices == 3 = "video"


main = do
    putStrLn ("Enter the the filenames contained in the folders:")
    folder1 <- getLine
    folder2 <- getLine
    folder3 <- getLine

    let files1 = sortFilesByExtension folder1
    let files2 = sortFilesByExtension folder2
    let files3 = sortFilesByExtension folder3
    
    let couple = bruteForce [files1, files2, files3]
    print $ fst couple
    putStr $ unlines $ map folderName (snd couple)
---------------------------------------------------------------------------------------------------
