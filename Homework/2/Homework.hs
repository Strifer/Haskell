import System.Environment
import Data.List
import Data.Tuple
----------------------------------------------------------------------------------------------------
-- PROBLEM 1
----------------------------------------------------------------------------------------------------
-- Checks if the matrix is well formed (all rows of equal length)
isWellFormed :: [[Int]] -> Bool
isWellFormed xss = [a | a <- xss, length a /= length (head xss)] == []

-- Returns the size of the matrix.
size :: [[Int]] -> (Int, Int)
size xss
    | isWellFormed xss = (rows, columns)
    | otherwise        = error "Matrix is malformed"
    where rows         = length xss
          columns      = length $ head xss

-- Returns specified matrix element.
getElement :: [[Int]] -> Int -> Int -> Int
getElement xss i j
    | i<0 || j < 0            = error "Out of Bounds"
    | i > rows || j > columns = error "Out of bounds"
    | not $ isWellFormed xss  = error "Matrix is malformed"
    | otherwise               = (xss!!i)!!j
    where rows                = fst $ size xss
          columns             = snd $ size xss

-- Adds up two matrices of equal size.
addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices xss yss
    | size xss /= size yss = error "Matrices are not of equal size"
    | otherwise            = [zipWith (+) (getRow xss i) (getRow yss i) | let n = (fst $ size xss), i <- [0..n-1]]

-- Returns the specified matrix row.
getRow :: [[Int]] -> Int -> [Int]
getRow xss i
    | not $ isWellFormed xss = error "Matrix is malformed"
    | i < 0 || i+1 > rows    = error "Out of bounds"
    | otherwise              = xss!!i
    where rows               = fst $ size xss

-- Returns the specified matrix column.
getCol :: [[Int]] -> Int -> [Int]
getCol xss j
    | not $ isWellFormed xss = error "Matrix is malformed"
    | j < 0 || j+1 > columns = error "Out of bounds"
    | otherwise              = [a!!j | a <- xss]
    where columns            = snd $ size xss

-- Transposes a matrix.
transpose' :: [[Int]] -> [[Int]]
transpose' xss = [getCol xss j | let n = snd $ size xss, j <-[0..n-1]]

-- Helper function for matrix multiplication.
-- Multiplies only a specific row with a matrix.
helperMult :: [Int] -> [[Int]] -> [Int]
helperMult xs yss = [sum $ zipWith (*) xs (getCol yss j) | let m = snd $ size yss, j <- [0..m-1]]

-- This function multiplies two matrices
-- zss = xss*yss
multMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multMatrices xss yss
    | (snd $ size xss) /= (fst $ size yss) = error "Incompatible matrix dimensions"
    | otherwise                            = [helperMult xs yss | xs <- xss]

-- This function calculates the sum of the matrix' diagonal.
diagonalSum :: [[Int]] -> Int
diagonalSum xss = sum [getElement xss i i | let max = snd $ size xss, i <- [0..max-1]]

-- This function calculates A^m where A is the matrix and m is the specified power.
-- Only works for positive powers.
powerMatrix :: (Int) -> [[Int]] -> [[Int]]
powerMatrix 1 m = m
powerMatrix n m
    | n>0 = multMatrices (powerMatrix (n-1) m) m
    | otherwise = error "The power must be positive"
----------------------------------------------------------------------------------------------------
-- PROBLEM 2
----------------------------------------------------------------------------------------------------
-- Counts the number of lines, words and characters in a String.
wc :: String -> (Int, Int, Int)
wc s = (length $ lines s, length $ words s, length s)

-- Combines two string lists with a padded \t
paste :: [String] -> [String] -> [String]
paste xss yss
    | length xss <= length yss = zipWith (pad) (xss ++ repeat "") (yss)
    | otherwise                = zipWith (pad) (xss) (yss ++ repeat "")
    where pad a "" = a
          pad "" a = a 
          pad a b  = a++"\t"++b


-- Splits a String around the provided character.
delimit :: String -> Char -> [String]
delimit "" _              =  []
delimit s c               =  cons (case break (== c) s of
                                    (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:s''   -> delimit s'' c))
  where
    cons ~(h, t)          =  h : t

-- Cuts a string around a provided delimiter and makes a list taking only the specified index
cut :: String -> Int -> [String] -> [String]
cut delimiter index xs = [(delimit x (head $ delimiter))!!(index-1) | x <- xs]

main = do
    xs <- getArgs
    let name = head xs
    if (name == "wc")
        then do s <- readFile (xs!!1)
                print $ wc s
        else if (name == "paste")
            then do s1 <- readFile (xs!!1)
                    s2 <- readFile (xs!!2)
                    putStr $ unlines $ paste (lines s1) (lines s2)
            else do s3 <- readFile (xs!!3)
                    putStr $ unlines $ cut (xs!!1) (read (xs!!2)) (lines s3)
----------------------------------------------------------------------------------------------------
-- PROBLEM 3
----------------------------------------------------------------------------------------------------
type Node = Int
type Edge = (Node, Node)


-- This function takes an edge list and returns a sorted edgeList padded with any implied but unlisted connections.
-- For example prepareList [(1,2),(2,3)] = [(1,2),(2,1),(2,3),(3,2)]
prepareList :: [Edge] -> [Edge]
prepareList edgeList = nub $ sort $ edgeList ++ (reverseEdges edgeList)
    where reverseEdges edgeList = [swap node | node <- edgeList]

-- Counts the number of nodes in a provided list of edges.
-- WARNING: applies only to graphs with no lonely (unconnected) nodes
countNodes :: [Edge] -> Int
countNodes edgeList = maximum $ maximum $ edgeList

-- Makes a single row in an adjacency matrix from a row of connections that only list the connection of one specific edge.
makeMatrixRow :: [Int] -> Int -> [Int]
makeMatrixRow row nodeCount = makeMatrixRowWrapper row 1 nodeCount
    where makeMatrixRowWrapper [] count n
            | count <= n = 0 : (makeMatrixRowWrapper [] (count+1) n)
            | otherwise  = []  
          makeMatrixRowWrapper (x:xs) count n
            | x == count = 1 : makeMatrixRowWrapper (xs) (count+1) n
            | otherwise  = 0 : makeMatrixRowWrapper (x:xs) (count+1) n

-- This function takes a list of edges, the number of nodes in a graph and returns
-- a list of lists where each row represents a single node and lists out to which other nodes it is connected
prepareRows :: [Edge] -> Int -> [[Int]]
prepareRows list max = [makeIndices i list |
                                        let makeIndices i list = [snd x | x <- list, fst x == i],
                                        i <- [1..max]]

-- Makes an adjacency matrix of a graph from a provided list of edges.
makeAdjacencyMatrix :: [Edge] -> [[Int]]
makeAdjacencyMatrix list = [makeMatrixRow row max | 
                                let max = countNodes list,
                                let preparedList = prepareRows (prepareList list) max,
                                row <- preparedList]

-- Calculates the number of distinct cycles of length 'len' in a graph.
-- The graph is represented as a list of edges, represented by tuples.
cyclicWalks :: [Edge] -> Int -> Int
cyclicWalks list len
    | len <= 0 = error "Cycle length must be greater than 1"
    | otherwise = diagonalSum $ powerMatrix len m
    where m = makeAdjacencyMatrix list


