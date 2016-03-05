module CSVUtils
    ( parseCSV
    , showCSV
    , colFields
    , readCSV
    , writeCSV
    , Separator
    , Document
    , CSV
    , Entry
    , Field ) where


import Data.List.Split
import System.IO

type Separator  = String
type Document   = String
type CSV        = [Entry]
type Entry      = [Field]
type Field      = String

doc = "John;Doe;15\nTom;Sawyer;12\nAnnie;Blake;20"
brokenDoc = "One;Two\nThree;Four;Five"
csv = parseCSV ";" doc

-- Takes a list of lists and checks if all the sublists are of the same length.
checkLengths :: [[a]] -> Bool
checkLengths xss@((xs):_) = and $ map (predicate) xss
    where predicate x = (length x ) == (length $ xs)

-- Takes some delimiter and a list of Strings and makes up a single String putting the delimiter between each element.
-- concatWithDelimiter [String1, String2, String3] d => String1dString2dString3
concatWithDelimiter :: [[a]] -> [a] -> [a]
concatWithDelimiter [x] separator = x
concatWithDelimiter [] separator = []
concatWithDelimiter wordList@(x:xs) separator = x++separator++(concatWithDelimiter xs separator)

-- Takes a separator and a string representing a
-- CSV document and returns a CSV representation of the document.
parseCSV :: Separator -> Document -> CSV
parseCSV separator document
    | (length $ head list) == 1 = error $ "The separator '"++separator++"' does not occur in the text"
    | checkLengths list == False = error "The CSV-file is not well formed"
    | otherwise = list
    where list = [splitOn separator line | line <- lines document]

-- Takes a separator and a CSV representation of
-- a document and creates a CSV string from it.
showCSV :: Separator -> CSV -> Document
showCSV separator [] = error "The CSV file word list is empty"
showCSV separator wordList
    | checkLengths wordList == False = error "The CSV file is not well-formed"
    | otherwise = unlines connectedList
    where connectedList = [concatWithDelimiter line separator | line <- wordList]


-- Takes a CSV document and a field number
-- and returns a list of fields in that column.
colFields :: Int -> CSV -> [Field]
colFields number [] = []
colFields number csvList
    | checkLengths csvList == False = error "The CSV-file is not well formed"
    | number > length csvList-1 = error $ "There is no column "++(show number)++" in the CSV document"
    | otherwise = [line!!number | line <- csvList]

-- Takes a file path and a separator and
-- returns the CSV representation of the file (wrapped due to impurity).
readCSV :: Separator -> FilePath -> IO CSV
readCSV separator file = do document <- readFile file
                            return $ parseCSV separator document

-- Takes a separator, a file path, and a CSV
-- document and writes the document into a file.
writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV separator file csvList = do  writeFile file (showCSV separator csvList)
