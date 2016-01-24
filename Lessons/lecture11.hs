import Control.Exception
import Data.Char
import Data.List
import qualified Data.Map as M
import Control.Monad
import System.IO
import System.Directory
import System.IO.Error
import System.Environment
import System.FilePath
import System.Random

--- IO ACTIONS ------------------------------------------------------------

-- Most programming we've done so far was puer functional programming.
-- This doesn't apply to IO operations. They're different because they 'must'
-- result in side effects. Input functions like reading from a keyboard 
-- are not consistent, output functions can change the state of the system.

-- We referred to them as actions.

-- IO a

-- IO String | Int | (Int, String) | IO ()

-- The last type is for actions that don't return anything, the parenthesis are
-- caled UNIT.

-- IO is a type constructor of '*->*' kind. It's like a box that stores actions 
-- that are not purely functional.
-- We pay important mind to seperating purely functional from impure (IO) code.
-- The IO type is also an instance of the Monad type class.

-- Here's hello world in Hasklel the meme language
-- main :: IO ()
-- putStrLn :: String -> IO ()

-- The function takes a String and returns an action (that outputs a string to the screen)
-- The above program can be compiled and ran.

-- The main function can invoke other functions that do wor
-- main
-- sayHelloToTheWorld = putStrLn "Hello, World!"
-- It's rare that we only do a single action, if we need to execute more actions we need to sequence
-- into one action. This is done with a special construct called 'do'

main2 = do
    putStrLn "here"
    putStrLn "we"
    putStrLn "go"
-- Statements in do blocks need to be aligned beneath ec other.
-- Main 2 executes three actions that are tied into a single action of a type IO ().
-- The actions are executed sequentially like in imperative language.

main3 = do
  putStrLn "Enter you lucky number"
  number <- getLine
  putStrLn $ "I guess your lucky number is " ++ number

-- The '<-' operator takes an action on its right side and a variable on te left side
-- It stores te return value of the action into the variable, it unwraps the return value of type
-- IO a into a value of type a. This can only be done in a do block of a function whose type is IO a.

-- Strings and IO Strings are not interchangable they need to be unwrapped with the arrow operator.
askNumber :: IO String
askNumber = do
  putStrLn "Enter your lucky number"
  getLine

main5 :: IO ()
main5 = do
  number <- askNumber
  putStrLn $ "Your lucky number is " ++ number

-- The return value of the entire do block is the result of the last action perormed.
-- The return value of ask Number is a string returned by the getline action

-----------------------------------------------------------------
--- EXERCISE 1
-----------------------------------------------------------------
main' :: IO ()
main' = do
    s1 <- getLine
    s2 <- getLine
    putStrLn $ (reverse s1) ++ (reverse s2)

threeNumbers :: IO ()
threeNumbers = do
    putStrLn "enter number 1"
    num1 <- getLine
    putStrLn "enter number 2"
    num2 <- getLine
    putStrLn "enter number 3"
    num3 <- getLine
    putStrLn $ show $ sum [read num1, read num2, read num3]
-----------------------------------------------------------------
---  RETURN
-----------------------------------------------------------------
askNumber2 :: IO String
askNumber2 = do
  putStrLn "Enter your lucky number"
  number <- getLine
  return $ number ++ "0"

-- The function 'return :: a -> IO a' gets a value and turns it into 
-- an action result.

-- Return doesn't need to be the last action it just wraps up a pure value
-- into an 'IO' type (ore generally into some Monad)

askNumber3 :: IO String
askNumber3 = do
  putStrLn "Enter your lucky number"
  number <- getLine
  return $ number ++ "0"
  getLine

-- This is silly but it illustrates the point. The return value of
-- of a do block is the value of the last action regardless if retur has appeared.

-- A name like wrap would have probably been a better coice.

askNumber7 :: IO String
askNumber7 = do
   putStrLn "Enter your lucky number"
   number <- getLine
   if number == "" then askNumber7 else return number 

-- Return recursion!

askNumber8 :: IO String
askNumber8 = do
  putStrLn "Enter your lucky number"
  number <- getLine
  if number == "" then do
    putStr "No input! "
    askNumber8 
  else return number 

-- Several actions in a branc require a do block. They need to be sequenced
-- into a single action.
-----------------------------------------------------------------
--- EXERCISE 2
-----------------------------------------------------------------
-- 2.1
threeStrings :: IO Int
threeStrings = do
    putStrLn "Enter string 1"
    s1 <- getLine
    putStrLn "Enter string 2"
    s2 <- getLine
    putStrLn "Enter string 3"
    s3 <- getLine
    let line = concat [s1,s2,s3]
    putStrLn line
    return (length line)

-- 2.2
askNumber9 :: IO Int
askNumber9 = do
    putStrLn "Enter your lucky number"
    number <- getLine
    if number == "" then do
        putStr "No input!"
        askNumber9
    else do
        let num = fromIntegral $ read number
        return num

main = do
    num <- askNumber9
    putStrLn $ show num

-- 2.3
askUser :: String -> (String -> Bool) -> IO String
askUser m p = do
    putStrLn m
    input <- getLine
    if (not (p input)) then do
        askUser m p
    else do
            return input


----------------------------------------------------------------------
--- WHERE & LET
----------------------------------------------------------------------
-- In IO actions we can use let to assign values to variables.
askName1 :: IO String
askName1 = do
  s1 <- getLine
  s2 <- getLine
  let forename = map toUpper s1
      lastname = map toUpper s2
  return $ forename ++ " " ++ lastname

-- You can also use a where block, but it has to be outside of a do block
askName4 :: IO String
askName4 = do
  s1 <- getLine
  s2 <- getLine
  return $ upperCase s1 ++ " " ++ upperCase s2
  where upperCase = map toUpper
-----------------------------------------------------------------------
--- COMMON IO FUNCTIONS
-----------------------------------------------------------------------
-- putStr :: String -> IO ()
-- putStrLn :: String -> IO ()
-- putChar :: Char -> IO ()
-- print :: Show a => a -> IO ()

-- Print can print any value that is a member of the Show type class.

-- In control monad we have a number of funny functions for managing actions
-- when :: Monad m => Bool -> m () -> m ()
-- sequence :: Monad m => [m a] -> m [a]
-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- forever :: Monad m => m a -> m b

-- IO is a member of the Monad type class so these will work on IO actions.

-- When function executes the provided action when the given condition is met, otherwise
-- it executes retur ()

main8 = do
    input <- getLine
    when (input == "") $
        putStrLn "Input is empty"

-- Sequence function takes a list of actions and returns a single action that
-- runs these actions in sequence and returns a list of their return values.

action1 = sequence [getLine, getLine]

action2 = do
    x1 <- getLine
    x2 <- getLine
    return [x1,x2]

main10 = do
    putStrLn "Top kek m8"
    xs <- sequence [getLine, getLine, getLine]
    putStrLn $ "Well meme'd m8s" ++ unwords xs

-- Sequence is cool for mapping actios on a list.
main12 = sequence $ map print [1..10]

main14 = do
    sequence $ map print [1..10]
    return ()

-- Sequence $ map is often used so there is a function that does this
main16 = mapM print [1..10]

main20 = do
  ys <- forM [1..10] $ \x -> do
    putStrLn $ "Input " ++ show x ++ "th number"
    y <- getLine
    return $ read y
  putStrLn $ "The sum is " ++ show (sum ys)



----------------------------------------------------------------------
--- EXERCISE 3
----------------------------------------------------------------------
-- 3.1
multiRead = do
    input <- getLine
    let num = read input
    xs  <- sequence $ take num $ repeat getLine
    sequence $ map putStrLn (reverse xs)
    return ()

--- 3.2
sequence' []     = []
sequence' (x:xs) = do
    x
    sequence' (xs)
----------------------------------------------------------------------
--- STREAMS
----------------------------------------------------------------------
-- Up until now we could read stdin line by line, and char by char.

-- With getChar
main22 :: IO ()
main22 = do
  c <- getChar
  putChar $ toUpper c
  eof <- isEOF
  if eof then return () else main22

-- With getLine
main23 :: IO ()
main23 = do
  l <- getLine
  putStrLn $ map toUpper l
  eof <- isEOF
  if eof then return () else main23

-- When we want to read all of the data it's better to work with streams.
-- A strema is a string that contains the whole input but lazyness saves us
-- from actually overloading the memory.

-- getContents :: IO String

-- Reads text and converts it into uppercase

main24 :: IO ()
main24 = do
  s <- getContents
  putStr $ map toUpper s

main25 :: IO ()
main25 = do
    s <- getContents
    putStr . take 10 $ map toUpper s

main26 :: IO ()
main26 = do
    s <- getContents
    putStr . unlines . take 10 . lines $ map toUpper s

main27 :: IO ()
main27 = do
    s <- getContents
    putStr . unlines . filter (not . null) $ lines s
----------------------------------------------------------------------
--- EXERCISE 4
----------------------------------------------------------------------
--- 4.1
filterOdd :: IO ()
filterOdd = do
    s <- getContents
    putStr . unlines . map (snd) . filter (odd . fst) . zip [0..] $ lines s

--- 4.2
numberLines :: IO ()
numberLines = do
    s <- getContents
    putStr . unlines . map (\(num, line) -> show num ++ " " ++ line) . zip [0..] $ lines s

----------------------------------------------------------------------
--- WORKING WITH FILES
----------------------------------------------------------------------
-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
-- This function takes care of closing a file if an error occurs.

cat5 :: String -> IO ()
cat5 f = withFile f ReadMode $ \h -> do
 s <- hGetContents h
 putStr . unlines . zipWith (\i l -> show i ++ ": " ++ l) [0..] $ lines s

cat6 :: String -> IO ()
cat6 f = do
 s <- readFile f
 putStr . unlines . zipWith (\i l -> show i ++ ": " ++ l) [0..] $ lines s


----------------------------------------------------------------------
--- EXERCISE 5
----------------------------------------------------------------------
--- 5.1
--- 5.2
----------------------------------------------------------------------

-- readFile :: FilePath -> IO String
-- writeFile :: FilePath -> String -> IO ()

-- readFile takes the name of a file and gives us a String of the file
-- writeFile takes a file and a string and writes to the file
-- Using these functions there is no need to explicitly open and maintain a file handle.

uppercaseFile :: FilePath -> IO ()
uppercaseFile f = do
  s <- readFile f
  putStr $ map toUpper s

interlaceFiles :: FilePath -> FilePath -> FilePath -> IO ()
interlaceFiles f1 f2 f3 = do
  s1 <- readFile f1
  s2 <- readFile f2
  writeFile f3 . unlines $ interlace (lines s1) (lines s2)
  where interlace xs ys = concat $ zipWith (\x1 x2 -> [x1,x2]) xs ys

-- Serialization of a data structure can be accomplished using these handy
-- functions.

-- Serialization example:
main30 :: IO ()
main30 = do
  let l = [(x,y) | x <- [0..100], y <- [x..100]]
  writeFile "list.txt" $ show l

-- Deserialization example:
main31 :: IO ()
main31 = do
  s <- readFile "list.txt"
  let l = read s :: [(Int,Int)]
  print l

-- When using read you have to specify the type value of what we are reading 
-- to make sure the string really parses like it should parse.

-- Serialization using plain text is inefficient so it's better to use
-- special packages like binary or beamable

-- Example of serialization, dictionary

type Dict = M.Map String String
dictFile = "dict.txt"

main11b :: IO ()
main11b = do
  d1 <- readDict
  d2 <- useDict d1
  writeFile dictFile $ show d2

readDict :: IO Dict
readDict = do
  e <- doesFileExist dictFile
  if e then do
    s <- readFile dictFile
    return $ read s 
  else return M.empty

useDict :: Dict -> IO Dict
useDict d = do
  putStrLn "Enter term: "
  w1 <- getLine
  if null w1 then return d else
    case M.lookup w1 d of
      Just w2 -> do
        putStrLn w2
        useDict d
      Nothing -> do
        putStrLn $ "No entry. How would you translate " ++ w1 ++ "?"
        w2 <- getLine 
        useDict $ M.insert w1 w2 d
----------------------------------------------------------------------
--- EXERCISE 6
----------------------------------------------------------------------
--- 6.1
wordTypes :: FilePath -> IO Int
wordTypes f = do
  s <- readFile f
  return (length $ nub $ words s)
----------------------------------------------------------------------
--- EXCEPTION HANDLING
----------------------------------------------------------------------

-- IO actions can cause exceptions so they have to be handled.
-- There are two functions used for handling this.

-- try :: Exception e => IO a -> IO (Either e a)
-- catch :: Exception e => IO a -> (e -> IO a) -> IO a

-- Either is like Maybe, the difference is that maybe has one value
-- wrapped in Just or just Nothing.
-- Either has two branches, left and right, the usual syntax is that
-- left is failure, right is sucess.

cat7 :: String -> IO ()
cat7 f = do
 r <- try $ cat6 f
 case r of 
   Left e  -> putStrLn $ "Error: " ++ ioeGetErrorString e
   _       -> return ()

cat8 :: String -> IO ()
cat8 f = catch (cat6 f) $ \e ->
  if isDoesNotExistError e then putStrLn "Error: someone stole the file"
  else ioError e  -- rethrowing the exception


----------------------------------------------------------------------
--- ENVIRONMENT VARIABLES
----------------------------------------------------------------------
--- Arguments can be passed via a command line. The following functions
-- recieve these arguments:
-- getProgName :: IO String
-- getArgs :: IO [String]

main33 :: IO ()
main33 = do
  xs <- getArgs
  x <- getProgName
  putStrLn $ "Program " ++ x ++ " is invoked with arguments " ++ show xs

main34 :: IO ()
main34 = do
  xs <- getArgs
  h <- case xs of
    (f:_) -> do e <- doesFileExist f
                if e then openFile f ReadMode else return stdin
    []    -> return stdin
  s <- hGetContents h
  putStr . unlines . sort $ lines s
----------------------------------------------------------------------
--- EXERCISE 7
----------------------------------------------------------------------

----------------------------------------------------------------------
--- RANDOM NUMBER GENERATOR
----------------------------------------------------------------------
-- The main and most general function is 

-- random :: (RandomGen g, Random a) => g -> (a,g)

-- Given a generator this function gives a random value and a new generator.
-- This is done because a specific randomgenerator will always output the same value.

-- In most cases we use a standard random generator called 'StdGen' which is an instance of RandomGen.
-- To create it we use this function

-- mkStdGen :: Int -> StdGen which takes a seed value

g = mkStdGen 13
(r1,g2) = random g :: (Int, StdGen)
(r2,g3) = random g2 :: (Int, StdGen)
(r3,g4) = random g3 :: (Int, StdGen)

-- randoms :: (RandomGen g, Random a) => g -> [a]
-- This function takes a randomgenerator and outputs an infinite list of random results

xs = randoms g :: [Float]
fiveCoins = take 5 $ randoms g :: [Bool]

-- randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
-- takes a range of values and a randomgenerator, returns a random value in that range and a new generator

-- randomRs :: (RandomGen g, Random a) => (a, a) -> g -> [a]
-- takes a range of values and a randomgenerator, returns an infinite list of random values in that range

fiveDice = take 5 $ randomRs (1,6) g :: [Int]

-- The problem is that we have to always drag the RNG because Haskell is pure.
-- This can be fixed if we use RNG in the IO monad.

-- We use this function for that

-- getStdGen :: IO StdGen

-- However we must split the RNG using 'newStdGen'

main36 :: IO ()
main36 = do
  g <- getStdGen
  putStrLn $ take 10 (randomRs ('a','z') g)
  g2 <- newStdGen
  putStrLn $ take 10 (randomRs ('a','z') g2)

-- A shorter way of doing this is using this function
-- getStdRandom :: (StdGen -> (a, StdGen)) -> IO a
-- It takes some function that takes a RNG and returns a new random value and a new RNG

main37 :: IO ()
main37 = do
  x <- getStdRandom (randomR (0,100)) :: IO Int
  print x

----------------------------------------------------------------------
--- EXERCISE 8
----------------------------------------------------------------------
--- 8.1
randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = x:(randoms' newGen)
  where (x, newGen) = random gen

--- 8.2
randomPositions :: Int -> Int -> Int -> Int -> IO [(Int, Int)]
randomPositions a1 b1 a2 b2 = do
  g <- getStdGen
  let xs = (randomRs (a1, b1) g)
  g2 <- newStdGen
  let ys = (randomRs (a2, b2) g2)
  return (zip xs ys)