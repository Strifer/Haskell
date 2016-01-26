> import Control.Exception
> import Data.Char
> import Data.List
> import qualified Data.Map as M
> import Control.Monad
> import System.IO
> import System.Directory
> import System.IO.Error
> import System.Environment
> import System.FilePath
> import System.Random
> import System.Exit


== IO ACTIONS =============================================================================

IO operations are not pure functional programming. They almost always result in some kind
of side effect. Input functions cannot always return the same value and output functions
change the state of the system.

These operations are called IO Actions and they have the following type

    IO a

Where 'a' is the type of action's return value.

    IO String
    IO Int
    IO (Int, String)
    IO ()

The last type is for actions that do not return anything. It's called a 'unit'.

It is actually a 0-tuple.

'IO' is a type constructor of '* -> *' kind. It's a box that stores actions that aren't functionally pure.
It's important to seperate pure functional code from impure code. All impure actions
are wrapped up into the IO type so this seperation is made.

Haskell's hello world is as follows:

> main = putStrLn "Hello, world!"

This is an action, and it's type is as follows:

    main :: IO ()
    putStrLn :: String -> IO ()

'putStrLn' takes a String and returns an action (that outputs the string to the screen)

The 'main' function can invoke other functions that do some actions for example

    main = sayHello
    sayHello = putStrLn "Hello, World!"

If we need to execute many actions we have to sequence them. This is done using a special construct called 'do':

> main2 = do
>   putStrLn "here"
>   putStrLn "we"
>   putStrLn "go"

All statements are aligned one below the other. The three actions are tied into a single action that returns IO ().

They are executed sequentially like in an imperative language.

> main3 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   putStrLn $ "I guess your lucky number is " ++ number

The '<-' operator takes an action on its right side and a variable on its left side.
It unwraps the value 'a' from IO 'a' and stores it to the left.
This can only be done within a do block of a function whose type is 'IO a'.

The following will not work

  foo = "Your name is " ++ getLine

The (++) operator expects the type of String but getLine results in IO String.

  main4 = do
    putStrLn "Enter your lucky number"
    putStrLn $ "Your lucky number is " ++ getLine

This is the same case.

> askNumber :: IO String
> askNumber = do
>   putStrLn "Enter your lucky number"
>   getLine

> main5 :: IO ()
> main5 = do
>   number <- askNumber
>   putStrLn $ "Your lucky number is " ++ number

The return value of the whole block is the return value of the last performed action.
This is why the return value of askString is a string returned by getLine. $

Every action results in a value that we can store but we don't necessarily
have to consume the value.

> main6 :: IO ()
> main6 = do
>   x <- putStrLn "Enter your lucky number"
>   getLine
>   putStrLn "Thanks!"

X literally has en empty tuple inside of it.
getLine recieves the input but it doesn't do anything with it.

== EXERCISE 1===================================================================

1.1

> main' = do
>   putStrLn "Enter the first line"
>   x <- getLine
>   putStrLn "Enter the second line"
>   y <- getLine
>   putStrLn ( reverse x ++ reverse y )

1.2

> threeNumbers = do
>   putStrLn "Enter the 1st number"
>   x <- getLine
>   putStrLn "Enter the 2nd number"
>   y <- getLine
>   putStrLn "Enter the 3rd number"
>   z <- getLine
>   let sum = read x + read y + read z
>   print sum

== RETURN =====================================================================

What if we wish to modify a string taken from the input before we return it?
We can do this:

> askNumber2 :: IO String
> askNumber2 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   return $ number ++ "0"

$
The function 'return :: a -> IO a' takes some value and wraps it into an IO action.

'return' doesn't need to be the last action it can appear anywhere.
It's not really analogous to return in imperative languages.

> askNumber3 :: IO String
> askNumber3 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   return $ number ++ "0"
>   getLine

$
This doesn't really make sense but you get the point.

We can have control flow in an action:

> askNumber4 :: IO String
> askNumber4 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   if number == "" then return "7"
>       else return number

It is important that both branches return an action of the same type.
We can have recursion.

> askNumber7 :: IO String
> askNumber7 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   if number == "" then askNumber7 else return number

This function recursively calls itself until the user actually types something.
If one branch has several actions we need to make another do block:

> askNumber8 :: IO String
> askNumber8 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   if number == "" then do
>       putStr "No input!"
>       askNumber8
>   else return number

== EXERCISE 2 =================================================================
2.1

> threeStrings :: IO Int
> threeStrings = do
>   putStrLn "Enter three lines:"
>   x <- getLine
>   y <- getLine
>   z <- getLine
>   let xyz = x ++ y ++ z
>   putStrLn xyz
>   return (length xyz)

2.2

> isValidNumString :: String -> Bool
> isValidNumString = all isDigit

> askNumber9 :: IO Int
> askNumber9 = do
>   putStrLn "Enter your lucky number"
>   number <- getLine
>   if (isValidNumString number) then return (read number)
>       else askNumber9

2.3

> askUser :: String -> (String -> Bool) -> IO String
> askUser m p = do
>   putStr m
>   input <- getLine
>   if p input then return input
>       else askUser m p

> askUser' :: Read a => String -> (String -> Bool) -> IO a
> askUser' m p = do
>   putStr m
>   input <- getLine
>   if p input then return (read input)
>       else askUser' m p

2.4

> inputStrings :: IO [String]
> inputStrings = concatInputs []
>   where concatInputs xs = do
>                       s <- getLine
>                       if s == "" then return xs
>                           else concatInputs (s:xs)


== WHERE & LET =============================================================

Inside IO actions, we can use 'let' to assign values to variables:



> askName1 :: IO String
> askName1 = do
>    s1 <- getLine
>    s2 <- getLine
>    let forename = map toUpper s1
>        lastname = map toUpper s2
>    return $ forename ++ " " ++ lastname

We can also use where blocks but they must be outside of do blocks:

> askName4 :: IO String
> askName4 = do
>    s1 <- getLine
>    s2 <- getLine
>    return $ upperCase s1 ++ " " ++ upperCase s2
>    where upperCase = map toUpper

== COMMON IO FUNCTIONS =====================================================

    putStr :: String -> IO ()
    putStrLn :: String -> IO ()
    putChar :: Char -> IO ()
    print :: Show a => a -> IO ()

    print = putStrLn . show

    when :: Monad m => Bool -> m () -> m ()
    sequence :: Monad m => [m, a] -> m [a]
    mapM :: Monad m => (a -> m b) -> [a] -> m [b]
    forever :: Monad m => m a -> m b

'm' refers to any kind of Monad but when working with IO they refer to IO monads.

'when'
This function executes a given action if the condition is met.
Otherwise it executes return ().

> main8 = do
>   input <- getLine
>   when (input == "") $
>       putStrLn "Input is empty"

$

'sequence'
This function takes a list of actions and returns a single action
that runs these actions in sequence and returns a list of their return values.

> action1 = sequence [getLine,getLine]

is the same as

> action2 = do
>   x1 <- getLine
>   x2 <- getLine
>   return [x1,x2]

> main10 = do
>   putStrLn "Enter three numbers"
>   xs <- sequence [getLine, getLine, getLine]
>   putStrLn $ "Thanks. You've entered " ++ unwords xs

$

Sequence is useful for mappin an IO action over a list:

> main12 = do
>   sequence $ map print [1..10]

$

What does this do?

> main15 = do
>   sequence $ map (putStrLn . show) [1..10]
>   return ()



Because sequence $ map is used a lot there is a standard function that
does precisely this.

'mapM'

> main16 = mapM print [1..10]

If we don't want the results of the function we can use 'mapM_' instead.

> main17 = mapM_ print [1..10]

There's also a forM function which is just mapM but with flipped arguments.

== EXERCISE 3 =============================================================

3.1

> readNumbers = do
>   putStrLn "Enter how many lines you wish to enter:"
>   n <- getLine
>   putStrLn $ "Please provide " ++ n ++ " lines:"
>   xs <- sequence $ take (read n) $ repeat getLine
>   mapM_ print $ reverse xs

3.2

> sequence' [] = return []
> sequence' (x:xs) = do
>   h <- x
>   t <- sequence' xs
>   return (h:t)

> sequence_' []     = return ()
> sequence_' (x:xs) = do
>   h <- x
>   t <- sequence_' xs
>   return ()

3.3

> mapM' f = sequence' . map f

> mapM'_ f xs = do
>   mapM' f xs
>   return ()

3.4

> printTriplets = mapM_ print [(x,y,z) | x <- [1..100], y <- [x..100], z <- [y..100], x^2 + y^2 == z^2 ]

== READING FROM STREAMS ======================================================

We can read char by char from stream.

> main22 :: IO ()
> main22 = do
>   c <- getChar
>   putChar $ toUpper c
>   eof <- isEOF
>   if eof then return () else main22

We can do the same line by line:

> main23 :: IO ()
> main23 = do
>   l <- getLine
>   putStrLn $ map toUpper l
>   eof <- isEOF
>   if eof then return () else main23

It's easier to work with all of the contents at once using streams.

 getContents :: IO String

Often we read some data from stdin, apply some function to it an print it out again.
This can be done with 'interact'

 interact :: (String -> String) -> IO ()
 interact f = do s <- getContents
                  putStr (f s)

== EXERCISE 4 ================================================================
4.1

> filterOdd :: IO ()
> filterOdd = interact (removeOdd)
>       where removeOdd = unlines . map snd . filter (even . fst) . zip [0..] . lines 

4.2

> numberLines :: IO ()
> numberLines = interact (unlines . zipWith (\i s -> show i ++ " " ++ s) [1..] . lines)

4.3

filterWords :: Set String -> IO ()
filterWords set = interact (unlines . filter (set `member`) . words)

==============================================================================

There's lots of handy dandy functions that work with files in hasklel.

 hPutStr :: Handle -> String -> IO ()
 hPutStrLn :: Handle -> String -> IO ()
 hGetLine :: Handle -> IO String
 hGetChar :: Handle -> IO Char
 hGetContents :: Handle -> IO String

These functions take a handle that contains stuff about the file.
We get handles by opening a file

 openFile :: FilePath -> IOMode -> IO Handle

FilePath is the same as String
IOMode is
 data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode


After we're finished with a handle we need to close it
hClose :: Handle -> IO ()

> cat1 :: FilePath -> IO ()
> cat1 f = do
>   h <- openFile f ReadMode
>   printLines h
>   hClose h

> printLines :: Handle -> IO ()
> printLines h = do
>   l <- hGetLine h
>   putStrLn l
>   eof <- hIsEOF h
>   if eof then return () else printLines h

Using streams we can have

> cat2 :: FilePath -> IO ()
> cat2 f = do
>   h <- openFile f ReadMode
>   s <- hGetContents h
>   putStr . unlines . zipWith (\i l -> show i ++ ": " ++ l) [0..] $ lines s
>   hClose h

$
== EXERCISE 5 ================================================================
5.1

> wc :: FilePath -> IO (Int, Int, Int)
> wc f = do
>   h <- openFile f ReadMode
>   s <- hGetContents h
>   let charCount =  length s
>   let wordCount =  length $ words s
>   let lineCount =  length $ lines s
>   hClose h
>   return (charCount, wordCount, lineCount)

5.2

> copyLines :: [Int] -> FilePath -> FilePath -> IO ()
> copyLines xs f1 f2 = do
>   h1 <- openFile f1 ReadMode
>   h2 <- openFile f2 AppendMode
>   s  <- hGetContents h1
>   let chosenLines = map ((lines s) !!) xs
>   mapM (hPutStrLn h2) chosenLines
>   hClose h1
>   hClose h2
>   return ()

==============================================================================

Reading from a stream (using hGetContents) and writing to a stream (using hPutStr) is very common.
There are functions that make this easier without the need to  work with handles.

    readFile :: FilePath -> IO String
    writeFile :: FilePath -> String -> IO ()

> cat6 :: String -> IO ()
> cat6 f = do
>   s <- readFile f
>   putStr . unlines . zipWith (\i l -> show i ++ ": " ++ l) [0..]  $ lines s

> upperCaseFile :: FilePath -> IO ()
> upperCaseFile f = do
>   s <- readFile f
>   putStr $ map toUpper s

> interlaceFiles :: FilePath -> FilePath -> FilePath -> IO ()
> interlaceFiles f1 f2 f3 = do
>   s1 <- readFile f1
>   s2 <- readFile f2
>   writeFile f3 . unlines $ interlace (lines s1) (lines s2)
>   where interlace xs ys = concat $ zipWith (\x1 x2 -> [x1,x2]) xs ys

Serialization of a data structure (writing it toa disk) and deserialization (readin from disk) can be done in a straightforward manner using readFile and writeFile by combining read and show, for example:

> main30 :: IO ()
> main30 = do
>   let l = [(x,y) | x <- [0..100], y <- [x..100]]
>   writeFile "list.txt" $ show l

> main31 :: IO ()
> main31 = do
>   s <- readFile "list.txt"
>   let l = read s :: [(Int, Int)]
>   print l

When using read you must specify the type of the value that is read and we must take care
that whatever is being parsed is in the string that really parses otherwise we get an error.

The above is an example of textual serialization which is inefficient for large structures.

> type Dict = M.Map String String
> dictFile = "dict.txt"

> main11b :: IO ()
> main11b = do
>   d1 <- readDict
>   d2 <- useDict d1
>   writeFile dictFile $ show d2

> readDict :: IO Dict
> readDict = do
>   e <- doesFileExist dictFile
>   if e then do
>       s <- readFile dictFile
>       return $ read s
>   else return M.empty

> useDict :: Dict -> IO Dict
> useDict d = do
>   putStrLn "Enter term:"
>   w1 <- getLine
>   if null w1 then return d else
>       case M.lookup w1 d of
>           Just w2 -> do
>               putStrLn w2
>               useDict d
>           Nothing -> do
>               putStrLn $ "No entry. How would you translate " ++ w1 ++ "?"
>               w2 <- getLine
>               useDict $ M.insert w1 w2 d

== EXERCISE 6 ================================================================

6.1

> wordTypes :: FilePath -> IO Int
> wordTypes f = do
>   s <- readFile f
>   return $ length $ nub $ words s

6.2

> diff :: FilePath -> FilePath -> IO ()
> diff f1 f2 = do
>   s1 <- readFile f1
>   s2 <- readFile f2
>   mapM_ (\(x,y) -> do { putStrLn $ "> "++x; putStrLn $ "< "++y}) $ filter (\(x,y) -> x /= y) $ zip (lines s1) (lines s2)

6.3

> removeSpaces :: FilePath -> IO ()
> removeSpaces f = do
>   (ft, ht) <- openTempFile "" f
>   s <- readFile f
>   hPutStr ht . unlines . map trim $ lines s
>   hClose ht
>   renameFile ft f

> trim :: String -> String
> trim = f . f
>   where f = reverse . dropWhile isSpace

== EXCEPTION HANDLING =========================================================

IO actions can cause exceptions when something fucks up. They can only be handled in
an IO monad. They are handled using control functions

 try :: Exception e => IO a -> IO (Either e a)
 catch :: Exception e => IO a -> (e -> IO a) -> IO a


try takes an action and returns 'IO (Right a)' if everything is ok otherwise it returns
'Left e' if an exception occurred

> cat7 :: String -> IO ()
> cat7 f = do
>   r <- try $ cat2 f
>   case r of
>       Left e  -> putStrLn $ "Error: " ++ ioeGetErrorString e
>       _       -> return ()

'catch' takes two actions: one that can cause an exception and one that handles it

> cat8 :: String -> IO ()
> cat8 f = catch (cat8 f) $ \e ->
>   if isDoesNotExistError e then putStrLn "Error: someone stole the file"
>   else ioError e

== ENVIRONMENT VARIABLES =========================================================

Arguments can be possed toa  program through CLI. 

    getProgName :: IO String
    getArgs :: IO [String]

> main33 :: IO ()
> main33 = do
>   xs <- getArgs
>   x <- getProgName
>   putStrLn $ "Program" ++ x ++ " is invoked with arguments " ++ show xs

> main34 :: IO ()
> main34 = do
>   xs <- getArgs
>   h <- case xs of
>     (f:_) -> do e <- doesFileExist f
>                 if e then openFile f ReadMode else return stdin
>     []    -> return stdin
>   s <- hGetContents h
>   putStr . unlines . sort $ lines s

== EXERCISE 7 =================================================================

7.1

> fileHead :: IO ()
> fileHead = do
>   xs <- getArgs
>   let n = case xs of
>               (('-':n):_) -> read n
>               _           -> 10
>   h <- case xs of
>           []          -> return stdin
>           [('-':n)]   -> return stdin
>           (_:f:_)     -> do e <- doesFileExist f
>                             if e then openFile f ReadMode else exitFailure
>           (f:_)       -> do e <- doesFileExist f
>                             if e then openFile f ReadMode else exitFailure
>   s <- hGetContents h
>   putStr $ unlines $ take n $ lines s

7.2

> sortFiles :: IO ()
> sortFiles = do
>   xs <- getArgs
>   forM_ xs $ \x ->
>       do e <- doesFileExist x
>          if e then sortFile x else putStrLn $ "File " ++ x ++ " doesn't exist"

> sortFile :: FilePath -> IO ()
> sortFile f = do
>   s <- readFile f
>   putStr $ unlines $ sort $ lines s

== RANDOM NUMBER GENERATOR ====================================================

The module 'System.Random' provides functions for generating random numbers.

    random :: (RandomGen g, Random a) => g -> (a, g)

'Random' is a type class for types whose values ccan be randomly generated.
'RandomGen' is a type class for various implementations of random generators.

    mkStdGen :: Int -> StdGen

Gives a standard random integer generator, it takes a seed and an RNG initialized with the seed.

> g = mkStdGen 13
> (r1,g2) = random g :: (Int, StdGen)
> (r2,g3) = random g2 :: (Int, StdGen)
> (r3,g4) = random g3 :: (Int, StdGen)

  randoms :: (RandomGen g, Random a) => g -> [a]

> xs = randoms g :: [Float]
> fiveCoins = take 5 $ randoms g :: [Bool]

To generate random numbers from within an interval, use

  randomR :: (RandomGen g, Random a) => (a, a) -> g -> (a, g)
  randomRs :: (RandomGen g, Random a) => (a, a) -> g -> [a]

> fiveDice = take 5 $ randomRs (1,6) g :: [Int]

The obvious problem is that, because Haskell is pure, we need to drag around
the last instance of the RNG. Another problem is that we must specify the seed.
Both problems disappear if we use an RNG in the IO monad. To create an RNG in
the IO monad, use

    getStdGen :: IO StdGen

> main35 :: IO ()
> main35 = do
>   g <- getStdGen
>   putStrLn $ take 10 (randomRs ('a', 'z') g)
>   g2 <- getStdGen
>   putStrLn $ take 10 (randomRs ('a', 'z') g2)

The problem here is that we don't get different values every time we run the
RNG, despite calling 'getStdGen' twice. We need to "split" the RNG using
'newStdGen':

> main36 :: IO ()
> main36 = do
>   g <- getStdGen
>   putStrLn $ take 10 (randomRs ('a','z') g)
>   g2 <- newStdGen
>   putStrLn $ take 10 (randomRs ('a','z') g2)

== EXERCISE 8 =================================================================
8.1

> randoms' :: (RandomGen g, Random a) => g -> [a]
> randoms' g = x:(randoms' newGen)
>   where (x, newGen) = random g

8.2

> randomPositions :: Int -> Int -> Int -> Int -> IO [(Int,Int)]
> randomPositions x1 y1 x2 y2 = do
>   g <- getStdGen
>   let xs = (randomRs (x1, y1) g)
>   g2 <- newStdGen
>   let ys = (randomRs (x2, y2) g2)
>   return $ zip xs ys