import Data.List
------------------------------------------------------------------------------------------------------------------
-- DATA TYPES
------------------------------------------------------------------------------------------------------------------

-- data Tricolor = Red | Green | Blue

-- Tricolor is a new type. RGB are data constructors.
-- Data types like this, where we explicitly enumerate all the possible
-- values are called algebraic data types.

-- This is different from aliases.

data Tricolor = Red | Green | Blue deriving Show

-- Now our tricolor has all the properties of Show.
-- Data constructors can be binary, ternary etc.

data Shape =
    Circle Double Double Double
  | Rectangle Double Double Double Double
  deriving Show
-- This showcases that data constructors are actually functions.

-- Circle :: Float -> Float -> Float -> Shape
-- We can pattern match against data constructors like this.
-- Keep in mind that all parameters must be listed.

isCircle :: Shape -> Bool
isCircle (Circle _ _ _) = True
isCircle _              = False

-- Constructors can be used anywhere where functions can be used.
-- For example..
myCircle = Circle 5 5 10
myRectangle = Rectangle 10 10 100 200
unitCircle x y = Circle x y 1

data Point = Point Double Double 
  deriving Show
data Shape2 = Circle2 Point Double | Rectangle2 Point Point 
  deriving Show

area :: Shape -> Double
area (Circle _ _ r) = r ^ 2 * pi
area (Rectangle x1 y1 x2 y2) = (abs $ x1 - x2) * (abs $ y1 - y2)
------------------------------------------------------------------------------------------------------------------
-- EXERCISE 1
------------------------------------------------------------------------------------------------------------------
-- 1.1
data Date = Date Int Int Int deriving Show
showDate :: Date -> String
showDate (Date d m y) = (show d) ++ "." ++ (show m) ++ "." ++ (show y)

-- 1.2
-- translate :: Point -> Shape2 -> Shape2
-- translate vector@(Point v1 v2) (Circle2 x2 y2 r) = Circle2 (Point (v1 + x2) (v1 + y2)) r
-- translate vector@(Point v1 v2) (Rectangle2 x1 y1 x2 y2) = Rectangle2 (x1+v1) (y1+v2) (x2+v1) (y2+v2) 
------------------------------------------------------------------------------------------------------------------
data Level    = Bachelor | Master | PhD deriving (Show,Eq)
data Student2 = Student2 String String String Level Double deriving Show

firstName2 :: Student2 -> String
firstName2 (Student2 f _ _ _ _) = f

lastName2 :: Student2 -> String
lastName2  (Student2 _ l _ _ _) = l

studentId2 :: Student2 -> String
studentId2 (Student2 _ _ i _ _) = i

-- This is somewhat tedious and long-winded. It is better to use RECORDS:

data Student = Student
  { firstName  :: String
  , lastName   :: String
  , studentId  :: String
  , level      :: Level
  , avgGrade   :: Double } deriving Show

-- Let's define a function to show some data from the record:

showStudent :: Student -> String
showStudent s = studentId s ++ " " ++ firstName s ++ " " ++ lastName s

-- or

showStudent2 :: Student -> String
showStudent2 s = intercalate " " [studentId s,firstName s,lastName s]

-- We can also define it like this:

showStudent3 :: Student -> String
showStudent3 (Student {studentId=id,firstName=f,lastName=l}) = 
  intercalate " " [id,f,l]

-- Let's write a function to select students whose average grade is above a given
-- threshold:

aboveStudents :: Double -> [Student] -> [Student]
aboveStudents x = filter ((>=x) . avgGrade)

-- When we define a record, we need not define all fields. Those that we don't
-- define will be 'undefined'. The code will compile, but we'll get a warning.

someStudent = Student { firstName = "Marko", avgGrade = 4.3 }

-- Will 'showStudent someStudent' work?
-- Will 'map firstName $ aboveStudents 4.0 [bestStudent, someStudent]' work?

-- We can partially define a student and then later give it the rest of its values.

bestStudent = Student 
  { studentId = "0036491215"
  , firstName = "John", lastName = "Doe"
  , level = Master, avgGrade = 5.0 }   


bestStudent2 = bestStudent { avgGrade = 4.9 }
someStudent2 = someStudent { lastName = "Markov", studentId = "0036365438" }

-- This is useful when we want to define default values:

bachelorStudent = Student { level = Bachelor }
masterStudent = Student { level = Master }
phdStudent = Student { level = PhD }

newStudent = bachelorStudent { 
   firstName = "Zoran", lastName = "Zoki", 
   studentId = "00364532350", avgGrade = 4.5 }

-- We can define a record in a shorter way, respecting the order of the fields:

newStudent2 = Student "Petar" "PeriÄ‡" "00364542345" Master 3.5
------------------------------------------------------------------------------------------------------------------
-- EXERCISE 2
------------------------------------------------------------------------------------------------------------------
-- 2.1
improveStudent :: Student -> Student
improveStudent s@(Student {avgGrade=grade})
    | (grade+1.0) >= 5.0 = s{avgGrade=grade+1.0}
    | otherwise = s{avgGrade=grade+1.0}
-- 2.2
avgGradePerLevels :: [Student] -> (Double,Double,Double)
avgGradePerLevels xs = (avgGradeLevel Bachelor xs, avgGradeLevel Master xs, avgGradeLevel PhD xs)
average xs = realToFrac (sum xs) / genericLength xs
avgGradeLevel lev studentList = average $ map (avgGrade) (filter ((==lev) . level) studentList)
------------------------------------------------------------------------------------------------------------------

data OldLevels  = Graduate | Doctorate deriving Show 

data GeneralStudent a = Student3 String String String a Double deriving Show

-- 'GeneralStudent' has a type parameter 'a'. Depending on what type we choose for
-- 'a', we will get different types:

type BolognaStudent = GeneralStudent Level
type FER1Student    = GeneralStudent OldLevels

-- We call such types, which take parameters as input, TYPE CONSTRUCTORS.

-- Parametrized data types are typically data containers of some sort. E.g.:

data MyBox a = InBox a

-- So, 'MyBox' is a type constructor that we can use to define different types.
-- E.g.:

type StringBox = MyBox String
type IntBox    = MyBox Int

-- 'InBox' is a data constructor that we can use to construct different values of
-- different types. Haskell will automatically determine the correct type:

b1 = InBox 1.2
b2 = InBox "Haskell"
b3 = InBox (1,3)

data Box a = Box { unbox :: a } deriving Show

-- A parametrized type can have multiple parameters:

data MyPair a b = MyPair (a,b) deriving Show

-- So we can have:
--  MyPair (1,1) :: MyPair Int Int
--  MyPair ("bla",1.2) :: MyPair String Double

-- We can, for example, define:

type IntMyPair = MyPair Int Int
type MyPairType a = MyPair a String

-- A function to take the first element from 'MyPair':

fstMyPair :: MyPair a b -> a
fstMyPair (MyPair (x,_)) = x
------------------------------------------------------------------------------------------------------------------
-- MAYBE
------------------------------------------------------------------------------------------------------------------
-- Maybe is a cool parametrized type.
-- data Maybe a = Nothing | Just a

-- For example
-- Just 5 :: Maybe Int
-- Just "kek" :: Maybe String

-- It is used in the case very a value is optional or an error can ocurr.

-- OPTIONALITY
data Employee = Employee
   { name   :: String
   , salary :: Maybe Double } deriving Show

-- We can now define:

showSalary :: Employee -> String 
showSalary e = case salary e of
    Nothing -> "unknown"
    Just n  -> show n ++ " kn"

concatMaybeStrings :: Maybe String -> Maybe String -> Maybe String
concatMaybeStrings (Just s1)  (Just s2)  = Just $ s1 ++ s2
concatMaybeStrings s@(Just _) Nothing    = s
concatMaybeStrings Nothing    s@(Just _) = s
concatMaybeStrings _          _          = Nothing

-- ERROR HANDLING
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

--Also useful is the 'Either' type:

--  data Either a b = Left a | Right b

-- It is commonly used when we also want to return an error message.
-- We return 'Right b' is there was no error, otherwise we return 'Left a', where
-- 'a' is typically a 'String' (the error message).

safeHead2 :: [b] -> Either String b
safeHead2 []    = Left "empty list"
safeHead2 (x:_) = Right x
------------------------------------------------------------------------------------------------------------------
-- EXERCISE 3
------------------------------------------------------------------------------------------------------------------
-- 3.1
data MyTriplet a b c = MyTriplet {first :: a, second :: b, third :: c} deriving Show
toTriplet :: MyTriplet a b c -> (a,b,c)
toTriplet value = (first value, second value, third value)

-- 3.2
totalSalaries :: [Employee] -> Double
totalSalaries employees = sum $ map (safeSalary . salary) employees
    where safeSalary (Just x) = x
          safeSalary _        = 0