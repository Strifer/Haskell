import Data.List
import Data.Char
import Control.Monad
import Data.Maybe

----------------------------------------------------------------------
--- INTRO
----------------------------------------------------------------------
-- Monads are an important aspect in Haskell.
-- The term itself comes from category theory.
-- They abstract computational models.

-- computation with state
-- We are changing or building something, instead of dragging around a state
-- we can switch monads.

-- non-deterministic computation
-- we have a function that can give multiple results
-- we have another function that uses the first function's result
-- so it all branches out and we end up with a list of possible solutions
-- monads can be used for dealing with this kind of computation more simply

-- computation that can fail
-- we have three functions and each of those can fail
-- 1 -> 2 -> 3
-- they are executed in a chain of successful computation


-- IO monad represents a state monad. The state is the world.
-- Whenever we write or read we are changing the state(the world)
----------------------------------------------------------------------
--- MOTIVATING EXAMPLE
----------------------------------------------------------------------

data Sex = Male | Female deriving (Show,Read,Eq,Ord)
data Person = Person {
  forename :: String,
  surname  :: String,
  sex      :: Sex,
  mother   :: Maybe Person,
  father   :: Maybe Person,
  partner  :: Maybe Person,
  children :: [Person] } deriving (Show,Read,Eq,Ord)

pero  = Person "Pero" "Peric" Male    (Just ana) Nothing Nothing    []
ana   = Person "Ana"  "Anic"  Female  (Just tea) Nothing Nothing    [pero]
tea   = Person "Tea"  "Teic"  Female  Nothing    Nothing (Just ivo) [ana]
ivo   = Person "Ivo"  "Ivic"  Male    Nothing    Nothing (Just tea) [ana]


-- Pero has Ana as a parent
-- Ana has Tea as a parent

-- Let's look at two functions:

grandmothersPartner :: Person -> Maybe Person
grandmothersPartner p = case mother p of
  Just m -> case mother m of
    Just g  -> partner g
    Nothing -> Nothing
  Nothing -> Nothing

-- The mother of p can succeed or not.
-- If it succeeds we ask who the mother of that mother is.
-- If it succeeds we ask who the partner of the grandmother is.
-- There are three instances in which this can fail connected in a chain.
-- Who is the mother ? IDK : mother
-- Who is the mother of mother ? IDK : grandmother
-- Who is the partner of the grandmother ? IDK : partner

-- This is ugly and gets very complicated with a more complex data structure.

partnersForename :: Person -> Maybe String
partnersForename p = case partner p of
  Just p  -> Just $ forename p
  Nothing -> Nothing

-- Who is the partner ? IDK : partner
-- What is the partners forename? forename (partner)

-- Both function compute a value that can be Nothing or Just x.
-- If its nothing we just return nothing, otherwise we send the x into another function
-- that returns either Just f(x) or Nothing
-- In these examples we deal with a chain of computations each link can fail.
-- If something fails we return Nothing otherwise just return 'Just x'

-- f(x) -- Just y -- > g(y) -- Just z -- > h(z)
--  |                   |                   |
-- fails              fails               fails
-- nothing            nothing            nothing

-- Lets implement two helper functions.

inject :: a -> Maybe a
inject x = Just x        --- (or: inject = Just)

-- Inject wraps up a value into a 'Just' type.

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing  _ = Nothing
bind (Just x) k = k x

-- Bind links some value wrapped in 'Maybe' with a function that takes an unwrapped value
-- and returns a new 'Maybe' type
-- it unwraps the input value returns Nothing if its Nothing
-- otherwise it just gives the value to the new function for further computation

grandmothersPartner2 :: Person -> Maybe Person
grandmothersPartner2 p = (mother p `bind` mother) `bind` partner

partnersForename2 :: Person -> Maybe String
partnersForename2 p = partner p `bind` (\r -> inject (forename r))

partnersForename2' :: Person -> Maybe String
partnersForename2' p = partner p `bind` (inject . forename)

-- These two functions, inject and bind define a MONAD.

-- class Monad m where
--    (>>=)   :: m a -> (a -> m b) -> m b
--    return  :: a -> m a

-- Monad is a type class of all polymorphic types that provide definitions for
-- these two functions.


grandmothersPartner3 :: Person -> Maybe Person
grandmothersPartner3 p = (mother p >>= mother) >>= partner

partnersForename3 :: Person -> Maybe String
partnersForename3 p = partner p >>= return . forename

-- (>>=) operator is left associative so we can drop the paranthesis
-- This operation depends on the previous value.

grandmothersPartner4 :: Person -> Maybe Person
grandmothersPartner4 p = mother p >>= mother >>= partner

-- instance Monad Maybe where
--  (Just x) >>= k  = k x
--  Nothing  >>= _  = Nothing

--  return          = Just

-- (>>) this is called the sequence operator.
-- it does not depend solely
----------------------------------------------------------------------
--- MONADIC LAWS
----------------------------------------------------------------------
-- 1) return a >>= k          == k a
-- 2) m >>= return            == m
-- 3) m >>= (\x -> k x >>= h) == (m >>= k) >>= h
----------------------------------------------------------------------
--- EXERCISE 1
----------------------------------------------------------------------
grandfathersPartnerForename :: Person -> Maybe String
grandfathersPartnerForename p = (father p >>= father) >>= partner >>= return . forename

--stripSuffix :: Eq a >= [a] -> [a] -> Maybe a
stripSuffix suffix s = stripPrefix (reverse suffix) (reverse s) >>= return . reverse

----------------------------------------------------------------------
--- EXERCISE 1
----------------------------------------------------------------------
-- Let's look at the IO monad.

-- IO is an isntance of a monad.

-- putStrLn :: String -> IO ()
-- getLine :: IO String

hello1 :: IO ()
hello1 = putStrLn "Hello" >>= \_ -> putStrLn "Hello!"

hello2 :: IO ()
hello2 = putStrLn "Hello" >> putStrLn "Hello!"

-- A do block is syntactic sugar for bindings and sequencing.

main1 :: IO ()
main1 = do
  putStrLn "Introduce tu numero de la suerte"
  number <- getLine
  putStrLn $ "Lo creas o no, tu numero de la suerte es " ++ number

main2 :: IO ()
main2 =
  putStrLn "Give a number" >>
  getLine >>=
    (\number -> putStrLn $ "Your number is " ++ number)

-- we could have written:

askName2 :: IO String
askName2 =
  putStrLn "Inserisci il tuo nome" >>
  getLine >>= (\s1 ->
    putStrLn "Inserisci il tuo cognome" >>
    getLine >>= (\s2 ->
      return $ s1 ++ " " ++ s2))

-- Sugarization rules look like this

-- do e                =>    e
-- do e1; e2; ...; en  =>    e1 >> do e2; ...; en
-- do x <- e1; e2; ...; en  =>  e1 >>= \x -> do e2; ...; en
-- do pat <- e1; e2; ...; en => let ok pat = do e2; ...; en
--                                  ok _   = fail "..."
--                              in e1 >>= ok
-- Instead of
-- grandmothersPartner4 :: Person -> Maybe Person
-- grandmothersPartner2 p = mother p >>= mother >>= partner

grandmothersPartner5 :: Person -> Maybe Person
grandmothersPartner5 p = do
  m <- mother p
  g <- mother m
  partner g

----------------------------------------------------------------------
--- EXERCISE 2
----------------------------------------------------------------------
-- 2.1
grandfathersPartnerForename' :: Person -> Maybe String
grandfathersPartnerForename' p = do
  f <- father p
  g <- father f
  pa <- partner g
  return $ forename pa

-- 2.2