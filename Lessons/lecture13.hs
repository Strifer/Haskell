import Data.List
import Control.Monad
import Data.Maybe
import System.Random

----------------------------------------------------------------------
--- STATE MONAD
----------------------------------------------------------------------

-- Haskell is a purely functional language and doesn't allow side effects.
-- There are no implicit states in Haskell, we have stateless computation.

-- To sidestep this problem we can use a STATE MONAD. The state monad
-- hides an explicit state and makes it accesible in the monad.

-- Let's look at an example

-- Labeling the nodes of a tree.

data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show, Eq)

t1 = Branch (Branch (Leaf 'b')) (Leaf 'c') (Branch ())

label :: Tree a -> Tree int
label = snd . step 0
	where step n (Leaf _) = (n+1, Leaf n) 
	        (n1, t1') = step n t1
	        (n2, t2') = step n1 t2
	        in (n2, Branch t1' ,t20)