module Ratio (Ratio, (%), simplify) where

data Ratio = Ratio Integer Integer

(%) :: (Integral a, Integral b) => a -> b -> Ratio
(%) _ 0 = error "Division by zero"
(%) a b = (Ratio (fromIntegral a) (fromIntegral b))
(five, three, tenth) = (5 % 1, 15 % 5, 1 % 10)

simplify :: Ratio -> Ratio
simplify r@(Ratio a b) = Ratio (a `div` divisor) (b `div` divisor)
    where divisor = gcd a b

toStr :: Ratio -> String
toStr (Ratio a b) = (show a)++" % "++(show b)

instance Eq Ratio where
    r1@(Ratio a1 b1) == r2@(Ratio a2 b2) = (a1*b2)==(a2*b1) 

instance Ord Ratio where
    r1@(Ratio a1 b1) <= r2@(Ratio a2 b2) = (a1*b2) <= (a2*b1)
    r1 > r2  = not (r1<=r2)
    r1 >= r2 = (r1 > r2) || (r1 == r2)
    r1 < r2 = not (r1 >= r2)

instance Num Ratio where
    r1@(Ratio a1 b1) + r2@(Ratio a2 b2) = simplify $ Ratio (a1*b2+b1*a2) (b1*b2)
    r1@(Ratio a1 b1) - r2@(Ratio a2 b2) = simplify $ Ratio (a1*b2-b1*a2) (b1*b2)
    r1@(Ratio a1 b1) * r2@(Ratio a2 b2) = simplify $ Ratio (a1*a2) (b1*b2)
    negate r1@(Ratio a1 b1)             = Ratio (-a1) (b1)
    abs r1@(Ratio a1 b1)                = Ratio (abs a1) (abs b1)
    signum r1@(Ratio a1 b1) 
        | a1 == 0 = 0
        | (a1 > 0 && b1 < 0) && (a1 < 0 && b1 > 0 ) = (-1)
        | otherwise = (1)
    fromInteger a1 = Ratio a1 1

instance Show Ratio where
    show = toStr . simplify