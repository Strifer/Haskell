import Data.Char
import Data.List

--1.1
dropper x
    | length x <4 = error "Must be at least 4 characters long"
    | otherwise = tail (reverse (drop 3 (reverse x)))

--1.2
initials x y = [head (x)] ++ ". " ++ [head(y)] ++"."

--1.3
specialConcat x y
    | length x < length y = y ++ x
    | otherwise = x ++ y

--1.4
safeHead x
    | null x = []
    | otherwise = [head x]

--1.5
hasDuplicates x = length x /= length (nub (x))


--2.1
doublesFromTo' a b
    | a<b = [x*2 | x <- [a..b]]
    | otherwise = [x*2 | x <- [b..a]]

--2.2
caesarCode' s n = [chr (ord c + n) | c <- s, c /= ' ', c >= 'a', c <= 'z']

--3.1
lengths xss = [length xs | xs <- xss]
totalLength xss = sum $ lengths xss
letterCount s = totalLength [w | w <- words s, length w > 2]

--3.2
helper s = [toUpper c | c <- s, c /= ' ']
isPalindrome x = (helper x) == (reverse $ helper x)

--3.3
flipp xss = concat [reverse xs | let xss' = reverse xss, xs <- xss']

--4.1
inCircle r x y = [(a,b) | a <- [-10..10], b <- [-10..10], (a-x)^2+(b-y^2)==r^2]
--4.2
steps xs = zip (xs) (tail (xs))

--5.1
indices x xs = [ snd pair | let xs' = zip xs [1..], pair <- xs', fst pair == x ]

--5.2
showLineNumbers s = unlines [ show (fst couple) ++ " "++ (snd couple) | let listLines = lines s, let couples = zip [1..] listLines, couple <- couples]

--5.3
common xs ys =  [ snd couplex | let couplesx = zip [1..] xs,
                                let couplesy = zip [1..] ys,
                                couplex <- couplesx,
                                coupley <- couplesy,
                                couplex == coupley]

haveAlignment xs ys = not $ null $ common xs ys