x=2
inc x = x+1
digits2Number x y = x * 10 + y
d = inc 6
fiftytwo = digits2Number 5 2
condDec x = if x > 0 then x -1 else x
foo x = (if even x then x*2 else 2) + 1
merge s1 s2 =
   s1 ++ (if s1 < s2 then " is not " else " is ") ++ s2

merge3 s1 s2
   | s1 < s2   = s1 ++ "is" ++ s2
   | otherwise = s1 ++ " is not " ++ s2

grade core
    | core < 50 = 1
    | core < 60 = 2
    | core < 70 = 3
    | core < 80 = 4
    | otherwise = 5

concat3 s1 s2 s3
   | length s2 < 2 = s1++s3
   | otherwise = s1++s2++s3

concatS s1 s2 s3 = s1++ (if length s2 < 2 then "" else s2) ++ s3

showSalary amount bonus = "Salary is "++show amount++ (if bonus > 0 then ", bonus is "++show bonus else "")

main1 = putStrln "Hello, World!"