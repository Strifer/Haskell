-- EXERCISE 1 ------------------------------------------------------------------------
--1.1
concat3 a b c
    | length b < 2 = a++c
    | otherwise = a++b++c

--1.2
showSalary amount bonus
    | amount < 0 || bonus < 0 = error "Arguments cannot be negative."
    | otherwise = if bonus /= 0
                  then "Salary is "++show amount++", and a bonus "++show bonus++"."
                  else "Salary is "++show amount++"."