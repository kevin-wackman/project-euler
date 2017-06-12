--UNFINISHED--

fac :: Integer -> Integer
fac a 
    | a <= 1    = 1
    | otherwise = a * (fac (a-1))
    
numDivisors n = length $ [x | x <- [1..n], n `rem` x == 0]