--UNFINISHED--

fac :: Integer -> Integer
fac n = product [2..n]
    
numDivisors n = length $ [x | x <- [1..n], n `rem` x == 0]