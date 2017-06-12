import Data.Numbers.Primes

sumUntil :: (Num a, Ord a) => a -> [a] -> a
sumUntil num xs = sum' xs num 0

sum' (x:xs) num n
        | n+x>num = n
        | otherwise = sum' xs num (n+x)
        
main = print $ head $ filter isPrime $ map (sumUntil 1000000) $ map ((flip drop) primes) [0..]