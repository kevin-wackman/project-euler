import Data.Numbers.Primes

isFactor :: Integer -> Integer -> Bool
isFactor num divisor = mod num divisor == 0

getIntSqrt :: Integer -> Integer
getIntSqrt = ceiling . sqrt . fromIntegral

reducePrime :: Integer -> Integer
reducePrime n = reduce' n [2..]
    where
        reduce' n (x:xs)
            | n <= x = x
            | n `isFactor` x = reduce' (n `div` x) [x..]
            | otherwise = reduce' n xs

main = do
    print $ reducePrime 600851475143
    --putStrLn $ show $ maximum $ filter (isFactor 600851475143) $ takeWhile (< (getIntSqrt 600851475143)) primes
