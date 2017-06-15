--SLOW SOLUTION--
import Data.Numbers.Primes
import Data.List

isGen :: Integral a => a -> Bool
isGen n = all isPrime $ map (\x -> x + (n `div` x)) $ factors n

factors :: Integral a => a -> [a]
factors = nub . map product . subsequences . primeFactors

main = print $ sum $ filter isGen [1..100000000]