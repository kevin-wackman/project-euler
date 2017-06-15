import Data.List
import Data.Numbers.Primes

factors :: Integral a => a -> [a]
factors = init . nub . map product . subsequences . primeFactors

checkAmicable :: Int -> Bool
checkAmicable n = ((sum $ factors n) < 1000) && (n == (sum $ factors $ sum $ factors n))

isAmicable :: Int -> Bool
isAmicable n = let
    sumFac = sum $ factors n
    in (sumFac /= n) && (n == (sum $ factors sumFac))

factorSum :: Int -> Int
factorSum = sum . factors
    
main = do
    print $ sum $ filter isAmicable [2..9999]