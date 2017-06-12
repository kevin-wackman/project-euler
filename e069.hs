import Data.Numbers.Primes
import Data.List


isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

sieve :: [Integer]
sieve = 2:(sieveFilter 2 [3..])
    where sieveFilter p (nextP:list) = nextP:(sieveFilter nextP (filter (\x -> x `mod` p /= 0) list))

noOverlap :: (Eq a) => [a] -> [a] -> Bool
noOverlap xs ys = isEmpty $ intersect xs ys

factorList = [primeFactors a | a <- [1..1000000]]

findCoprimeRatio :: (Fractional a) => Int -> a
findCoprimeRatio n = (fromIntegral n)/(fromIntegral (length $ filter (noOverlap (factorList !! (n-1))) (take n factorList)))

numFactors :: (Integral a) => a -> Int
numFactors = length . nub . primeFactors
        
main = do
    print $ head $ filter ((== 7) . numFactors) [1..1000000]
