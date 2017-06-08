import Data.Numbers.Primes
import Data.List

tris = scanl (+) 1 [2..]

numFactors :: (Integral a) => a -> Int
numFactors n = product $ map ((+1) . length) (group (primeFactors n))

main = do
    print $ head $ filter ((> 500) . numFactors) tris