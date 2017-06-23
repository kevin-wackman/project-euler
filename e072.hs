import Data.Numbers.Primes
import Data.List

numCoprimes :: (Integral a) => a -> a
numCoprimes n = round $ (*(fromIntegral n)) $ product $ map (\x -> (1-(1/(fromIntegral x))))$ nub $ primeFactors n

main = do
    print $ sum $ map numCoprimes [2..1000000]
    