import Data.Numbers.Primes
import Data.List

isCoprime :: Integral a => a -> a -> Bool
isCoprime n = (== 1) . gcd n

numCoprimes :: (Integral a) => a -> a
numCoprimes n = round $ (*(fromIntegral n)) $ product $ map (\x -> (1-(1/(fromIntegral x))))$ nub $ primeFactors n

main = do
    print $ sum $ map numCoprimes [2..1000000]
    