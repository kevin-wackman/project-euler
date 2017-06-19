import Data.Numbers.Primes
import Data.List
    
numCoprimes :: (Integral a) => a -> a
numCoprimes n = round $ (*(fromIntegral n)) $ product $ map (\x -> (1-(1/(fromIntegral x))))$ nub $ primeFactors n
        
main = do
    print $ last $ sortOn (\x -> (fromIntegral x) / (fromIntegral (numCoprimes x))) [1..1000000]