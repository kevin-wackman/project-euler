import Data.Numbers.Primes
import Data.List
import Data.Ratio

isCoprime :: Integral a => a -> a -> Bool
isCoprime n = (== 1) . gcd n

numCoprimes :: (Integral a) => a -> a
numCoprimes n = round $ (*(fromIntegral n)) $ product $ map (\x -> (1-(1/(fromIntegral x))))$ nub $ primeFactors n

resilience :: Integer -> Ratio Integer
resilience n = numCoprimes n % (n-1)

main = do
    print $ head $ filter (< (15499%94744)) $ map resilience $ uglyFun [100000000..1000000000]
    
isFactor :: Integer -> Integer -> Bool
isFactor n m = mod m n == 0

uglyFun = filter (isFactor 23) . filter (isFactor 19) . filter (isFactor 17) . filter (isFactor 13) . filter (isFactor 11) . filter (isFactor 7) . filter (isFactor 5) . filter (isFactor 3) . filter even