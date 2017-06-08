import Data.Numbers.Primes
import Data.List

formulae = [((\n -> n^2 + a*n + b),a,b) | a<-[-999..999], b<-[(a+1)..1000], isPrime b]

findPrimes :: (Integer -> Integer) -> Int
findPrimes fun = length $ takeWhile isPrime $ map fun [0..]

main = do
    print $ (\(_,y,z) -> y*z) $ head $ sortBy (\(x,y,z) (x',y',z') -> compare x' x) $ map (\(x,y,z) -> (findPrimes x,y,z)) formulae