import Data.Numbers.Primes
import Data.List

isPrimePair :: (Show a, Integral a) => (a,a) -> Bool
isPrimePair (a,b) =
    let
        a' = show a
        b' = show b
        ab = read $ a' ++ b'
        ba = read $ b' ++ a'
    in isPrime ab && isPrime ba
    
isPrimeSet :: (Show a, Integral a) => [a] -> Bool
isPrimeSet xs = all isPrimePair [(a,b) | a <- xs, b <- xs, a < b]

relevantPrimes = takeWhile (< 1000) primes

possiblePrimeSets = [[a,b] | a <- relevantPrimes, b <- (dropWhile (<= a) relevantPrimes)]

main = do
    print $ filter isPrimeSet possiblePrimeSets