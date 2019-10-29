import Data.Numbers.Primes
import qualified Data.Set as Set

primesBelowNToTheXth :: (Integral a) => a -> Int -> [a]
primesBelowNToTheXth n x = takeWhile (< n) $ map (^x) primes

fiftyMillion = 50000000

primeSquares = primesBelowNToTheXth fiftyMillion 2
primeCubes  = primesBelowNToTheXth fiftyMillion 3
primeQuads  = primesBelowNToTheXth fiftyMillion 4

sums = Set.fromList [(a+b+c) | a <- primeSquares, b <- primeCubes, c <- primeQuads]

main = print $ length $ Set.filter (< 50000000) sums
