import Data.Numbers.Primes
import Data.List

numFactors :: Integral a => a -> Int
numFactors = length . nub . primeFactors

searchFourX :: Eq b => (a -> b) -> b -> Int -> [a] -> a
searchFourX fun occ 1 (x:xs) = if ((fun x) == occ) then x else (searchFourX fun occ 4 xs)
searchFourX fun occ n (x:xs) = if ((fun x) == occ) then (searchFourX fun occ (n-1) xs) else (searchFourX fun occ 4 xs)
    
main = do
    print $ (\x -> x-3) $ searchFourX numFactors 4 4 [3..]