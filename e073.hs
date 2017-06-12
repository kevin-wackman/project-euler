import Data.Ratio
import Data.List
import Data.List.Ordered
import Data.Numbers.Primes

isec [] _ = True
isec _ [] = True
isec (x:xs) (y:ys)
    | x == y    = False
    | x > y     = isec (x:xs) ys
    | otherwise = isec xs (y:ys)
    
cutHalfPlus :: Int -> [Int] -> [Int]
cutHalfPlus n xs 
    | odd n  = take (n `div` 2) xs
    | even n = take ((n `div` 2)-1) xs
    
cutThirdMinus :: Int -> [Int] -> [Int]
cutThirdMinus n xs = drop (n `div` 3) xs
    
sieve :: Int -> [Int] -> [Int]
sieve n xs = sieve' n xs
    where
        sieve' _ [] = []
        sieve' 1 (x:xs) = 0:(sieve' n xs)
        sieve' m (x:xs) = x:(sieve' (m-1) xs)
        
multSieve :: [Int] -> [Int] -> [Int]
multSieve [] xs     = xs
multSieve (n:ns) xs = multSieve ns $ sieve n xs

getRatios :: Int -> [Ratio Int]
getRatios n = map (%n) $ filter (/= 0) $ cutThirdMinus n $ cutHalfPlus n $ multSieve (primeFactors n) [1..n]

fracs = concat $ map getRatios [1..12000]

main = 
    print $ length fracs