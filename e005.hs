import Data.Numbers.Primes
import Data.List

expPair :: Integral a => (a,a) -> a
expPair (m,n) = m^n

factors = map (\xs -> (head xs, length xs)) $ concat $ map (group . sort . primeFactors) [2..20]

filterNumbers :: [(Int, Int)] -> Int -> Int
filterNumbers xs n = expPair $ maximumBy (\(x1,y1) (x2,y2) -> y1 `compare` y2) $ filter (\(v,c) -> v == n) xs

main = do
    print $ product $ map (filterNumbers factors) $ takeWhile (< 20) primes
    
