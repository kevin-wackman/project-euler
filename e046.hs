import Data.Numbers.Primes
import Data.List.Ordered
xs = [x+(2*(y^2)) | x<-(take 5000 primes), y<-[1..1000]]

compOdds = filter (not . isPrime) [3,5..]

customList = filter (not . isPrime) $ filter odd $ nubSort xs

findFirstMismatch :: [Int] -> [Int] -> Int
findFirstMismatch (x:xs) (y:ys)
    | ys == []  = undefined
    | x == y    = findFirstMismatch xs ys
    | otherwise = x
    
main = do
    print $ findFirstMismatch compOdds customList