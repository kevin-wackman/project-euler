import Data.Set (fromList, toList, difference)
factors n = [x | x <- [1..n `div` 2], n `mod` x == 0]

isAbundant :: Int -> Bool
isAbundant n = n < (sum $ factors n)

main = do
    let abundants = filter isAbundant [1..28122]
    let sumAbundants = [x+y | x<-abundants, y<-abundants, (x+y) <= 28123]
    let abundantSet = fromList sumAbundants
    let numsSet = fromList [1..28123]
    print $ sum $ toList $ difference numsSet abundantSet