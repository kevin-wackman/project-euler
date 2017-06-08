factors n = [x | x <- [1..n `div` 2], n `mod` x == 0]

checkAmicable :: Int -> Bool
checkAmicable n = ((sum $ factors n) < 1000) && (n == (sum $ factors $ sum $ factors n))

isAmicable :: Int -> Bool
isAmicable n = let
    sumFac = sum $ factors n
    in (sumFac /= n) && (n == (sum $ factors sumFac))

factorSum :: Int -> Int
factorSum = sum . factors
    
main = do
    print $ sum $ filter isAmicable [1..9999]