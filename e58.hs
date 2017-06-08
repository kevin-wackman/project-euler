import Data.Numbers.Primes

row :: Int -> [Int]
row 1 = [1]
row n = 
    let
        max = (n+1)^2
    in map ((-) max) [0,n,2*n,3*n]
    
diag = [1]:(map row [2,4..])

numPrimesInRow = map length $ map (filter isPrime) $ diag

numNumsBy :: Int -> Int
numNumsBy n = (n-1)*4 + 1

numPrimesBy :: Int -> Int
numPrimesBy n = sum $ take n numPrimesInRow

ratioList = map (\n -> ((numNumsBy n) `div` (numPrimesBy n))) [2..]

main = 
    print $ length $ takeWhile (< 10) ratioList