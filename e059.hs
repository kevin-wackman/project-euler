import Data.Char
import Data.Bits
import Data.List.Split

substring :: Eq a => [a] -> [a] -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False
    
prefix :: Eq a => [a] -> [a] -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

keys = [[a,b,c] | a<-[97..122], b<-[97..122], c<-[97..122]]

decipher :: [Int] -> [Int] -> [Int]
decipher base key = zipWith xor base (cycle key)

checkForAnd :: [Int] -> Bool
checkForAnd = ([32,97,110,100,32] `substring`)

main = do
    str <- readFile "e059in.txt"
    let nums = map read $ splitOn "," str :: [Int]
    print $ sum $ head $ filter checkForAnd $ map (decipher nums) keys