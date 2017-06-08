import Data.Number.CReal
import Data.Char

cut2off :: [Char] -> [Char]
cut2off (x:y:ys) 
    | ys == "0" = []
    | length ys == 101 = init $ init $ x:ys
    | length ys == 100 = init $ x:ys
    | otherwise = x:ys


getDecimalSums :: CReal -> Int
getDecimalSums num =
    let strarr = cut2off $ showCReal 101 (sqrt num)
    in sum $ map digitToInt strarr
    

main = 
    print $ sum $ map getDecimalSums [1..99]