import Data.List
import Data.Char

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

getAllInts :: [[Int]] -> [Int]
getAllInts = nub . concat

findAllBefore :: Int -> [[Int]] -> [Int]
findAllBefore n nss = nub $ concat $ map (findBefore n) nss

findBefore :: Int -> [Int] -> [Int]
findBefore n ns 
    | n `notElem` ns = []
    | otherwise      = takeWhile (/= n) ns
    
main = do
    str <- readFile "e079in.txt"
    let arr = parse str
    let nums = getAllInts arr
    putStrLn $ map intToDigit $ sortOn (length . (flip findAllBefore) arr) $ nums