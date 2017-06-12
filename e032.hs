import Data.List
import Data.Char

digits :: Int -> [Int]
digits = map digitToInt . show

isPandigital9 :: [Int] -> Bool
isPandigital9 n = sum n == 45 && (sort n) == [1..9]

findPandigitals :: Int -> [Int] -> [Int]
findPandigitals n xs = map (!! 2) $ filter (isPandigital9 . concat . map digits) $ map (\x -> [n,x,(n*x)]) xs

main = do
    print $ sum $ nub $ concat $ map (flip findPandigitals [1234..9876]) [2..9] ++ (map (flip findPandigitals [123..987]) [12..98])