import Data.List

digits :: Int -> [Int]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

checkSame :: Eq a => [a] -> Bool
checkSame [] = True
checkSame [x] = True
checkSame (x:y:xs) = (x == y) && (checkSame (y:xs))

checkNum :: Int -> Bool
checkNum n = 
    let digitLists = map sort $ map digits $ map (*n) [1,2,3,4,5,6]
    in checkSame digitLists
    
main = do
    print $ head $ filter checkNum [1..]