calcNextRow :: (Num a, Ord a) => [a] -> [a] -> [a]
calcNextRow (x:xs) (y:ys)
    | length xs < length ys = (x+y):(calcNextRow (x:xs) ys) 
    | length xs < 1         = [x+y]
    | otherwise             = (max (x+y) (head xs + y)):(calcNextRow (xs) ys) 
    
main = do
    str <- readFile "e18in.txt"
    let nums = map (map read) $ map words $ lines str :: [[Int]]
    print $ maximum $ foldl1 calcNextRow nums
    
    
--getVal :: Num a => [[a]] -> Int -> Int -> a
--getVal ns row col = (ns !! row) !! col
--
--getMax :: (Num a, Ord a) => [[a]] -> Int -> Int -> a
--getMax xs 14 col  = getVal xs 14 col
--getMax xs row col = (getVal xs row col) + (max (getMax xs (row+1) col) (getMax xs (row+1) (col+1)))
--
--main = do
--    str <- readFile "e18in.txt"
--    let nums = map (map read) $ map words $ lines str :: [[Int]]
--    print $ getMax nums 0 0
    
   