calcNextRow :: (Num a, Ord a) => [a] -> [a] -> [a]
calcNextRow (x:xs) (y:ys)
    | length xs < length ys = (x+y):(calcNextRow (x:xs) ys) 
    | length xs < 1         = [x+y]
    | otherwise             = (max (x+y) (head xs + y)):(calcNextRow (xs) ys) 
    
main = do
    str <- readFile "e018in.txt"
    let nums = map (map read) $ map words $ lines str :: [[Int]]
    print $ maximum $ foldl1 calcNextRow nums
   