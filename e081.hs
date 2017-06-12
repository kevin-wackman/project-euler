import Data.List.Split

calcFirstRow :: Num a => [a] -> [a]
calcFirstRow = scanl1 (+)

calcNextRow :: (Num a, Ord a) => [a] -> [a] -> [a]
calcNextRow (x:xs) (y:ys) = (x+y):(cnr' xs ((x+y):ys))

cnr' :: (Num a, Ord a) => [a] -> [a] -> [a]
cnr' [] _ = []
cnr' _ [] = []
cnr' (x:xs) (y':y:ys) 
    | x > y'    = (y'+y):(cnr' xs ((y'+y):ys))
    | otherwise = (x +y):(cnr' xs ((x +y):ys))
    
computeArray :: (Num a, Ord a) => [[a]] -> a
computeArray (xs:xss) = 
    let
        xs' = calcFirstRow xs
        ys  = foldl1 calcNextRow (xs':xss)
    in last ys
    
testArr = [[131,673,234,103,18],
    [201,96,342,965,150],
    [630,803,746,422,111],
    [537,699,497,121,956],
    [805,732,524,37,331]]
    
main = do
    str <- readFile "e081in.txt"
    let arr = map (map read) $ map (splitOn ",") $ lines str
    print $ computeArray arr