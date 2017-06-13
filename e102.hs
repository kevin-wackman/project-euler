import Data.List
import Data.List.Split

type Axis = Int

hasOrigin :: [(Int, Int)] -> Bool
hasOrigin [x,y,z] = (== 4) $ length $ nub $ (++ (getAxes x y)) $ (++ (getAxes x z)) $ getAxes y z

isNegative :: (Num a, Ord a) => a -> Bool
isNegative = (< 0)

isZero :: (Num a, Eq a) => a -> Bool
isZero = (== 0)

isPositive :: (Num a, Ord a) => a -> Bool
isPositive = (> 0)

div' :: Fractional a => Int -> Int -> a
div' a b = fromIntegral a / (fromIntegral b)

areOpposing :: Int -> Int -> Bool
areOpposing x y
    | isZero x || isZero y = True
    | isNegative x = isPositive y
    | otherwise    = isNegative y
    
intercept :: Fractional a => (Int, Int) -> (Int, Int) -> a
intercept (x1,y1) (x2,y2) = 
    let slope = (y2-y1) `div'` (x2-x1)
    in (fromIntegral y1) - (slope * (fromIntegral x1))
    
checkX :: (Int, Int) -> (Int, Int) -> [Axis]
checkX (x1,y1) (x2,y2)
    | not $ areOpposing x1 x2 = []
    | otherwise = if isPositive $ intercept (x1,y1) (x2,y2) then [1] else [3]

checkY :: (Int, Int) -> (Int, Int) -> [Axis]
checkY (x1,y1) (x2,y2)
    | not $ areOpposing y1 y2 = []
    | otherwise = if isPositive $ intercept (y1,x1) (y2,x2) then [2] else [4]

getAxes :: (Int, Int) -> (Int, Int) -> [Axis]
getAxes p1 p2 = (checkX p1 p2) ++ (checkY p1 p2)

lineToInts :: String -> [Int]
lineToInts = map read . splitOn ","

groupPoints :: [Int] -> [(Int, Int)]
groupPoints [] = []
groupPoints (x:y:xs) = (x,y):(groupPoints xs)


main = do
    str <- readFile "e102in.txt"
    print $ length $ filter hasOrigin $ map (groupPoints . lineToInts) $ lines str
        