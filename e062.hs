import Data.List

cubes = map (^3) [1..10000]

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy (\x y -> f x == f y)

main = do
    print $ head $ head $ sortOn head $ map sort $ filter (\x -> length x == 5) $ groupOn sort $ sortOn sort $ map show cubes