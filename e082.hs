import Data.List.Split
import Data.List

calcNextRow :: (Num a, Ord a) => [a] -> [a] -> [a]
calcNextRow xs ys = map (\start -> getMinDist start xs ys) $ take (length ys) [0..]

computeArray :: (Num a, Ord a) => [[a]] -> a
computeArray = minimum . foldl1' calcNextRow . transpose

getMinDist :: (Num a, Ord a) => Int -> [a] -> [a] -> a
getMinDist start xs ys = minimum $ map (\end -> getDist start end xs ys) $ take (length xs) [0..]

getDist :: Num a => Int -> Int -> [a] -> [a] -> a
getDist start end xs ys = 
    let
        xnum = xs !! end
        (s',e') = if start > end then (end, start) else (start, end)
    in xnum + sumBetween s' e' ys

sumBetween :: Num a => Int -> Int -> [a] -> a
sumBetween start end = sum . drop start . take (end+1)

testArr = [[131,673,234,103,18],
    [201,96,342,965,150],
    [630,803,746,422,111],
    [537,699,497,121,956],
    [805,732,524,37,331]]

main = do
    str <- readFile "e081in.txt"
    let arr = map (map read) $ map (splitOn ",") $ lines str
    print $ computeArray arr