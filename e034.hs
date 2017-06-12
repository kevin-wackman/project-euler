import Data.Char

factorial :: Int -> Int
factorial n = product [2..n]

isCurious :: Int -> Bool
isCurious n = n == (sum $ map factorial $ map digitToInt $ show n)

main = do
    print $ sum $ filter isCurious [3..99999]