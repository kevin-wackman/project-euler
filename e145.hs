import Data.Char

digits :: Int -> [Int]
digits = map digitToInt . show

fromDigits :: [Int] -> Int
fromDigits = foldl1 $ (+).(*10)

reverseInt :: Int -> Int
reverseInt = read . reverse . show

isReversible :: Int -> Bool
isReversible n = (all odd $ digits $ n+(reverseInt n)) && (n `mod` 10 > 0)

main = do
    print $ length $ filter isReversible [1..(10^7)]