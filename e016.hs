import Data.Char

digits :: (Integral a, Show a) => a -> [Int]
digits = map digitToInt . show

main = do
    print $ sum $ digits $ 2^1000