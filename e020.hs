import Data.Char

digits :: (Integral a, Show a) => a -> [Int]
digits = map digitToInt . show

fac :: Integral a => a -> a
fac n = product [2..n]

main = do
    print $ sum $ digits $ fac 100