--UNFINISHED--
import Data.Char
import Data.List
import Data.Numbers.Primes

digits :: (Integral a, Show a) => a -> [Int]
digits = map digitToInt . show

hasThreeNL :: Int -> Bool
hasThreeNL = (== 3) . last . sort . map length . group . sort . init . digits

main = do
    print $ length $ filter hasThreeNL $ dropWhile (< 1000) $ takeWhile (< 1000000) primes