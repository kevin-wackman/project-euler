import Data.Numbers.Primes
import Data.List
import Data.Char

digits :: Int -> [Int]
digits = map digitToInt . show

isPandigital9 :: Int -> Bool
isPandigital9 n = sum (digits n) == 28 && (sort $ digits n) == [1..7]

nineDigitPrimes :: [Int]
nineDigitPrimes = takeWhile (<10000000) $ dropWhile (<1000000) primes

main = do
    print $ head $ filter isPandigital9 $ reverse nineDigitPrimes