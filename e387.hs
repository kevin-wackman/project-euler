--UNFINISHED--

import Data.Char
import Data.Numbers.Primes
import Data.MemoCombinators as Memo

digits :: Integer -> [Int]
digits = map digitToInt . show

unDigits :: [Int] -> Integer
unDigits = read . map intToDigit

isHarshad :: Integer -> Bool
isHarshad = Memo.integral isHarshad'
    where isHarshad' n = ((fromIntegral n) `mod` (sum $ digits n)) == 0

isRTHarshad :: Integer -> Bool
isRTHarshad = Memo.integral isRTHarshad'

isRTHarshad' n
    | n <= 10 = True
    | otherwise = isHarshad n && isRTHarshad (n `div` 10)
    
isSRTHarshadP :: Integer -> Bool
isSRTHarshadP = Memo.integral isSRTHarshadP'
isSRTHarshadP' n =
   let n' = n `div` 10
   in isPrime (sum $ digits n') && isRTHarshad (n `div` 10)
    
main = print $ (+90619) $ sum $ filter isSRTHarshadP $ takeWhile (< 10^5) $ dropWhile (< 10000) primes 