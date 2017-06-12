import Data.Numbers.Primes
import Data.List

digits :: Int -> [Int]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

relevantList = takeWhile (< 4000) $ dropWhile (< 1000) primes

totalList = [n | n<-relevantList, isPrime (n+3330), isPrime (n+6660)]

checkDigits :: Int -> Bool
checkDigits n = (sort $ show n) == (sort $ show $ n+3330) && (sort $ show n) == (sort $ show $ n+6660) 

main = do
    putStrLn $ concat $ map show $ (\x -> map (+x) [0,3330,6660]) $ last $ filter checkDigits totalList