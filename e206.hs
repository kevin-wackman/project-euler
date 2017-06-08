import Math.NumberTheory.Powers.Squares
import Data.List
findNums = takeWhile (< 1929394959697989991) $ filter (>= 1020304050607080900) $ map (^2) [1010101010..]

checkDigit :: String -> (Char, Int) -> Bool
checkDigit str (chr,num) = str!!num == chr

checkNum :: Integer -> Bool
checkNum num = 
    let str = show num
    in all (checkDigit str) [('1',0),('2',2),('3',4),('4',6),('5',8),('6',10),('7',12),('8',14),('9',16),('0',18)]
    
main = do
    print $ head $ filter checkNum $ findNums