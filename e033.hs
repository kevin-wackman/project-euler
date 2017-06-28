import Data.Ratio
import Data.Char (digitToInt)
import Data.Set (fromList, difference)

digits = map digitToInt . show

filteredArray = filter ((> 0) . (`mod` 10)) [11..99]

pairs = [(x,y) | x<-filteredArray, y<-filteredArray] :: [(Int, Int)]

checkPair :: (Int, Int) -> Bool
checkPair (x,y) = 
    let xd = digits x
        yd = digits y
        d' = (head xd) % (last yd)
        d  = x%y
    in (x /= y) && (last xd) == (head yd) && d == d'
    
main = do
    print $ denominator $ product $ map (\(x,y)->x%y) $ filter checkPair pairs
