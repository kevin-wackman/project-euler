import Data.Ratio
import Data.Set (fromList, difference)

digits :: Int -> [Int]
digits 0 = []
digits x = digits (div x 10) ++ [mod x 10]

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
    
    
    --16/64
    --26/65
    --19/95
    --49/98
    