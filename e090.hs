import Data.List
import Data.Set (Set, fromList, member,size)

fac n = product [1..n]

choose n list = concatMap permutations $ choose' list [] 
    where
        choose' []     r = if length r == n then [r] else []
        choose' (x:xs) r | length r == n = [r]
                         | otherwise     = choose' xs (x:r) 
                                        ++ choose' xs r

squares = ["01","04","09","16","25","36","49","64","81"]

dice = nub $ map fromList $ choose 6 ['0'..'9']

member' :: Char -> Set Char -> Bool
member' '6' xs = member '6' xs || member '9' xs
member' '9' xs = member '6' xs || member '9' xs
member' n xs = member n xs

combos = [((dice !! x),(dice !! y)) | x <- [0..(length dice - 1)], y <- [x..(length dice - 1)]]

checkSquare :: Set Char -> Set Char -> [Char] -> Bool
checkSquare xs ys [s1,s2] = ((member' s1 xs) && (member' s2 ys)) || ((member' s1 ys) && (member' s2 xs))

checkCombination :: (Set Char, Set Char) -> Bool
checkCombination (xs, ys) = and $ map (checkSquare xs ys) squares

    
main = do
    print $ length $ filter checkCombination combos