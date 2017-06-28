--UNFINISHED--
import Data.Char
import Data.List
import Data.Numbers.Primes
import Data.Maybe

digits :: (Integral a, Show a) => a -> [Int]
digits = map digitToInt . show

lengthSequences :: Int -> [a] -> [[a]]
lengthSequences n xs = concat $ catMaybes $ map (fs3 n) $ tail $ subsequences xs

fixSubsequence :: Int -> [a] -> Maybe [[a]]
fixSubsequence n xs
    | n >= length xs = Just $ [xs ++ replicate (n-(length xs)) (head xs)]
    | otherwise      = Nothing
    
fs3 n xs
    | n /= 3         = undefined
    | length xs == 1 = Just $ [replicate 3 (head xs)]
    | length xs == 2 = Just $ map (:xs) xs
    | length xs == 3 = Just $ [xs]
    | otherwise      = Nothing
                 
fastNub :: Ord a => [a] -> [a]
fastNub = map head . group . sort

nums len numVars = concat $ map fastNub $ map permutations $ map (++ (replicate numVars 'n')) $ fastNub $ map sort $ lengthSequences (len-numVars) ['0'..'9']

replace :: Eq a => a -> a -> [a] -> [a]
replace old new = map (\x -> if x==old then new else x)

replaceMult :: Eq a => a -> [a] -> [a] -> [[a]]
replaceMult old news xs = map (flip (replace old) xs) news

enumerate :: String -> [Integer]
enumerate xs@(n:_) = map read $ replaceMult 'n' ['1'..'9'] xs
enumerate xs       = map read $ replaceMult 'n' ['0'..'9'] xs

checkFamily :: String -> Bool
checkFamily n = 
    let enum = enumerate n
    in (odd . head) enum && ((== 8) $ length $ filter isPrime $ enum)

main = print $ concat $ map (filter checkFamily . nums 6) [3]

























































--hasThreeNL :: Int -> Bool
--hasThreeNL = (== 3) . last . sort . map length . group . sort . init . digits
--
--main = do
--    print $ length $ filter hasThreeNL $ dropWhile (< 1000) $ takeWhile (< 1000000) primes