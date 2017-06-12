import Data.List

check3Slice :: Int -> Int -> String -> Bool
check3Slice i n str = (read (take 3 $ drop (i-1) str)) `mod` n == 0

checkSlices :: String -> Bool
checkSlices str =  all ($ str) $ zipWith check3Slice [2..8] [2,3,5,7,11,13,17]

main = do
    print $ sum $ map read $ filter checkSlices $ permutations "0123456789"