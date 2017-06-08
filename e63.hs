numPowers :: Integer -> Int
numPowers n = length $ takeWhile (\x -> length (show (n^x)) == x) [1..]

main = do
    print $ sum $ map numPowers [1..10]