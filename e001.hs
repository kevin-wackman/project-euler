div35 :: Int -> Bool
div35 n = (mod n 3) == 0 || (mod n 5) == 0


main = do
    putStrLn $ show $ sum $ filter div35 [1..999]
