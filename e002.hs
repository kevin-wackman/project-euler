fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = do
    putStrLn $ show $ sum $ takeWhile (< 4000000) $ filter even fibs