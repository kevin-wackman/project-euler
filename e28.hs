(x:xs) = take 501 $ scanl (+) 1 [5,13..]

main = do
    print $ (+) x $ sum $ map (*4) xs