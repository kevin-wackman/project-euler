main = do
    print $ (-) (sum $ map (^2) [1..100]) ((^2) $ sum [1..100])
    