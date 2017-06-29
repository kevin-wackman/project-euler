getRem :: Integral a => a -> a -> a
getRem a n = ((a-1)^n + (a+1)^n) `rem` (a^2)

getTopFrom100 :: Integral a => a -> a
getTopFrom100 a = maximum $ map (getRem a) [1..a*2]

main = do
    print $ sum $ map getTopFrom100 [3..1000]