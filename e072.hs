isCoprime :: Integral a => a -> a -> Bool
isCoprime n = (== 1) . gcd n

numCoprimes :: Integral a => a -> Int
numCoprimes n = length $ filter (isCoprime n) [1..(n-1)]

main = do
    print $ sum $ map numCoprimes [2..10000]
    
    
subEveryN :: Int -> [Int] -> [Int]
subEveryN n (x:xs) = sub' (x:xs) (n-1)
    where
        sub' []     _ = []
        sub' (x:xs) 0 = (x-1):(sub' xs (n-1))
        sub' (x:xs) m =  x   :(sub' xs (m-1))
        
applyAll :: a -> [(a -> a)] -> a
applyAll x []     = x
applyAll x (f:fs) = applyAll (f x) fs