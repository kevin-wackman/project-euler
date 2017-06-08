import Data.List

lotsOfInts = [1..1000000]

isBouncy :: (Integral a, Show a) => a -> Bool
isBouncy xs = 
    let xs' = show xs
        sortedxs = sort xs'
    in (xs' /= sortedxs) && ((reverse xs') /= sortedxs)
    
    
bouncePercent :: (Fractional a) => Integer -> a
bouncePercent n = 
    let totalBounces = length $ filter isBouncy [1..n]
    in ((fromIntegral totalBounces) / (fromIntegral n))
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
--Slower than other method    
--isBouncy2 :: (Integral a, Show a) => a -> Bool
--isBouncy2 = isBouncy2' . show
--    
--isBouncy2' :: String -> Bool
--isBouncy2' (x:xs) = areBothTrue $ foldl' checkIncDec (False, False, x) xs
--    where
--        areBothTrue (True, True, _) = True
--        areBothTrue _ = False
--        checkIncDec (inc, dec, x) y
--            | y > x     = (True, dec, y)
--            | x > y     = (inc, True, y)
--            | otherwise = (inc, dec,  y)