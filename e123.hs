import Data.Numbers.Primes

getPrime :: Integral b => Int -> b
getPrime = ((2:primes) !!)

digits :: Integral a => a -> Int
digits n
    | n < 10    = 1
    | otherwise = 1 + (digits (n `div` 10))

getRemainder :: Integral b => Int -> b
getRemainder n = 
    let
        pn = getPrime n
        numerator = (pn-1)^n + (pn+1)^n
        denominator = pn^2
    in  numerator `mod` denominator

main = do
    print $ (+1) $ last $ takeWhile (\x -> getRemainder x <= (10^10)) [21000..]
    
    