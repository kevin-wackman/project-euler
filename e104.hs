--SLOW--

import Data.List

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isDoublePan :: (Integral a, Show a) => a -> Bool
isDoublePan n = 
    let strn    = show n
        first10 = take 9 strn
        last10  = drop (length strn - 9) strn
    in isPan first10 && isPan last10

isPan :: String -> Bool
isPan n = length n == 9 && ['1'..'9'] == sort n

main = do
    print $ fst $ head $ filter (isDoublePan . snd) $ drop 2740 $ zip [0..] fibs