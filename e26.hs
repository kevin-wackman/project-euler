import Data.List
import Data.Ord

grab :: Maybe a -> a
grab (Just a) = a

unsafeFind fun xs = grab $ find fun xs

decPeriod :: Integer -> Integer
decPeriod n = (\(x,y) -> x-y) $ unsafeFind (\(x,y) -> (10^x-10^y) `mod` n == 0) [(x,y) | x <- [1..], y<-(reverse [0..(x-1)])]

fixPair :: (Integer, Integer) -> (Int, Int)
fixPair (x,y) = ((fromIntegral x), (fromIntegral y))

main = do
    print $ fst $ maximumBy (comparing snd) $ map (\x -> (x,decPeriod x)) [2..1000]