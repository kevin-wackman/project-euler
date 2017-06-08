import Data.Ratio
import Data.List
import Data.Numbers.Primes


numFactors :: (Integral a) => a -> Int
numFactors n = (product $ map ((+1) . length) (group (primeFactors n)))

resilience :: Integer -> Ratio Integer
resilience n = let
    ns = [(x%n) | x<-[1..(n-1)]]
    numResilient = fromIntegral $ length $ filter ((== n) . denominator) ns
    in numResilient%(n-1)
    
myMin :: (Ord b) => (a -> b) -> [a] -> a
myMin fun (x:xs) = myMin' fun xs (x,(fun x))

myMin' :: (Ord b) => (a -> b) -> [a] -> (a,b) -> a
myMin' _ [] (a,_) = a
myMin' fun (x:xs) (a,b)
    | (fun x) < b = myMin' fun xs (x,(fun x))
    | otherwise   = myMin' fun xs (a,b)
    
multPrimes :: [Integer]
multPrimes = scanl1 (*) primes



isFactor :: Integer -> Integer -> Bool
isFactor n m = mod m n == 0


    
main = do
    print $ head $ filter ((< (15499%94744)) . resilience) $ take 20 $ filter even $ filter (isFactor 3) $ filter (isFactor 5) $ filter (isFactor 7) $ filter (isFactor 11) $ filter (isFactor 13) $ [100000000..6469693230]


    
    

