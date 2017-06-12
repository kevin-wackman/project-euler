import Data.Char

data Frac = Frac Integer Integer deriving (Show)

numerator :: Frac -> Integer
numerator (Frac n _) = n

denominator :: Frac -> Integer
denominator (Frac _ n) = n

numOverDem :: Frac -> Bool
numOverDem frac = (length $ digits $ numerator frac) > (length $ digits $ denominator frac)

digits :: Integer -> [Int]
digits = map digitToInt . show

plus :: Integer -> Frac -> Frac
plus n (Frac nmr dmr) = Frac ((n*dmr)+nmr) dmr

divy :: Integer -> Frac -> Frac
divy n (Frac nmr dmr) = Frac (dmr*n) nmr

frac1 :: Integer -> Frac
frac1 = Frac 1

divy1 :: Frac -> Frac
divy1 = divy 1

findFracSeries :: Integer -> [Integer] -> Frac
findFracSeries n xs = plus n $ findSeries' xs

findSeries' :: [Integer] -> Frac
findSeries' []     = Frac 0 1
findSeries' [x]    = frac1 x
findSeries' (x:xs) = divy1 $ plus x $ findSeries' xs

firstThousand :: [Frac]
firstThousand = map (findFracSeries 1) $ map (flip replicate $ 2) [0..999]

main = do
    print $ length $ filter numOverDem firstThousand
    
    