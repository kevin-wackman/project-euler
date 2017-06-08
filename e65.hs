import Data.Char

data Frac = Frac Integer Integer deriving (Show)

numerator :: Frac -> Integer
numerator (Frac n _) = n

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

eseq = concat $ map (\x -> [1,x,1]) [2,4..]

main = do
    print $ sum $ digits $ numerator $ findFracSeries 2 (take 99 eseq)