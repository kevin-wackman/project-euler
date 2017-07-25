import Data.List

addLists :: Num a => [a] -> [a] -> [a]
addLists = zipWith (+)

multList :: Num a => a -> [a] -> [a]
multList n = map (*n)

replaceValue :: Int -> a -> [a] -> [a]
replaceValue 0 val (x:xs) = val:xs
replaceValue n val (x:xs) = x:(replaceValue (n-1) val xs)

baseVector = (1.0):(replicate 39 0)

diceOdds = map ((/16) . fromIntegral . length) $ (group . sort) $ [x+y | x <- [1..4], y <- [1..4]]

adjustSpecials :: [Double] -> [Double]
adjustSpecials xs = 
    let vals  = map (xs !!) specialPoss
        xs'   = foldr (\x -> replaceValue x 0) xs specialPoss
        specs = zipWith multList vals specials
    in foldr addLists xs' specs

getRollVector :: Int -> [Double] -> [Double]
getRollVector pos xs = 
    let val = xs !! pos
        replaces = zip (map (`mod` 40) [pos+2..pos+8]) diceOdds
    in multList val $ foldr (\(pos,val) -> replaceValue pos val) (replicate 40 0) replaces

takeTurn :: [Double] -> [Double]
takeTurn xs =
    let rollVectors = map (\x -> getRollVector x xs) [0..39]
    in adjustSpecials $ foldr1 addLists rollVectors
    
applyUntilStable :: ([Double] -> [Double]) -> [Double] -> [Double]
applyUntilStable f xs = 
    let xs'  = f xs
    in if xs ==# xs' then xs' else (applyUntilStable f xs')
    
(==#) :: [Double] -> [Double] -> Bool
(==#) xs ys = all (< 0.00001) $ zipWith (-) xs ys
    
    
cc1 = [1/16,0,14/16]++(replicate 7 0) ++ (1/16:(replicate 29 0))
cc2 = (1/16:(replicate 9 0)) ++ (1/16:(replicate 6 0)) ++ (14/16:(replicate 22 0))
cc3 = (1/16:(replicate 9 0)) ++ (1/16:(replicate 22 0)) ++ (14/16:(replicate 6 0))
ch1 = [1/16,0,0,0,1/16,1/16,0,6/16,0,0,1/16,1/16,1/16,0,0,2/16]++(replicate 8 0)++(1/16:(replicate 14 0))++[1/16]
ch2 = [1/16,0,0,0,0,1/16,0,0,0,0,1/16,1/16,0,0,0,0,0,0,0,1/16,0,0,6/16,0,1/16,2/16,0,0,1/16,0,0,0,0,0,0,0,0,0,0,1/16]
ch3 = [1/16,0,0,0,0,3/16,0,0,0,0,1/16,1/16,1/16,0,0,0,0,0,0,0,0,0,0,0,1/16,0,0,0,0,0,0,0,0,1/16,0,0,6/16,0,0,1/16]
g2j = (replicate 10 0) ++ (1.0:(replicate 29 0))
specials = [cc1,cc2,cc3,ch1,ch2,ch3,g2j]
specialPoss = [2,17,33,7,22,36,30]

main = print $ take 3 $ reverse $ map fst $ sortOn snd $ zip [0..39] $ applyUntilStable takeTurn baseVector