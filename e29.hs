import Data.List

numUniques :: (Eq a, Ord a) => [a] -> Int
numUniques = length . group . sort

main = do
    print $ numUniques [a^b | a<-[2..100], b<-[2..100]]