--import Data.List
import Data.Set (Set, member, fromList, elemAt)

pents = map (\x -> x*(3*x-1) `div` 2) [1..]

pentSet = fromList $ take 3000 pents

isPent :: Int -> Bool
isPent = flip member pentSet

nearPentPairs :: Int -> [(Int,Int)]
nearPentPairs n = [((elemAt (n-1) pentSet),x) | x <- (take (n-1) pents)]

testPentPair :: (Int,Int) -> Bool
testPentPair (x,y) = isPent (x+y) && (isPent (x-y))

main = do
    print $ (\(x,y) -> abs (x-y)) $ head $ filter testPentPair $ concat $ map nearPentPairs [1..]
    
