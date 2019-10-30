import Data.List
import Data.Numbers.Primes
import Data.Maybe
import qualified Data.Set as Set
import Data.MemoCombinators as Memo

factors :: Integral a => a -> [a]
factors = init . nub . map product . subsequences . primeFactors

factorSum :: Int -> Int
factorSum = sum . factors

fsMemo = Memo.integral factorSum

getAmicableChainInRange :: Int -> Int -> Maybe (Set.Set Int)
getAmicableChainInRange max n = chain' max n n Set.empty

chain' :: Int -> Int -> Int -> Set.Set Int -> Maybe (Set.Set Int)
chain' max start n set = 
    case (Set.member n set) of
        True -> if n == start then Just set else Nothing
        False -> if (n>max) then Nothing else chain' max start (fsMemo n) (Set.insert n set)

main = print $ Set.findMin $ head $ reverse $ sortOn length $ catMaybes $ map (getAmicableChainInRange 1000000) [0..1000000]
