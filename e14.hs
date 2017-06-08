import Data.Ord
import Data.List
import Data.MemoCombinators as Memo

collatz :: Int -> [Int]
collatz = Memo.integral collatz'
    where
    collatz' 1 = [1]
    collatz' n
        | even n = n:(collatz $ div n 2)
        | odd  n = n:(collatz $ 3*n+1)
    
main = do
    print $ head $ maximumBy (comparing length) $ map collatz $ filter odd [1..999999]
