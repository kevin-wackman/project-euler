--UNFINISHED--

import Data.MemoCombinators as Memo

piles :: Int -> Int
piles = Memo.integral p'
    where
        p' 1 = 1
        p' 2 = 2
        p' n = 1 + (piles (n-1)) + (piles (n-2))

piles' :: Int -> Int -> Int
piles' _ 1 = 1
piles' n 2 = (n `div` 2) + 1