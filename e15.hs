import Data.MemoCombinators as Memo

advance = Memo.memo2 Memo.integral Memo.integral adv
    where
        adv _ 0 = 1
        adv 0 _ = 1
        adv m n = advance m (n-1) + advance (m-1) n

main = print $ advance 20 20