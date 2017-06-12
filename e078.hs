piles :: Int -> Int
piles 1 = 1
piles 2 = 2
piles n = 1 + (piles (n-1)) + (piles (n-2))

piles' :: Int -> Int -> Int
piles' _ 1 = 1
piles' n 2 = (n `div` 2) + 1