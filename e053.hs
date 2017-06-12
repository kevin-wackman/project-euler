import Math.Combinatorics.Exact.Binomial

main = do
    print $ length [(n,r) | n <- [1..100], r <- [1..n], n `choose` r > 1000000]