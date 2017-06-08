tris  = scanl (+) 1 [2..]
pents = scanl (+) 1 [4,7..]
hexes = scanl (+) 1 [5,9..]

findSame :: [Int] -> [Int] -> [Int] -> [Int]
findSame (x:xs) (y:ys) (z:zs)
    | (x==y) && (x==z) = x:(findSame xs ys zs)
    | (x<=y) && (x<=z) = findSame xs (y:ys) (z:zs)
    | (y<=x) && (y<=z) = findSame (x:xs) ys (z:zs)
    | (z<=x) && (z<=y) = findSame (x:xs) (y:ys) zs

main = do
    print $ last $ take 3 $ findSame tris pents hexes