singles = [1..20]++[25]
doubles = map (*2) singles
triples = map (*3) [1..20]

alls = singles ++ doubles ++ triples

oneshots = doubles
twoshots = [a+b | a <- doubles , b <- alls]
threeshots = [a+(alls!!b)+(alls!!c) | a <- doubles, b <- [0..((length alls)-1)], c <- [b..((length alls)-1)]]

main = do
    print $ length $ filter (< 100) $ (++oneshots) $ (++twoshots) threeshots