weekCycle = cycle [2,3,4,5,6,7,1]
nonLeapYear = [31,28,31,30,31,30,31,31,30,31,30,31]
leapYear    = [31,29,31,30,31,30,31,31,30,31,30,31]

nly = concat $ map (\x -> [1..x]) nonLeapYear
ly  = concat $ map (\x -> [1..x]) leapYear

years = concat $ nly:(take 100 (cycle (nly:nly:nly:[ly])))

days = drop 365 $ zipWith (*) years weekCycle

main = do
    print $ length $ filter (==1) days