numsList = [a*b*c | a<-[1..333], b<-[a..500], c<-[b..500], a+b+c == 1000, a^2+b^2 == c^2]

main = do
    print $ head numsList
  --  putStrLn $ show $ get4 $ head $ filter (\x -> get1 x == get2 x) $ filter (\x -> 1000 == get3 x) numsList
  
  