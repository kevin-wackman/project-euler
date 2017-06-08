import Data.List

pete  = [sum [a,b,c,d,e,f,g,h,i] | a<-[1..4], b<-[1..4], c<-[1..4], d<-[1..4], e<-[1..4], f<-[1..4], g<-[1..4], h<-[1..4], i<-[1..4]]
colin = [sum [a,b,c,d,e,f]       | a<-[1..6], b<-[1..6], c<-[1..6], d<-[1..6], e<-[1..6], f<-[1..6]]

countList :: Ord a => [a] -> [(a, Int)]
countList = map (\xs -> (head xs, length xs)) . group . sort

total = length pete * (length colin)
goods = length $ [ 1 | pet<-pete, col<-colin, pet > col]

findWins :: Ord a => [(a,Int)] -> (a,Int) -> Int
findWins xs (val, cnt) = (* cnt) $ foldl (\n (v,c) -> n+c) 0 $ takeWhile (\(v,c) -> v < val) xs

main = do
    print $ (/ (fromIntegral total)) $ fromIntegral $ sum $ map (findWins (countList colin)) $ countList pete
