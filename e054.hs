import Data.Maybe
import Data.List
import Data.Ord
import Data.Char

data Suit = Clubs | Spades | Hearts | Diamonds deriving (Eq, Show)

type Hand = [Card]

data Card = Card Int Suit deriving (Eq, Show)
instance Ord Card where
    (Card n1 _) `compare` (Card n2 _) = n1 `compare` n2
    
main = do
    str <- readFile "e54in.txt"
    let hands = lines str
    print $ length $ filter (== GT) $ map (\(x,y) -> compareHands x y) $ map readGame hands
    
readGame :: String -> (Hand, Hand)
readGame str =
    let
        cards = map readCard $ words str
        hand1 = take 5 cards
        hand2 = drop 5 cards
    in (hand1, hand2)
    
readCard :: String -> Card
readCard [x,y] = Card (readValue x) (readSuit y)
    where 
        readValue 'T' = 10
        readValue 'J' = 11
        readValue 'Q' = 12
        readValue 'K' = 13
        readValue 'A' = 14
        readValue  n  = digitToInt n
        readSuit 'H' = Hearts
        readSuit 'S' = Spades
        readSuit 'D' = Diamonds
        readSuit  _  = Clubs
    
getSuit :: Card -> Suit
getSuit (Card _ s) = s

getValue :: Card -> Int
getValue (Card v _) = v

compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2 =
    let r1 = rateHand h1
        r2 = rateHand h2
    in if r1 == r2 then compareHighCards h1 h2 else r1 `compare` r2

data Rating = HighCard | Pair Int | TwoPair Int Int | ThreeOfAKind Int | Straight Int | Flush | FullHouse Int Int | FourOfAKind Int | StraightFlush Int deriving (Eq, Ord, Show)

untilJust :: [(a -> Maybe b)] -> a -> Maybe b
untilJust [] _ = Nothing
untilJust (f:fs) a = 
    let ret = f a
    in if isJust ret then ret else untilJust fs a
    
handRankingFuns :: [(Hand -> Maybe Rating)]
handRankingFuns = [isStraightFlush, isFourOfAKind, isFullHouse, isFlush, isStraight, isThreeOfAKind, isTwoPair, isPair]
    
isStraightFlush :: Hand -> Maybe Rating
isStraightFlush hand = 
    let straight = isStraight' hand
        isjflush = isJust $ isFlush hand
    in if isjflush && isJust straight then Just $ StraightFlush (fromJust straight) else Nothing

isFourOfAKind :: Hand -> Maybe Rating
isFourOfAKind hand = 
    let values = sort $ map getValue hand
        values' = sortBy (comparing length) $ group values
        is4k = (== 4) $ last $ map length values'
    in if is4k then Just $ FourOfAKind $ head $ last values' else Nothing

isFullHouse :: Hand -> Maybe Rating
isFullHouse hand = 
    let values = sort $ map getValue hand
        values' = sortBy (comparing length) $ group values
        isfh = (length values' == 2) && ((== [2,3]) $ map length values')
    in if isfh then Just $ (FullHouse (head $ last values') (head $ head values')) else Nothing

isFlush :: Hand -> Maybe Rating
isFlush hand = 
    let suits = map getSuit hand
        isflush = (==1) $ length $ group suits
    in if isflush then Just Flush else Nothing

isStraight :: Hand -> Maybe Rating
isStraight hand =
    let straight = isStraight' hand
    in if isJust straight then (Just $ Straight (fromJust straight)) else Nothing

isStraight' :: Hand -> Maybe Int
isStraight' hand =
    let values = sort $ map getValue hand
        has5 = (== 5) $ length $ group values
        isStraight = has5 && values `isInfixOf` [2..14]
        isShortStraight = has5 && values == [2..5] ++ [14]
    in if isShortStraight then (Just 5) else (if isStraight then (Just $ last values) else Nothing)

isThreeOfAKind :: Hand -> Maybe Rating
isThreeOfAKind hand = 
    let values = sort $ map getValue hand
        values' = sortBy (comparing length) $ group values
        is3k = (== 3) $ last $ map length values'
    in if is3k then Just $ ThreeOfAKind $ head $ last values' else Nothing

isTwoPair :: Hand -> Maybe Rating
isTwoPair hand =
    let values = sort $ map getValue hand
        values' = tail $ sortBy (comparing length) $ group values
        is2p = (length values' == 2) && ((== [2,2]) $ map length values')
    in if is2p then Just $ (TwoPair (head $ last values') (head $ head values')) else Nothing

isPair :: Hand -> Maybe Rating
isPair hand =
    let values = sort $ map getValue hand
        values' = sortBy (comparing length) $ group values
        is2k = (== 2) $ last $ map length values'
    in if is2k then Just $ Pair $ head $ last values' else Nothing
    
compareHighCards :: Hand -> Hand -> Ordering
compareHighCards h1 h2 = 
    let v1 = reverse $ sort $ map getValue h1
        v2 = reverse $ sort $ map getValue h2
        ordArray = zipWith compare v1 v2
    in if v1 == v2 then EQ else head $ takeWhile (/= EQ) ordArray
        
rateHand :: Hand -> Rating
rateHand hand = 
    let rating = untilJust handRankingFuns hand
    in if isJust rating then fromJust rating else HighCard