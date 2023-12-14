module Day7 where

import Data.List.Split
import Data.List

parse :: String -> [(String, Int)]
parse = map (\(a:b:xs) -> (a, read b))
      . map (splitOn " ")
      . lines

countOccurances :: (Eq a) => [a] -> a -> Int
countOccurances [] a = 0
countOccurances (x:xs) a =
    (if x == a then 1 else 0) + countOccurances xs a


mostFrequentCount :: (Eq a) => [a] -> Int
mostFrequentCount xs =
    maximum $ map (countOccurances xs) xs

mostCommon xs =
    fst $ maximumBy (\a b -> compare (snd a) (snd b)) (zip xs (map (countOccurances xs) xs))

cardValue :: Char -> Int
cardValue c = case c of
                'A' -> 13
                'K' -> 12
                'Q' -> 11
                'J' -> 10
                'T' -> 9
                '9' -> 8
                '8' -> 7
                '7' -> 6
                '6' -> 5
                '5' -> 4
                '4' -> 3
                '3' -> 2
                '2' -> 1
                _   -> 0

firstNonEQ [] = EQ
firstNonEQ (x:xs) =
    if x /= EQ then x else firstNonEQ xs

-- could use HOF here but whatever

compareHandValues h1 h2 =
    firstNonEQ orderings
    where
        orderings = zipWith (\x y -> compare (cardValue x) (cardValue y)) h1 h2

compareHandValues2 h1 h2 =
    firstNonEQ orderings
    where
        orderings = zipWith (\x y -> compare (cardValue2 x) (cardValue2 y)) h1 h2

orderHands :: (String, Int) -> (String, Int) -> Ordering
orderHands (h1,_) (h2,_) =
    case (compare lu1 lu2) of
        LT -> GT
        GT -> LT
        EQ -> case (compare (mostFrequentCount h1) (mostFrequentCount h2)) of
            GT -> GT
            LT -> LT
            EQ -> compareHandValues h1 h2
    where
        u1  = nub h1
        u2  = nub h2
        lu1 = length u1
        lu2 = length u2

calcScores xs =
    zipWith (\(_, a) b -> a * b) xs [1..n]
    where
        n = length xs

part1 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ sum
             $ calcScores
             $ sortBy orderHands
             $ parse inp

cardValue2 :: Char -> Int
cardValue2 c = case c of
                'A' -> 13
                'K' -> 12
                'Q' -> 11
                'J' -> 0
                'T' -> 9
                '9' -> 8
                '8' -> 7
                '7' -> 6
                '6' -> 5
                '5' -> 4
                '4' -> 3
                '3' -> 2
                '2' -> 1
                _   -> 0

countJokers = (flip countOccurances) 'J'

convertJokers "JJJJJ" = "AAAAA"

convertJokers xs =
    map (\x -> if x == 'J' then target else x) xs
    where
        target = if mostFrequent == 'J' then mostCommon (filter ((/=) 'J') xs) else mostFrequent
        mostFrequent = mostCommon xs        

orderHands2 :: (String, Int) -> (String, Int) -> Ordering
orderHands2 (h1,_) (h2,_) =
    case (compare lu1 lu2) of
        LT -> GT
        GT -> LT
        EQ -> case (compare (mostFrequentCount h1') (mostFrequentCount h2')) of
            GT -> GT
            LT -> LT
            EQ -> compareHandValues2 h1 h2
    where
        h1' = if (countJokers h1) == 0 then h1 else convertJokers h1
        h2' = if (countJokers h2) == 0 then h2 else convertJokers h2
        u1  = nub h1'
        u2  = nub h2'
        lu1 = length u1
        lu2 = length u2

part2 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ sum
             $ calcScores
             $ sortBy orderHands2
             $ parse inp
