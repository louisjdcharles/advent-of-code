module Day4 where

import Data.List.Split

parseLine =
    (\(a:b:_) -> (words a,words b))
    . splitOn "|"
    . drop 2
    . dropWhile ((/=) ':')

matches (winning, has) =
    length [x | x <- winning, y <- has, x == y]

calcScore 0 = 0
calcScore 1 = 1
calcScore n = 2 * calcScore (n-1)

part1 :: IO ()
part1 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ sum
             $ map calcScore
             $ map matches
             $ map parseLine
             $ lines inp

nextIteration ((n,c):xs) =
    map (\(a,b) -> (a, b + c)) (take n xs) ++ drop n xs

calcIterations xs =
    take (length xs) (iterate nextIteration xs)

part2 :: IO ()
part2 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ sum
             $ map snd
             $ map head
             $ calcIterations
             $ map (\n -> (n, 1)) -- (number of matches, count)
             $ map matches
             $ map parseLine
             $ lines inp