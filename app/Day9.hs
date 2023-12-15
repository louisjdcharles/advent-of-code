module Day9 where

import Data.List.Split

parse :: String -> [[Int]]
parse = map parseLine
      . lines
      where
        parseLine = map read . splitOn " "


differences :: [Int] -> [[Int]]
differences xs =
    takeWhile (not . all (0 ==)) $ iterate (\a -> zipWith (-) (drop 1 a) a) xs

calcNext :: [[Int]] -> Int
calcNext = sum . map last

calcPrev :: [[Int]] -> Int
calcPrev (x:xs) =
    head x - calcDifference (map head xs)
    where
        calcDifference [x] = x
        calcDifference (x:xs) = x - calcDifference xs

part1 :: IO ()
part1 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ sum
             $ map calcNext
             $ map differences
             $ parse inp

part2 :: IO ()
part2 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ sum
             $ map calcPrev
             $ map differences
             $ parse inp