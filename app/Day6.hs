module Day6 where

import Data.Char
import Data.List.Split

parse :: String -> [(Int, Int)]
parse = uncurry zip
      . (\(a:b:xs) -> (a, b))
      . map (map read)
      . map (filter (/= ""))
      . map (splitOn " ")
      . map (dropWhile (\c -> not $ isDigit c))
      . lines 

possibilities (t, d) =
    [t' | t' <- [1..(t-1)], t' * (t - t') > d]

part1 :: IO ()
part1 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ product
             $ map length
             $ map possibilities
             $ parse inp

parse2 :: String -> (Int, Int)
parse2 = (\(a:b:xs) -> (a,b))
       . map (read . filter isDigit)
       . lines

part2 :: IO ()
part2 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ length
             $ possibilities
             $ parse2 inp