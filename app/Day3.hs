module Day3 where

import Data.Char

-- (Char : digit/symbol, Bool : is adj to symbol)

padInput (x:xs) = [topPadding] ++ (map (\r -> ['.'] ++ r ++ ['.']) (x:xs)) ++ [topPadding]
    where width = length x
          topPadding = take (width+2) (repeat '.')

part1 :: IO ()
part1 = do
    inp <- readFile "input.txt"
    putStrLn $ show $  padInput $ lines inp