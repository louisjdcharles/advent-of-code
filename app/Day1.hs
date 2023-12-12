module Day1 where

import Data.Char
import Control.Monad

part1 :: IO ()
part1 = do
    inp <- readFile "input.txt"
    putStrLn $ show $ sum $ map read $ filteredLines inp
    where
        filteredLines s  = map (\l -> [head l, last l]) $  map (filter isDigit) (lines s)

part2 :: IO ()
part2 = do
    inp <- readFile "input.txt"
    putStrLn $ show $ sum $ map maybeToNumber $ map (\l -> (liftM2 (++)) (head l) (last l)) $ map (filter ((/=) Nothing) . parse) $ lines inp
    where
        parse [] = []
        parse str = (if isDigit (head str) then
                            [Just [head str]]
                        else
                            [stringToDigit str])
                        ++ (parse (tail str))

        stringToDigit str | (take 3 str) == "one" = Just "1"
                          | (take 3 str) == "two" = Just "2"
                          | (take 5 str) == "three" = Just "3"
                          | (take 4 str) == "four" = Just "4"
                          | (take 4 str) == "five" = Just "5"
                          | (take 3 str) == "six" = Just "6"
                          | (take 5 str) == "seven" = Just "7"
                          | (take 5 str) == "eight" = Just "8"
                          | (take 4 str) == "nine" = Just "9"
                          | otherwise = Nothing
        
        maybeToNumber m = case m of 
                            (Just s) -> read s
                            Nothing -> 0
