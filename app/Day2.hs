module Day2 where

import Data.List.Split

data Colour = Red   Int
            | Green Int
            | Blue  Int
            deriving(Show)

listToColour :: [String] -> Maybe Colour
listToColour (n:s:[]) = case s of
                            "red" -> Just (Red (read n))
                            "green" -> Just (Green (read n))
                            "blue" -> Just (Blue (read n))
                            _ -> Nothing

checkColour :: (Maybe Colour) -> Bool
checkColour c = case c of
    (Just (Red n)) -> n <= 12
    (Just (Green n)) -> n <= 13
    (Just (Blue n)) -> n <= 14


isPossible xs = foldl (\a b -> a && checkColour b) True xs

possibleGameIDs [] idx = []
possibleGameIDs (x:xs) idx = 
    (case (isPossible x) of
        True -> [idx]
        False -> []) ++ possibleGameIDs xs (idx+1)

parseInput s = map parseLine (lines s)

parseLine = concat
            . map (map (listToColour . splitOn " "))
            . map (splitOn ", ")
            . splitOn "; " . drop 2 . dropWhile ((/=) ':')

part1 :: IO ()
part1 = do
    inp <- readFile "input.txt"
    putStrLn $ show $ sum $ possibleGameIDs (parseInput inp) 1


gameMaximums [] t = t
gameMaximums (x:xs) (r, g, b) = 
        gameMaximums xs (case x of
                            (Just (Red new_r)) -> (max r new_r, g, b)
                            (Just (Green new_g)) -> (r, max g new_g, b)
                            (Just (Blue new_b)) -> (r, g, max b new_b)
                        )

maximums xs = map (\x -> gameMaximums x (0, 0, 0)) xs

gamePower (r, g, b) = r * g * b

part2 :: IO ()
part2 = do
    inp <- readFile "input.txt"
    putStrLn $ show $ sum $ map gamePower $ maximums $ parseInput inp