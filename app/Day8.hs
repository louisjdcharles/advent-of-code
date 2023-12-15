module Day8 where

import Data.List.Split
import Data.Char

import qualified Data.Map as Map

parseNode node =
    (k, (l, r))
    where
        node' = filter isAlpha node
        k = take 3 node'
        l = (take 3 . drop 3) node'
        r = drop 6 node'

parse inp =
    (cycle instructions, nodes)
    where inp' = (filter (/= "") . lines) inp
          instructions = head inp'
          nodes = Map.fromList $ map parseNode (tail inp')

calculate :: String -> (String, Map.Map String (String, String)) -> Int
calculate k (i:is, nodes) =
    if k == "ZZZ" then 0 else 1 + calculate k' (is, nodes)
    where
        v = nodes Map.! k
        k' = case i of
            'L' -> fst v
            'R' -> snd v

part1 :: IO ()
part1 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ calculate "AAA"
             $ parse inp

part2 :: IO ()
part2 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ foldl lcm 1
             $ part2'
             $ parse inp

part2' (is, nodes) =
    map (flip calculate2 (is, nodes)) ks
    where
        ks = filter ((==) 'A' . last)(Map.keys nodes)

calculate2 k (i:is, nodes) =
    if last k == 'Z' then 0 else 1 + calculate2 k' (is, nodes)
    where
        v = nodes Map.! k
        k' = case i of
            'L' -> fst v
            'R' -> snd v
