module Day5 where

import Data.List.Split
import Control.Applicative
import Control.Monad

parseInput (seeds:ranges) =
    (seeds', ranges')
    where
        seeds' :: [Maybe Int]
        seeds' = map Just
               $ map read
               $ splitOn " "
               $ drop 2
               $ dropWhile ((/=) ':') seeds
        
        ranges' :: [[(Int -> Maybe Int)]]
        ranges' = map (map ((\(a:b:c:[]) -> (\n -> if b <= n && n < b + c then Just (a + n - b) else Nothing)) . (map read . (splitOn " "))) . tail)
                $ splitOn [""]
                $ drop 1 ranges

calculate (seeds,[]) = seeds
calculate (seeds,(r:rs)) = 
    calculate (seeds', rs)
    where
        seeds' = map (\(Just s) -> foldl (\a b -> b s <|> a ) (Just s) r) seeds


part1 :: IO ()
part1 = do
    inp <- readFile "input.txt"
    putStrLn $ show
             $ foldl (\a b -> min <$> a <*> b <|> a <|> b) (Nothing)
             $ calculate
             $ parseInput
             $ lines inp