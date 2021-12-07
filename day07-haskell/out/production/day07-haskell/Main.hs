module Main where

import Data.List
import Data.List.Split

median :: Ord a => [a] -> a
median input = sort input !! (length input `div` 2)

part1 :: [Int] -> Int
part1 crabs = sum $ map (\x -> abs $ x - pos) crabs
  where
    pos = median crabs

loss :: Int -> Int
loss n = n * (n + 1) `div` 2

part2 :: [Int] -> Int
part2 crabs = minimum [sum $ [loss $ abs (c - x) | c <- crabs] | x <- [minimum crabs .. maximum crabs]]

-- Main
main :: IO ()
main = do
  file <- readFile "input.txt"
  let crabs = map read $ splitOn "," file :: [Int]
   in do
        print $ part1 crabs
        print $ part2 crabs
