module Main where

import Data.List.Split

step :: Int -> Int
step 0 = 6
step x = x - 1

stepAll :: [Int] -> [Int]
stepAll xs = map step xs <> replicate numNew 8
  where
    numNew = length $ filter (== 0) xs

naive :: Int -> [Int] -> Int
naive d lanterns = length $ foldl (\c _ -> stepAll c) lanterns [1..d]

-- TODO array of contribution at each day (mod 7) + array of future 

main :: IO ()
main = do
  input <- readFile "test.txt"
  let lanterns = map read $ splitOn "," $input :: [Int]
   in print $ naive 80 lanterns
