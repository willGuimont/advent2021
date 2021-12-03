module Main where

import Data.List

parseBit :: Char -> Int
parseBit '0' = 0
parseBit '1' = 1
parseBit _ = undefined

countBits :: [Int] -> (Int, Int)
countBits xs = go xs 0 0
  where
    go [] ones zeroes = (ones, zeroes)
    go (1:ys) ones zeroes = go ys (ones + 1) zeroes
    go (0:ys) ones zeroes = go ys ones (zeroes + 1)

countToBit :: (Int, Int) -> Int
countToBit (ones, zeroes) = if ones > zeroes then 1 else 0

notBits :: [Int] -> [Int]
notBits [] = []
notBits (0:xs) = 1 : notBits xs
notBits (1:xs) = 0 : notBits xs

bitsToInt :: [Int] -> Int
bitsToInt = sum . map (2^) . elemIndices 1 . reverse

main :: IO ()
main = do
  input <- readFile "test.txt"
  let
    ls = lines input
    bits = map (map parseBit) ls
    transposed = transpose bits
    countedBits =  map (countToBit . countBits) transposed
    gamma = bitsToInt countedBits
    epsilon = bitsToInt $ notBits countedBits
    in do
      print $ gamma * epsilon
