module Main where

import Data.List (transpose, foldl')
import Data.Char (digitToInt)
import GHC.Base (divInt)
import System.IO (readFile)

main :: IO ()
main = do
    -- Get contents
    contents <- lines <$> readFile "src/day-3/input.txt"

    -- Part 1
    let half = length contents `divInt` 2
    let contentsList = map (map digitToInt) contents
    let colSums = map sum $ transpose contentsList
    let gammaBin = [if x > half then 1 else 0 | x <- colSums]
    let epsilonBin = map (1 -) gammaBin
    putStr "Part 1: "
    putStrLn . show . product $ map (foldl' (\sum dig -> sum*2 + dig) 0) [gammaBin, epsilonBin]
