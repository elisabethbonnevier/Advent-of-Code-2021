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
    print . product $ map (foldl' (\sum dig -> sum*2 + dig) 0) [gammaBin, epsilonBin]

    -- Part 2
    putStr "Part 2: "
    print . product $ map (foldl' (\sum dig -> sum*2 + dig) 0) 
                          [concat (filterByCommon (>) contentsList), -- oxygen generator rating
                           concat (filterByCommon (<=) contentsList)] -- CO2 scrubber rating

filterByCommon :: (Int -> Int -> Bool) -> [[Int]] -> [[Int]]
filterByCommon f xs = filterByCommonAux 0 f xs where
    filterByCommonAux :: Int -> (Int -> Int -> Bool) -> [[Int]] -> [[Int]]
    filterByCommonAux n f [x] = [x]
    filterByCommonAux n f xs =
        let (zeros, ones) = foldr (\binNum (zeros, ones)
                                    -> if binNum !! n == 0
                                          then (binNum : zeros, ones)
                                          else (zeros, binNum : ones))
                                    ([], []) xs in
        filterByCommonAux (n+1) f (if length zeros `f` length ones  then zeros else ones)
