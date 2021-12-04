module Main where

import System.IO
import Text.Read

main :: IO ()
main = do
    contents <- readFile "src/day-1/input.txt"
    let maybeInts = traverse (readMaybe :: String -> Maybe Int) $ lines contents
    case maybeInts of
        Nothing -> putStrLn "There was an error in the input file."
        Just ints -> do
            let diffs1 = zipWith (-) (drop 1 ints) ints
            putStr "Part 1: "
            putStrLn . show . length $ filter (> 0) diffs1
            let threeWindowSums = zipWith3 (\a b c -> a + b + c) (drop 2 ints) (drop 1 ints) ints
            let diffs2 = zipWith (-) (drop 1 threeWindowSums) threeWindowSums
            putStr "Part 2: "
            putStrLn . show . length $ filter (> 0) diffs2