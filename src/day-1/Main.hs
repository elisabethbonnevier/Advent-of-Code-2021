module Main where

import System.IO
import System.IO.Utils

import Text.Read

main :: IO ()
main = do
    withFile "src/day-1/input.txt" ReadMode (\handle -> do
        contents <- hGetLines handle
        let maybeInts = traverse (readMaybe :: String -> Maybe Int) contents
        case maybeInts of
            Nothing -> putStrLn "There was an error in the input file."
            Just ints -> do
                let diffs = zipWith (-) (drop 1 ints) ints
                putStrLn . show . length $ filter (> 0) diffs)