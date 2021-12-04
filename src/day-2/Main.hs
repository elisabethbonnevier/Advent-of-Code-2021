module Main where

import Data.List (foldl')
import System.IO
import Text.Read

main :: IO ()
main = do
    -- Get contents
    contents <- readFile "src/day-2/input.txt"
    let contentsList = 
            map ((\xs -> (xs !! 0 , (read :: String -> Int) (xs !! 1))) . words) -- unsafe
                $ lines contents
    -- Part 1
    putStr "Part 1: "
    let forwards = [val | (axis, val) <- contentsList, axis == "forward"]
    let ups = [val | (axis, val) <- contentsList, axis == "up"]
    let downs = [val | (axis, val) <- contentsList, axis == "down"]
    let horPos = sum forwards
    let depth = sum downs - sum ups
    putStrLn . show $ horPos * depth

    -- Part 2
    putStr "Part 2: "
    let finalPos = foldl' updatePos (0, 0, 0) contentsList
    putStrLn . show $ (\(horPos, depth, _) -> horPos * depth) finalPos

updatePos :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int) -- unsafe
updatePos (horPos, depth, aim) (dir, val)
    | dir == "forward" = (horPos + val, depth + aim * val, aim)
    | dir == "up" = (horPos, depth, aim - val)
    | dir == "down" = (horPos, depth, aim + val)
    