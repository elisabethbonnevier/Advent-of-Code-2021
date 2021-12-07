module Main where

import Data.List (transpose)
import Data.List.Index (modifyAt)
import GHC.Utils.Misc (count)

main :: IO ()
main = do
  -- Get contents
  contents <- lines <$> readFile "src/day-5/input.txt"
  let coordinates =
        map
          ( map (map (read :: String -> Int) . splitAtComma)
              . (\w -> [head w, last w])
              . words
          )
          contents
  -- Part 1
  -- print $ maximum (map (maximum . map head) coordinates)
  -- print $ maximum (map (maximum . map last) coordinates)
  let grid = replicate 990 (replicate 990 0)
  putStr "Part 1: "
  print . sum $ map (count (> 1)) (updateGrid coordinates grid)

splitAtComma :: String -> [String]
splitAtComma str =
  case break (== ',') str of
    (a, ',' : as) -> a : splitAtComma as
    (a, "") -> [a]

updateGrid :: [[[Int]]] -> [[Int]] -> [[Int]]
updateGrid [] grid = grid
updateGrid (line : lines) grid
  | head (head line) == head (last line) =
    updateGrid
      lines
      ( transpose
          ( modifyLine
              (head (head line))
              (last (head line))
              (last (last line))
              (transpose grid)
          )
      )
  | last (head line) == last (last line) =
    updateGrid
      lines
      ( modifyLine
          (last (head line))
          (head (head line))
          (head (last line))
          grid
      )
  | otherwise = updateGrid lines grid

modifyLine :: Int -> Int -> Int -> [[Int]] -> [[Int]]
modifyLine line start stop grid
  | start == stop = grid
  | start > stop = modifyLine line start (stop + 1) (modifyAt line (modifyAt stop (+ 1)) grid)
  | start < stop = modifyLine line (start + 1) stop (modifyAt line (modifyAt start (+ 1)) grid)
