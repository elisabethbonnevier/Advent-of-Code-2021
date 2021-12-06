module Main where

import Data.List (find, transpose)

main :: IO ()
main = do
  -- Get contents
  (numbers : boards) <- lines <$> readFile "src/day-4/input.txt"
  let numbersList = map (read :: String -> Int) $ splitAtComma numbers

  -- Part 1
  putStr "Part 1: "
  let unmarkedBoardsList =
        map (map (map (Unmarked . (read :: String -> Int)) . words)) $
          splitAtBoards (tail boards)
  let (winningNumber, winningBoard) = playBingoToWin numbersList unmarkedBoardsList
  print $ winningNumber * sumUnmarked winningBoard

  -- Part 2
  putStr "Part 2: "
  let markedBoardsList =
        map (map (map (Marked . (read :: String -> Int)) . words)) $
          splitAtBoards (tail boards)
  let (losingNumber, losingBoard) = playBingoToLose (reverse numbersList) markedBoardsList
  print $ losingNumber * sumUnmarked losingBoard

splitAtComma :: String -> [String]
splitAtComma str =
  case break (== ',') str of
    (a, ',' : as) -> a : splitAtComma as
    (a, "") -> [a]

splitAtBoards :: [String] -> [[String]]
splitAtBoards strs =
  case break null strs of
    (s, "" : strs) -> s : splitAtBoards strs
    (s, []) -> [s]

data BoardNumber = Unmarked Int | Marked Int
  deriving (Show)

type Board = [[BoardNumber]]

markNumber :: Int -> [Board] -> [Board]
markNumber num = map (map (map $ markUnmarked num))
  where
    markUnmarked :: Int -> BoardNumber -> BoardNumber
    markUnmarked m (Unmarked n) = if m == n then Marked n else Unmarked n
    markUnmarked m (Marked n) = Marked n

unmarkNumber :: Int -> [Board] -> [Board]
unmarkNumber num = map (map (map $ unmarkMarked num))
  where
    unmarkMarked :: Int -> BoardNumber -> BoardNumber
    unmarkMarked m (Unmarked n) = Unmarked n
    unmarkMarked m (Marked n) = if m == n then Unmarked n else Marked n

wins :: Board -> Bool
wins board = any markedLine board || any markedLine (transpose board)
  where
    markedLine :: [BoardNumber] -> Bool
    markedLine [] = True
    markedLine (Unmarked num : nums) = False
    markedLine (Marked num : nums) = markedLine nums

playBingoToWin :: [Int] -> [Board] -> (Int, Board)
playBingoToWin (number : numbers) boards =
  let updatedBoards = markNumber number boards
   in case find wins updatedBoards of
        Nothing -> playBingoToWin numbers updatedBoards
        Just board -> (number, board)

playBingoToLose :: [Int] -> [Board] -> (Int, Board)
playBingoToLose (number : numbers) boards =
  let updatedBoards = unmarkNumber number boards
   in case find (not . wins) updatedBoards of
        Nothing -> playBingoToLose numbers updatedBoards
        Just board -> (number, head $ markNumber number [board])

sumUnmarked :: Board -> Int
sumUnmarked board = sum $ map sumUnmarkedLine board
  where
    sumUnmarkedLine :: [BoardNumber] -> Int
    sumUnmarkedLine [] = 0
    sumUnmarkedLine (Unmarked num : nums) = num + sumUnmarkedLine nums
    sumUnmarkedLine (Marked num : nums) = sumUnmarkedLine nums
