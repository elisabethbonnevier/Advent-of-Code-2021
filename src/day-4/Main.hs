module Main where

import Data.List (transpose, find)

main :: IO ()
main = do
    -- Get contents
    (numbers : boards) <- lines <$> readFile "src/day-4/input.txt"
    let numbersList = map (read :: String -> Int) $ splitAtComma numbers
    let boardsList = map (map (map (Unmarked . (read :: String -> Int)) . words))
                         $ splitAtBoards boards
    
    -- Part 1
    putStr "Part 1: "
    let (number, board) = playBingoToWin numbersList boardsList
    print $ number * sumUnmarked board

    -- Part 2
    putStr "Part 2: "
    let (numberd, board) = playBingoToLose numbersList boardsList
    print $ number * sumUnmarked board

splitAtComma :: String -> [String]
splitAtComma str =
    case break (==',') str of
        (a, ',':as) -> a : splitAtComma as
        (a, "") -> [a]

splitAtBoards :: [String] -> [[String]]
splitAtBoards strs =
    case break null strs of
        (s, "":strs) -> s : splitAtBoards strs
        (s, []) -> [s]

data BoardNumber = Unmarked Int | Marked Int

type Board = [[BoardNumber]]

markNumber :: Int -> [Board] -> [Board]
markNumber num = map (map (map $ markUnmarked num)) where

    markUnmarked :: Int -> BoardNumber -> BoardNumber
    markUnmarked m (Unmarked n) = if m == n then Marked n else Unmarked n
    markUnmarked m (Marked n) = Marked num

wins :: Board -> Bool
wins board = any markedLine board || any markedLine (transpose board) where

    markedLine :: [BoardNumber] -> Bool
    markedLine [] = True
    markedLine (Unmarked num : nums) = False
    markedLine (Marked num : nums) = markedLine nums

playBingoToWin :: [Int] -> [Board] -> (Int, Board)
playBingoToWin (number : numbers) boards =
    let updatedBoards = markNumber number boards in
    case find wins updatedBoards of
         Nothing -> playBingoToWin numbers updatedBoards
         Just board -> (number, board)

playBingoToLose :: [Int] -> [Board] -> (Int, Board)
-- playBingoToLose (number : numbers) boards
--     | length boards == 1 = 
--         let updatedBoards = markNumber number boards in
--         if any wins updatedBoards
--            then (number, head updatedBoards)
--            else playBingoToLose numbers boards
--     | otherwise = 
--         let updatedBoards = markNumber number boards in
--             playBingoToLose numbers (filter (not . wins) updatedBoards)
playBingoToLose (number : numbers) [board] =
    let [updatedBoard] = markNumber number [board] in
    if wins updatedBoard
       then (number, updatedBoard)
       else playBingoToLose numbers [board]
playBingoToLose (number : numbers) boards =
    let updatedBoards = markNumber number boards in
    playBingoToLose numbers (filter (not . wins) updatedBoards)

sumUnmarked :: Board -> Int
sumUnmarked board = sum $ map sumUnmarkedLine board where

    sumUnmarkedLine :: [BoardNumber] -> Int
    sumUnmarkedLine [] = 0
    sumUnmarkedLine (Unmarked num : nums) = num + sumUnmarkedLine nums
    sumUnmarkedLine (Marked num : nums) = sumUnmarkedLine nums
