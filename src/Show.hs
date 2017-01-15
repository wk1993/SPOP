module Show (
    showSpreadsheet,
    showAvailableCommands,
    charToString,
    showFullCell
) where

import Data.List
import Data.Maybe
import Data.Char
import Model
import SpreadsheetOps

charToString :: Char -> String
charToString c = [c]

showSpreadsheet :: Spreadsheet -> IO ()
showSpreadsheet None = putStrLn ("Your spreadsheet is not set, create or read it from file!")
showSpreadsheet spreadsheet = do
    showSpreadsheetNameOfColumns ['A' .. 'J']
    showSpreadsheetLoop spreadsheet 1 50 ['A' .. 'J']

showSpreadsheetLoop :: Spreadsheet -> Int -> Int -> [Char] -> IO ()
showSpreadsheetLoop spreadsheet actualRowId lastRowId nameOfColumns = do
    showSpreadsheetRow spreadsheet actualRowId (getCellsRect spreadsheet 'A' actualRowId 100 0) nameOfColumns
    if actualRowId == lastRowId then
        return ()
    else
        showSpreadsheetLoop spreadsheet (actualRowId+1) lastRowId nameOfColumns

showSpreadsheetRow :: Spreadsheet -> Int -> [Cell] -> [Char] -> IO ()
showSpreadsheetRow s rowId cells nameOfColumns = do
    putStr $ (show rowId ++ "\t\t")
    mapM_ (\column -> if elem column [col cell | cell <- cells] 
                      then putStr $ (showTrimmedCell s (val (cells!!(last((findPos cells column))))) ++ "\t\t")
                      else putStr $ "\t\t") nameOfColumns
    putStr "\n"

showTrimmedCell :: Spreadsheet -> CellVal -> String
showTrimmedCell s cv = case cv of
                     StringVal a -> trimCellValLength a
                     other       -> case (calculateValue s (Just other)) of
                                        Left err -> "Error: " ++ err
                                        Right val -> trimCellValLength (show val)

showFullCell :: Spreadsheet -> Char -> Int -> String
showFullCell s c r = case (val (last (getCellsRect s c r 0 0))) of
                     StringVal a -> a
                     other       -> case (calculateValue s (Just other)) of
                                        Left err -> "Error: " ++ err
                                        Right val -> show val

trimCellValLength :: String -> String
trimCellValLength inputStr = if length inputStr < 15 then inputStr else (take 11 inputStr) ++ "..."

showSpreadsheetNameOfColumns :: [Char] -> IO ()
showSpreadsheetNameOfColumns nameOfColumns = do
    putStr $ ("\t\t")
    mapM_ (\nameOfColumn -> (putStr $ (charToString (nameOfColumn) ++ "\t\t"))) (sort nameOfColumns)
    putStr "\n"

findPos :: [Cell] -> Char -> [Int]
findPos cells column = [index | (index, c) <- zip [0..] cells, column == col c]

-- prints available commands
showAvailableCommands :: IO ()
showAvailableCommands = do 
    putStrLn ("\nAvailable commands:")
    putStrLn ("open filename      - open spreadsheet from file")
    putStrLn ("save filename      - save spreadsheet to file")
    putStrLn ("remove column id   - remove column with specified id, id should be a single capital letter")
    putStrLn ("remove row id      - remove row with specified id, id should be a number")
    putStrLn ("add column id      - add a new column to the spreadsheet at position id, id should be a single capital letter")
    putStrLn ("add row id         - add a new row to the spreadsheet at position id, id should be a number")
    putStrLn ("modify cell        - modify value for specified (in next steps) cell")
    putStrLn ("create             - create new spreadsheet")
    putStrLn ("show               - show content of actual spreadsheet, cells with long values are trimmed, you can print them using 'show cell' command")
    putStrLn ("show cell          - show full value for specified (in next steps) cell")
    putStrLn ("exit               - close the program")
    putStrLn ("help               - show help page")
    putStrLn ("")
