module Show (
    showSpreadsheet,
    showAvailableCommands,
    charToString
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
                      then putStr $ (showCellVal s (val (cells!!(last((findPos cells column))))) ++ "\t\t")
                      else putStr $ "\t\t") nameOfColumns
    putStr "\n"

showCellVal :: Spreadsheet -> CellVal -> String
showCellVal s cv = case cv of
                     StringVal a -> a
                     other       -> case (calculateValue s (Just other)) of
                                        Left err -> "Error: " ++ err
                                        Right val -> show val

showSpreadsheetNameOfColumns :: [Char] -> IO ()
showSpreadsheetNameOfColumns nameOfColumns = do
    putStr $ ("\t\t")
    mapM_ (\nameOfColumn -> (putStr $ (charToString (nameOfColumn) ++ "\t\t"))) (sort nameOfColumns)
    putStr "\n"

findPos :: [Cell] -> Char -> [Int]
findPos cells column = [index | (index, c) <- zip [0..] cells, column == col c]

showAvailableCommands :: IO ()
showAvailableCommands = do 
    putStrLn ("\nAvailable commands:")
    putStrLn ("open filename      - open spreadsheet from file")
    putStrLn ("save filename      - save spreadsheet to file")
    putStrLn ("remove column id   - remove column with specified id, id should be a single letter")
    putStrLn ("remove row id      - remove row with specified id, id should be a number")
    putStrLn ("add column         - add a new column to the spreadsheet, id for this column will be designated automatically")
    putStrLn ("add row            - add a new row to the spreadsheet, id for this row will be designated automatically")
    putStrLn ("modify cell        - modify value for specified (in next steps) cell")
    putStrLn ("create             - create new spreadsheet")
    putStrLn ("show               - show content of actual spreadsheet")
    putStrLn ("exit               - close the program")
    putStrLn ("help               - show help page")
    putStrLn ("")
