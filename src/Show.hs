module Show (
    showSpreadsheet,
    showAvailableCommands
) where

import Data.List
import Data.Maybe
import Data.Char
import Model
import SpreadsheetOps

showSpreadsheet :: Spreadsheet -> IO ()
showSpreadsheet None = putStrLn ("Your spreadsheet is not set, create or read it from file!")
showSpreadsheet (Spreadsheet _ []) = putStrLn ("Your spreadsheet is empty!")
showSpreadsheet spreadsheet = do
    showSpreadsheetNameOfColumns (nub([col cell | cell <- (cells spreadsheet)]))
    showSpreadsheetLoop spreadsheet 1 (row (last (sort (cells spreadsheet)))) (nub([col cell | cell <- (cells spreadsheet)]))

showSpreadsheetLoop :: Spreadsheet -> Int -> Int -> [Char] -> IO ()
showSpreadsheetLoop spreadsheet actualRowId lastRowId nameOfColumns = do
    showSpreadsheetRow actualRowId (getCellsRect spreadsheet 'A' actualRowId 100 0) nameOfColumns
    if actualRowId == lastRowId then
        return ()
    else
        showSpreadsheetLoop spreadsheet (actualRowId+1) lastRowId nameOfColumns

showSpreadsheetRow :: Int -> [Cell] -> [Char] -> IO ()
showSpreadsheetRow rowId cells nameOfColumns = do
    putStr $ (show rowId ++ "\t")
    mapM_ (\column -> if elem column [col cell | cell <- cells] 
                      then putStr $ (show (val (cells!!(last((findPos cells column))))) ++ "\t") 
                      else putStr $ "\t") nameOfColumns
    putStr "\n"

showSpreadsheetNameOfColumns :: [Char] -> IO ()
showSpreadsheetNameOfColumns nameOfColumns = do
    putStr $ ("\t")
    mapM_ (\nameOfColumn -> (putStr $ (show (nameOfColumn) ++ "\t"))) (sort nameOfColumns)
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
    putStrLn ("create             - create new spreadsheet")
    putStrLn ("show               - show content of actual spreadsheet")
    putStrLn ("exit               - close the program")
    putStrLn ("help               - show help page")
    putStrLn ("")
