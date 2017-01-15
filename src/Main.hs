import System.IO
import System.Exit
import Command
import Parser
import Model
import Show
import SpreadsheetOps
import CellValParser

-- first, opening function in program
main = do
    putStrLn ("Welcome to SPOP project")
    putStrLn ("Authors: Krystian Kieczka, Wiktor Kusmirek")
    putStrLn ("To print list of available commands, type 'help'")
    iterateLoop None

-- loop for main program
iterateLoop actualSpreadSheet = do
    putStr "% "
    hFlush stdout
    unparsedCommand <- getLine
    let
        command = parse unparsedCommand
        handleErrors = \x -> case x of
                             Left e -> do
                                putStrLn("Error: " ++ e)
                                return actualSpreadSheet
                             Right s -> s
    case command of 
        OpenSpreadsheet file -> do 
            putStrLn ("Opening spreadsheet from " ++ file ++ " file...")
            actualSpreadSheet <- do res <- (openSpreadsheet file)
                                    case res of
                                     Left e -> do
                                         putStrLn("Error: " ++ e)
                                         return actualSpreadSheet
                                     Right s -> return s
            iterateLoop actualSpreadSheet
        SaveSpreadsheet file -> do 
            putStrLn ("Saving spreadsheet to " ++ file ++ " file...")
            err <- saveSpreadsheet actualSpreadSheet file
            case err of
                Just s -> do
                    putStrLn("Error: " ++ s)
                Nothing -> return ()
            iterateLoop actualSpreadSheet
        RemoveColumn id -> do 
            putStrLn ("Removing column " ++ show id ++ "...")
            actualSpreadSheet <- case (removeColumn actualSpreadSheet id) of
                                     Left e -> do
                                         putStrLn("Error: " ++ e)
                                         return actualSpreadSheet
                                     Right s -> s
            iterateLoop actualSpreadSheet
        RemoveRow id -> do 
            putStrLn ("Removing row " ++ show id ++ "...")
            actualSpreadSheet <- handleErrors (removeRow actualSpreadSheet id)
            iterateLoop actualSpreadSheet
        AddColumn id -> do 
            putStrLn ("Adding a new column to spreadsheet...")
            actualSpreadSheet <- handleErrors (addColumn actualSpreadSheet id)
            iterateLoop actualSpreadSheet
        AddRow id -> do 
            putStrLn ("Adding a new row to spreadsheet...")
            actualSpreadSheet <- handleErrors (addRow actualSpreadSheet id)
            iterateLoop actualSpreadSheet
        ModifyCell -> do 
            putStr "column: "
            hFlush stdout
            line <- getLine
            if isSingleLetter line then 
                putStrLn ("Column value is valid...") 
            else do
                putStrLn ("Column value is invalid, it should be single character")
                iterateLoop actualSpreadSheet
            let
                column = (head (read ("\"" ++ line ++ "\"") :: String))
            putStr "row: "
            hFlush stdout
            line <- getLine
            if isValidInt line then 
                putStrLn ("Row value is valid...") 
            else do
                putStrLn ("Row value is invalid, it should be integer")
                iterateLoop actualSpreadSheet
            let
                row = read line :: Int
            putStr "new value (leave blank to remove actual cell value): "
            hFlush stdout
            line <- getLine
            let
                newCellValue = evalFromString line
            putStrLn ("Modifing cell " ++ (charToString column) ++ show row ++ " with value equal to " ++ (show newCellValue) ++ " ...")
            actualSpreadSheet <- handleErrors (modifyCell actualSpreadSheet column row newCellValue)
            iterateLoop actualSpreadSheet
        CreateSpreadsheet -> do 
            putStrLn ("Creating a new spreadsheet...")
            iterateLoop createSpreadsheet
        ShowSpreadsheet -> do 
            putStrLn ("Showing spreadsheet...")
            showSpreadsheet actualSpreadSheet
        ShowCell -> do 
            putStr "column: "
            hFlush stdout
            line <- getLine
            if isSingleLetter line then 
                putStrLn ("Column value is valid...") 
            else do
                putStrLn ("Column value is invalid, it should be single character")
                iterateLoop actualSpreadSheet
            let
                column = (head (read ("\"" ++ line ++ "\"") :: String))
            putStr "row: "
            hFlush stdout
            line <- getLine
            if isValidInt line then 
                putStrLn ("Row value is valid...") 
            else do
                putStrLn ("Row value is invalid, it should be integer")
                iterateLoop actualSpreadSheet
            let
                row = read line :: Int
            putStrLn ("Showing full value of cell " ++ (charToString column) ++ show row ++ " ...")
            putStrLn ("Full cell value: " ++ showFullCell actualSpreadSheet column row)
        Exit -> do 
            putStrLn ("Bye, bye...")
            exitSuccess 
        Help -> do 
            putStrLn ("Printing help...")
            showAvailableCommands
        UnknownCommand -> do 
            putStrLn ("Unknown command, type 'help' to show help page...")
    hFlush stdout
    iterateLoop actualSpreadSheet


