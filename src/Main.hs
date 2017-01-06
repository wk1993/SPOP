import System.IO
import System.Exit
import Command
import Parser
import Model
import Show

cellValA =  StringVal "a"
cellValB =  StringVal "b"
cellValC =  StringVal "c"
cellValD =  StringVal "d"
cellValE =  StringVal "e"

cellA = Cell 'A' 1 cellValA
cellB = Cell 'A' 2 cellValB
cellC = Cell 'B' 1 cellValC
cellD = Cell 'B' 2 cellValD
cellE = Cell 'C' 3 cellValE

spreadsheet = Spreadsheet Nothing [cellA, cellB, cellC, cellD, cellE]


main = do
    putStrLn ("Welcome to SPOP project")
    putStrLn ("Authors: Krystian Kieczka, Wikor Kusmirek")
    putStrLn ("To print list of available commands, type 'help'")
    iterateLoop spreadsheet

iterateLoop actualSpreadSheet = do
    putStr "% "
    hFlush stdout
    unparsedCommand <- getLine
    let
        command = parse unparsedCommand
    case command of 
        OpenSpreadsheet file -> do 
            putStrLn ("Opening spreadsheet from " ++ file ++ " file...")
            -- TODO --
        SaveSpreadsheet file -> do 
            putStrLn ("Saving spreadsheet to " ++ file ++ " file...")
            -- TODO --
        RemoveColumn id -> do 
            putStrLn ("Removing column " ++ id ++ "...")
            -- TODO --
        RemoveRow id -> do 
            putStrLn ("Removing row " ++ id ++ "...")
            -- TODO --
        CreateSpreadsheet -> do 
            putStrLn ("Creating a new spreadsheet...")
            -- TODO --
        ShowSpreadsheet -> do 
            putStrLn ("Showing spreadsheet...")
            showSpreadsheet actualSpreadSheet
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


