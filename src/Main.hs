import System.IO
import System.Exit
import Command
import Parser

main = do
    putStrLn ("Welcome to SPOP project")
    putStrLn ("Authors: Krystian Kieczka, Wikor Kusmirek")
    putStrLn ("To print list of available commands, type 'help'")
    iterateLoop null

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
            -- TODO --
        Exit -> do 
            putStrLn ("Bye, bye...")
            exitSuccess 
        Help -> do 
            putStrLn ("Printing help...")
            printAvailableCommands
        UnknownCommand -> do 
            putStrLn ("Unknown command, type 'help' to show help page...")
    hFlush stdout
    iterateLoop actualSpreadSheet

printAvailableCommands = do 
    putStrLn ("\nAvailable commands:")
    putStrLn ("open filename      - open spreadsheet from file")
    putStrLn ("save filename      - save spreadsheet to file")
    putStrLn ("remove column id   - remove column with specified id")
    putStrLn ("remove row id      - remove row with specified id")
    putStrLn ("create             - create new spreadsheet")
    putStrLn ("show               - show content of actual spreadsheet")
    putStrLn ("exit               - close the program")
    putStrLn ("help               - show help page")
    putStrLn ("")
