import System.IO
import System.Exit
import Command
import Parser

main = do
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
        Exit -> do 
            putStrLn ("Bye, bye...")
            exitSuccess 
    hFlush stdout
    main

