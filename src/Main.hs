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
        Exit -> do 
            putStr "Bye, bye...\n"
            exitSuccess 
    hFlush stdout
    main

