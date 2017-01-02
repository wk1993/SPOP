import System.IO


main = do
    putStr "% "
    hFlush stdout
    lineFromUser <- getLine
    hFlush stdout
    main

