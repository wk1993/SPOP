module Parser (
    parse
) where

import Command

parse :: String -> Command
parse unparsedCommand = case unparsedCommand of
    "exit" -> Exit



