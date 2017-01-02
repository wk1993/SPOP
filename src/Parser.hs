module Parser (
    parse
) where

import Command
import Data.Char

parse :: String -> Command
parse unparsedCommand = case unparsedCommand of
    'o':'p':'e':'n':' ':file -> OpenSpreadsheet file
    's':'a':'v':'e':' ':file -> SaveSpreadsheet file
    "exit" -> Exit



