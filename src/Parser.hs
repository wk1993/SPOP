module Parser (
    parse
) where

import Command
import Data.Char

parse :: String -> Command
parse unparsedCommand = case unparsedCommand of
    'o':'p':'e':'n':' ':file -> OpenSpreadsheet file
    's':'a':'v':'e':' ':file -> SaveSpreadsheet file
    "create" -> CreateSpreadsheet
    "show" -> ShowSpreadsheet
    "exit" -> Exit
    "help" -> Help
    unparsedCommand -> UnknownCommand unparsedCommand



