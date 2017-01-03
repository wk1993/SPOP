module Parser (
    parse
) where

import Command
import Data.Char

parse :: String -> Command
parse unparsedCommand = case unparsedCommand of
    'o':'p':'e':'n':' ':file -> OpenSpreadsheet file
    's':'a':'v':'e':' ':file -> SaveSpreadsheet file
    'r':'e':'m':'o':'v':'e':' ':'c':'o':'l':'u':'m':'n':' ':id -> RemoveColumn id
    'r':'e':'m':'o':'v':'e':' ':'r':'o':'w':' ':id -> RemoveRow id
    "create" -> CreateSpreadsheet
    "show" -> ShowSpreadsheet
    "exit" -> Exit
    "help" -> Help
    _ -> UnknownCommand



