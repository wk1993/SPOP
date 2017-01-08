module Parser (
    parse
) where

import Command
import Data.Char
import Text.Read (readMaybe)

parse :: String -> Command
parse unparsedCommand = case unparsedCommand of
    'o':'p':'e':'n':' ':file -> OpenSpreadsheet file
    's':'a':'v':'e':' ':file -> SaveSpreadsheet file
    'r':'e':'m':'o':'v':'e':' ':'c':'o':'l':'u':'m':'n':' ':id -> if length id == 1 then RemoveColumn (head id) else UnknownCommand
    'r':'e':'m':'o':'v':'e':' ':'r':'o':'w':' ':id -> if (readMaybe id :: Maybe Int) /= Nothing then RemoveRow (read id :: Int) else UnknownCommand
    "add column" -> AddColumn
    "add row" -> AddRow
    "create" -> CreateSpreadsheet
    "show" -> ShowSpreadsheet
    "exit" -> Exit
    "help" -> Help
    _ -> UnknownCommand



