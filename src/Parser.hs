module Parser (
    parse, isSingleLetter, isValidInt
) where

import Command
import Data.Char
import Text.Read (readMaybe)

-- parses input string typed by user and recognizes specified command
parse :: String -> Command
parse unparsedCommand = case unparsedCommand of
    'o':'p':'e':'n':' ':file -> OpenSpreadsheet file
    's':'a':'v':'e':' ':file -> SaveSpreadsheet file
    'r':'e':'m':'o':'v':'e':' ':'c':'o':'l':'u':'m':'n':' ':id -> if length id == 1 then RemoveColumn (head id) else UnknownCommand
    'r':'e':'m':'o':'v':'e':' ':'r':'o':'w':' ':id -> if (readMaybe id :: Maybe Int) /= Nothing then RemoveRow (read id :: Int) else UnknownCommand
    'a':'d':'d':' ':'c':'o':'l':'u':'m':'n':' ':id -> if length id == 1 then AddColumn (head id) else UnknownCommand
    'a':'d':'d':' ':'r':'o':'w':' ':id -> if (readMaybe id :: Maybe Int) /= Nothing then AddRow (read id :: Int) else UnknownCommand
    "modify cell" -> ModifyCell
    "create" -> CreateSpreadsheet
    "show" -> ShowSpreadsheet
    "show cell" -> ShowCell
    "exit" -> Exit
    "help" -> Help
    _ -> UnknownCommand

-- checks if string is a single letter
isSingleLetter :: String -> Bool
isSingleLetter inputString = (readMaybe ("\"" ++ inputString ++ "\"") :: Maybe String) /= Nothing && (length inputString == 1)

-- checks if string represents valid int number
isValidInt :: String -> Bool
isValidInt inputString = (readMaybe inputString :: Maybe Int) /= Nothing
