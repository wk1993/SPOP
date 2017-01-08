module Command (
    Command(..)
) where

data Command =
    OpenSpreadsheet String |
    SaveSpreadsheet String |
    RemoveColumn Char |
    RemoveRow Int |
    AddColumn |
    AddRow |
    CreateSpreadsheet |
    ShowSpreadsheet |
    Exit |
    Help |
    UnknownCommand
