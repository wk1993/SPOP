module Command (
    Command(..)
) where

data Command =
    OpenSpreadsheet String |
    SaveSpreadsheet String |
    RemoveColumn Char |
    RemoveRow Int |
    AddColumn Char |
    AddRow Int |
    ModifyCell |
    CreateSpreadsheet |
    ShowSpreadsheet |
    Exit |
    Help |
    UnknownCommand
