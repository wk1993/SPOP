module Command (
    Command(..)
) where

-- type for representing input command (text typed by user is parsed, parser returns Command)
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
    ShowCell |
    Exit |
    Help |
    UnknownCommand
