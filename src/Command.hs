module Command (
    Command(..)
) where

data Command =
    OpenSpreadsheet String |
    SaveSpreadsheet String |
    RemoveColumn String |
    RemoveRow String |
    CreateSpreadsheet |
    ShowSpreadsheet |
    Exit |
    Help |
    UnknownCommand
