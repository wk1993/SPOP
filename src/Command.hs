module Command (
    Command(..)
) where

data Command =
    OpenSpreadsheet String |
    SaveSpreadsheet String |
    CreateSpreadsheet |
    ShowSpreadsheet |
    Exit |
    UnknownCommand String
