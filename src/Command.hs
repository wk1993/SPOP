module Command (
    Command(..)
) where

data Command =
    OpenSpreadsheet String |
    SaveSpreadsheet String |
    Exit
