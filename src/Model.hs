module Model (
    CellVal(..), Cell(..), Spreadsheet(..)
) where

import System.IO


--
-- CellVal - type representing a single (logical) value of a cell. It can contain
-- following types of values:
-- - simple strings (e.g. "Total sum:")
-- - double values (e.g. 1.234)
-- - sum function: "=sum(<range>)"
-- - multiplication function: "=mul(<range>)"
-- - average function: "=avg(<range>)"
-- where <range> is defined as in popular spreadsheets software: it contains semicolon
-- separated cell descriptions which can describe a single cell (e.g. "A1") or range
-- (e.g. "A2:B4"), e.g. "A4;B3;A8:B14;C5".
-- Function names ("sum", "avg", "mul") are case insensitive.
--
-- Showing this data type doesn't perform any computation, use SpreadsheetOps::calculateValue
-- to get computed value of a cell.
--

data CellVal = StringVal String
               | NumVal Double
               -- functions take list of cells coordinates and the string typed by user (used when showing)
               | SumFunc { range :: [(Char, Int)], str :: String}
               | MulFunc { range :: [(Char, Int)], str :: String}
               | AvgFunc { range :: [(Char, Int)], str :: String}

instance Show CellVal where -- TODO restore after debugging
    show (StringVal a) = "str " ++ a                            -- a
    show (NumVal a) = "num " ++ show a                          -- show a
    show (SumFunc t s) = "sum " ++ show t ++ ", txt: " ++ s     -- s
    show (MulFunc t s) = "mul " ++ show t ++ ", txt: " ++ s     -- s
    show (AvgFunc _ s) = s

-- ---------------------------------------------------------------------------

--
-- Cell - type representing a single cell in spreadsheet. It contains:
-- - col - column coordinate. Should be 'A'-'Z'
-- - row - row coordinate. An integer from range [1,100].
-- - val - value of this cell
--

data Cell = Cell {
    col :: Char,
    row :: Int,
    val :: CellVal
}

-- Two cells are 'equal' if they have the same coordinations
-- This means that, at the moment, VALUES of cells don't matter for
-- their equality!
instance Eq Cell where
    c1 == c2 = (col c1) == (col c2) && (row c1) == (row c2)

-- The order of cells is defined as follows: firstly the row numbers are compared
-- and cell with lower row number is "smaller". If both cells are in the same row,
-- the one with lower column character is "smaller".
instance Ord Cell where
    c1 <= c2 | (row c1) == (row c2) && (col c1) == (col c2)   = True
             | (row c1) < (row c2)                            = True
             | (row c1) == (row c2) && (col c1) <= (col c2)   = True
             | otherwise                                      = False

-- ---------------------------------------------------------------------------

--
-- Spreadsheet - type representing a single spreadsheet. It contains:
-- - io_handle - a Handle, if this spreadsheet is connected with a file (i.e. was
--               opened from a file or saved to one), or Nothing
-- - cells - a list of Cells with defined values.
--

data Spreadsheet = Spreadsheet {
    io_handle :: Maybe Handle,
    cells :: [Cell]
} | None
