module Model (
    CellVal(..), Cell(..), Spreadsheet(..)
) where

import System.IO


--
-- CellVal - type representing a single (logical) value of a cell. It can contain
-- following types of values:
-- - simple strings (e.g. "Total sum:")
-- - double values (e.g. 1.234)
-- - sum/multiplication/average functions
--
-- Showing this data type doesn't perform any computation, use SpreadsheetOps::calculateValue
-- to get computed value of a cell.
--

data CellVal = StringVal [Char] | NumVal Double | SumFunc | MulFunc | AvgFunc

instance Show CellVal where
    show (StringVal a) = a
    show (NumVal a) = show a
    -- show SumFunc = <sum formula typed by user>
    show _ = "<CellVal not showable yet>"
-- ---------------------------------------------------------------------------

--
-- Cell - type representing a single cell in spreadsheet. It contains:
-- - col - column coordinate. Should be 'A'-'Z'
-- - row - row coordinate. A positive, non-zero integer.where
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
}
