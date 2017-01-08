module SpreadsheetOps (
    createSpreadsheet, openSpreadsheet, saveSpreadsheet, closeSpreadsheet,
    getValue, calculateValue, setValue,
    getCellsRect,
    removeColumn, removeRow
) where

import System.IO
import Data.Maybe
import Data.Char
import Data.List

import Model

-- ---------------------------------------------------------------------------
-- Spreadsheet operations
-- ---------------------------------------------------------------------------

-- creates an empty spreadsheet
createSpreadsheet :: Spreadsheet
createSpreadsheet = Spreadsheet Nothing []

-- opens spreadsheet from a file
openSpreadsheet :: [Char] -> IO Spreadsheet
openSpreadsheet filename = do
                            handle <- openFile filename ReadWriteMode
                            contents <- hGetContents handle
                            parsed_cells <- parseSpreadsheetFile contents
                            return (Spreadsheet (Just handle) parsed_cells)

-- saves spreadsheet to file
saveSpreadsheet :: Spreadsheet -> [Char] -> IO ()
saveSpreadsheet s filename = error "Not implemented"

-- closes spreadsheet file
closeSpreadsheet :: Spreadsheet -> IO ()
closeSpreadsheet s = if isJust (io_handle s) then hClose (fromJust (io_handle s)) else return ()


-- ---------------------------------------------------------------------------
-- Cells operations
-- ---------------------------------------------------------------------------

-- returns value of cell (i,j) without calculating it
getValue :: Spreadsheet -> Char -> Int -> Maybe CellVal
getValue s c r = if filtered_len == 1 then Just (val (head filtered))
                    else if filtered_len == 0 then Nothing
                    else error "Inconsistent spreadsheet state: more than one cell with given (col,row)"
                 where
                    filtered = filter (\i -> (col i) == c && (row i) == r) (cells s)
                    filtered_len = length filtered

-- returns cells from rectangle (begc, begr, begc+countc, begr+countr). Doesn't calculate them.
-- returned list is sorted in in order specified for cells (lower rows first, see definition of Ord for Cell)
getCellsRect :: Spreadsheet -> Char -> Int -> Int -> Int -> [Cell]
getCellsRect s begc begr countc countr = sort (filter filter_fun (cells s))
                                         where
                                            maxc = chr (ord begc + countc)
                                            maxr = begr+countr
                                            filter_fun = (\x -> inRange (col x) begc maxc && inRange (row x) begr maxr)

-- calculates value from cell (c,r). For Cell with value StringVal and without value, returns 0.0.
-- In case of errors in computation returns NaN
calculateValue :: Spreadsheet -> Char -> Int -> Double
calculateValue s c r =  error "Not implemented"

-- sets value of cell (c,r) to v.
-- if a cell already exists on spreadsheet list - modifies its value
-- else - adds new cell to spreadsheet's cells list (at the end)
setValue :: Spreadsheet -> Char -> Int -> CellVal -> Spreadsheet
setValue s c r v = if exists then
                       -- replace existing cell with new one (containing new value)
                       s { cells = replaceItem (cells s) (head filtered) (Cell c r v) }
                   else
                       s { cells = (cells s) ++ [Cell c r v] }
                   where
                       filtered = filter (\i -> (col i) == c && (row i) == r) (cells s)
                       exists = if (length filtered == 1) then True
                                else if (length filtered == 0) then False
                                else error "Inconsistent spreadsheet state: more than one cell with given (col,row)"



parseSpreadsheetFile :: [Char] -> IO [Cell] -- TODO shouldn't it be just '[Cell]' without IO?
parseSpreadsheetFile content = error "Not implemented"

-- remove column from spreadsheet
removeColumn :: Spreadsheet -> Char -> IO Spreadsheet
removeColumn s c = do
                    return (s)
                    -- TODO --

-- remove row from spreadsheet
removeRow :: Spreadsheet -> Int -> IO Spreadsheet
removeRow s r = do
                    return (s)
                    -- TODO --

-- ---------------------------------------------------------------------------
-- Utils
-- ---------------------------------------------------------------------------

replaceItem :: Eq a => [a] -> a -> a -> [a]
replaceItem [] _ _ = []
replaceItem (x:xs) old new = if x == old then (new : xs) else (x : replaceItem xs old new)

inRange :: Ord a => a -> a -> a -> Bool
inRange a lower upper = (a >= lower) && (a <= upper)

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

basicTest = getValue (setValue (createSpreadsheet) 'a' 1 (StringVal "abc")) 'a' 1



