module SpreadsheetOps (
    createSpreadsheet, openSpreadsheet, saveSpreadsheet,
    getValue, calculateValue, setValue,
    getCellsRect,
    removeColumn, removeRow, addColumn, addRow,
    modifyCell
) where

import System.IO
import Data.Maybe
import Data.Char
import Data.List
import Data.Binary

import Model
import Serialization

-- ---------------------------------------------------------------------------
-- Spreadsheet operations
-- ---------------------------------------------------------------------------

-- creates an empty spreadsheet
createSpreadsheet :: Spreadsheet
createSpreadsheet = Spreadsheet Nothing []

-- opens spreadsheet from a file
openSpreadsheet :: [Char] -> IO Spreadsheet
openSpreadsheet filename = do
                               s <- decodeFile filename
                               return s

-- saves spreadsheet to file
saveSpreadsheet :: Spreadsheet -> [Char] -> IO ()
saveSpreadsheet s filename = encodeFile filename s

-- closes spreadsheet file
--closeSpreadsheet :: Spreadsheet -> IO ()
--closeSpreadsheet s = if isJust (io_handle s) then hClose (fromJust (io_handle s)) else return ()


-- ---------------------------------------------------------------------------
-- Cells operations
-- ---------------------------------------------------------------------------

-- returns value of cell (c,r) without calculating it
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

-- calculates value from cell (c,r). For Cell without value, returns 0.0. For StringVal throws error.
-- In case of errors in computation returns NaN
-- TODO test it
calculateValue :: Spreadsheet -> Char -> Int -> Double
calculateValue s c r =  let cell = getValue s c r in
                        case cell of
                            Nothing -> 0.0
                            Just (NumVal a) -> a
                            Just (StringVal _) -> error "String value"
                            Just (SumFunc _range _) -> calculateFunc s (+) 0 _range
                            Just (MulFunc _range _) -> calculateFunc s (*) 1 _range
                            Just (AvgFunc _range _) -> (calculateFunc s (+) 0 _range) / (fromIntegral (length _range))

calculateFunc :: Spreadsheet -> (Double -> Double -> Double) -> Double -> [(Char, Int)] -> Double
calculateFunc s f neutral _range = foldl f neutral (map (\x -> calculateValue s (fst x) (snd x)) _range)

-- sets value of cell (c,r) to v.
-- if a cell already exists on spreadsheet list - modifies its value
-- else - adds new cell to spreadsheet's cells list (at the end)
-- TODO removing cell if its value is (StringVal "")
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

--parseRange :: String -> [(Char, Int)]
--parseRange str =

-- remove column from spreadsheet
-- TODO protect from indices out of bounds
-- TODO test it
removeColumn :: Spreadsheet -> Char -> IO Spreadsheet
removeColumn s c = return s { cells = map moveOneLeft filtered }
                   where
                       filtered = filter (\x -> (col x) /= c) (cells s)
                       moveOneLeft = \x -> if (col x) > c then (x {col = chr(ord(col x)-1)}) else x

-- remove row from spreadsheet
-- TODO protect from indices out of bounds
-- TODO test it
removeRow :: Spreadsheet -> Int -> IO Spreadsheet
removeRow s r = return s { cells = map moveOneUp filtered }
                   where
                       filtered = filter (\x -> (row x) /= r) (cells s)
                       moveOneUp = \x -> if (row x) > r then (x {row = (row x)-1}) else x

-- add a new column to spreadsheet
-- The index 'ind' indicates what address will the newly created column have,
-- i.e. addColumn s 'B' adds a new column 'B', moving previous column 'B' and
-- all the successors one column further to the right. If there are cells in
-- column 'J', an error is thrown.
-- TODO protect from indices out of bounds
-- TODO test it
addColumn :: Spreadsheet -> Char -> IO Spreadsheet
addColumn s ind = if (any (\x -> (col x) == 'J') (cells s)) then
                      error "Non-empty cells in last column"
                  else
                      return s { cells = map moveOneRight (cells s) }
                      where
                          moveOneRight = \x -> if (col x) >= ind then (x {col = chr(ord(col x)+1)}) else x

-- add a new row to spreadsheet
-- The index 'ind' indicates what address will the newly created row have,
-- i.e. addRow s 3 adds a new column 3, moving previous column 3 and
-- all the successors one column down. If there are cells in column 50, an error is thrown.
-- TODO protect from indices out of bounds
-- TODO test it
addRow :: Spreadsheet -> Int -> IO Spreadsheet
addRow s ind = if (any (\x -> (row x) == 50) (cells s)) then
                   error "Non-empty cells in last row"
               else
                   return s { cells = map moveOneDown (cells s) }
                   where
                       moveOneDown = \x -> if (row x) >= ind then (x {row = (row x)+1}) else x

-- modify cell value
-- TODO protect from indices out of bounds
modifyCell :: Spreadsheet -> Char -> Int -> CellVal -> IO Spreadsheet
modifyCell s c r v = return (setValue s c r v)

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

-- basicTest = getValue (setValue (createSpreadsheet) 'a' 1 (StringVal "abc")) 'a' 1



