module SpreadsheetOps (
    createSpreadsheet, openSpreadsheet, saveSpreadsheet,
    getValue, calculateValue,
    getCellsRect,
    removeColumn, removeRow, addColumn, addRow,
    modifyCell
) where

import System.IO
import System.IO.Error
import Data.Maybe
import Data.Char
import Data.List
import Data.Binary
import qualified Data.ByteString.Lazy as BS
import Control.Exception

import Model
import Serialization

magic_maxRecursionDepth = 16 :: Int

-- ---------------------------------------------------------------------------
-- Spreadsheet operations
-- ---------------------------------------------------------------------------

-- creates an empty spreadsheet
createSpreadsheet :: Spreadsheet
createSpreadsheet = Spreadsheet []

-- opens spreadsheet from a file
openSpreadsheet :: [Char] -> IO (Either String Spreadsheet)
openSpreadsheet filename = do
                               catch (do
                                      handle <- openFile filename ReadMode
                                      hSetBinaryMode handle True
                                      contents <- BS.hGetContents handle
                                      res <- return (decodeOrFail contents)
                                      case res of
                                          Left (_,_,errstr) -> do
                                                               hClose handle
                                                               return (Left errstr)
                                          Right (_,_,s) -> do
                                                           hClose handle
                                                           return (Right s)
                                      ) errHandler
                               where
                                   errHandler e = return (Left (translateIOError e))


-- saves spreadsheet to file
saveSpreadsheet :: Spreadsheet -> [Char] -> IO (Maybe String)
saveSpreadsheet s filename = do
                                 res <- try (BS.writeFile filename (encode s))
                                 case res of
                                     Left e -> return (Just (translateIOError e))
                                     Right _ -> return Nothing

translateIOError :: IOError -> String
translateIOError (e) = (show e)


-- ---------------------------------------------------------------------------
-- Cells operations
-- ---------------------------------------------------------------------------

-- returns value of cell (c,r) without calculating it
getValue :: Spreadsheet -> Char -> Int -> Either String (Maybe CellVal)
getValue s c r = if not (inRange c magic_cellMinCol magic_cellMaxCol) ||
                    not (inRange r magic_cellMinRow magic_cellMaxRow) then
                     Left "Cell address out of bounds"
                 else Right (getValueInternal s c r)

getValueInternal :: Spreadsheet -> Char -> Int -> Maybe CellVal
getValueInternal s c r = if filtered_len == 1 then Just (val (head filtered))
                         else if filtered_len == 0 then Nothing
                         else error "Inconsistent spreadsheet state: more than one cell with given (col,row)"
                         where
                             filtered = filter (\i -> (col i) == c && (row i) == r) (cells s)
                             filtered_len = length filtered

-- returns cells from rectangle (begc, begr, begc+countc, begr+countr). Doesn't calculate them.
-- returned list is sorted in order specified for cells (lower rows first, see definition of Ord for Cell)
getCellsRect :: Spreadsheet -> Char -> Int -> Int -> Int -> [Cell]
getCellsRect s begc begr countc countr = sort (filter filter_fun (cells s))
                                         where
                                            maxc = chr (ord begc + countc)
                                            maxr = begr+countr
                                            filter_fun = (\x -> inRange (col x) begc maxc && inRange (row x) begr maxr)

getNonEmptyCountInRange :: Spreadsheet -> [(Char, Int)] -> Int
getNonEmptyCountInRange s range = let
                                      cell_values = map (\x -> getValue s (fst x) (snd x)) range
                                      non_empty_cells = filter pred cell_values
                                      pred = \x -> case x of
                                               Right Nothing -> False
                                               Right (Just _) -> True
                                               _ -> error "All the cells for getNonEmptyCountInRange should be valid!"

                                  in
                                      length non_empty_cells

-- calculates value from cell (c,r). For Cell without value, returns 0.0.
-- Returns Right <value> if succeeded and Left <err_string> if error ocurred
calculateValue :: Spreadsheet -> Maybe CellVal -> Either String Double
calculateValue s cv = calculateValueInternal s cv magic_maxRecursionDepth

calculateValueInternal :: Spreadsheet -> Maybe CellVal -> Int -> Either String Double
calculateValueInternal s cv rd =
                          if rd == 0 then Left "Cyclic reference or formula too complicated"
                          else case cv of
                          Nothing -> Right 0.0
                          Just (NumVal a) -> Right a
                          Just (StringVal _) -> Left "Attempting to calculate on string value"
                          Just (SumFunc _range _) -> calculateFunc s (+) 0 _range rd
                          Just (MulFunc _range _) -> calculateFunc s (*) 1 _range rd
                          Just (AvgFunc _range _) -> case (calculateFunc s (+) 0 _range rd) of
                                                         Left err -> Left err
                                                         Right val -> if non_empty == 0 then
                                                                          Left "Division by zero"
                                                                      else
                                                                          Right (val / fromIntegral non_empty)
                                                                      where
                                                                          non_empty = getNonEmptyCountInRange s _range

calculateFunc :: Spreadsheet -> (Double -> Double -> Double) -> Double -> [(Char, Int)] -> Int -> Either String Double
calculateFunc s f neutral _range rd = let
                                       map_range = \x ->
                                           case getValue s (fst x) (snd x) of
                                               Right val -> calculateValueInternal s val (rd-1)
                                               Left err  -> Left err

                                       range_cells_calculated = map map_range _range
                                       isErroneus = \x -> case x of
                                                              Left _ -> True
                                                              Right _ -> False
                                       isValid = \x -> case x of
                                                           Left _ -> False
                                                           Right _ -> True

                                   in
                                       if any isErroneus range_cells_calculated then
                                           -- return first encountered error
                                           head (dropWhile isValid range_cells_calculated)
                                       else
                                           Right (foldl f neutral (map (\(Right x) -> x) range_cells_calculated))

-- sets value of cell (c,r) to v.
-- if a cell already exists on spreadsheet list - modifies its value
-- else - adds new cell to spreadsheet's cells list (at the end)
-- Note: all the error conditions checks should be done in modifyCell
setCellValue :: Spreadsheet -> Char -> Int -> CellVal -> Spreadsheet
setCellValue s c r v = if exists then
                       -- replace existing cell with new one (containing new value)
                       s { cells = replaceItem (cells s) (head filtered) (Cell c r v) }
                   else
                       s { cells = (cells s) ++ [Cell c r v] }
                   where
                       filtered = filter (\i -> (col i) == c && (row i) == r) (cells s)
                       exists = if (length filtered == 1) then True
                                else if (length filtered == 0) then False
                                else error "Inconsistent spreadsheet state: more than one cell with given (col,row)"

-- removes cell (c,r) from cells list of spreadsheet, efectively clearing it out
-- Note: all the error conditions checks should be done in modifyCell
removeCell :: Spreadsheet -> Char -> Int -> Spreadsheet
removeCell s c r = s {cells = filter (\i -> not ((col i) == c && (row i) == r)) (cells s)}


-- removes column with given address from spreadsheet. Effectively, it removes
-- all the cells from this column and moves all the cells from further columns
-- one column to the left
removeColumn :: Spreadsheet -> Char -> Either String (IO Spreadsheet)
removeColumn None _ = Left "No spreadsheet. Create or open a spreadsheet first"
removeColumn s c = if not (inRange c magic_cellMinCol magic_cellMaxCol) then
                       Left "Column address out of range"
                   else
                       Right (return s { cells = map moveOneLeft filtered })
                   where
                       filtered = filter (\x -> (col x) /= c) (cells s)
                       moveOneLeft = \x -> if (col x) > c then (x {col = chr(ord(col x)-1)}) else x

-- removes row with given address from spreadsheet. Effectively, it removes
-- all the cells from this row and moves all the cells from further rows
-- one row up
removeRow :: Spreadsheet -> Int -> Either String (IO Spreadsheet)
removeRow None _ = Left "No spreadsheet. Create or open a spreadsheet first"
removeRow s r = if not (inRange r magic_cellMinRow magic_cellMaxRow) then
                    Left "Row address out of range"
                else
                    Right (return s { cells = map moveOneUp filtered })
                   where
                       filtered = filter (\x -> (row x) /= r) (cells s)
                       moveOneUp = \x -> if (row x) > r then (x {row = (row x)-1}) else x

-- add a new column to spreadsheet
-- The index 'ind' indicates what address will the newly created column have,
-- i.e. addColumn s 'B' adds a new column 'B', moving previous column 'B' and
-- all the successors one column further to the right. If there are non-empty
-- cells in column magic_cellMaxCol, an error is returned.
addColumn :: Spreadsheet -> Char -> Either String (IO Spreadsheet)
addColumn None _ = Left "No spreadsheet. Create or open a spreadsheet first"
addColumn s ind = if not (inRange ind magic_cellMinCol magic_cellMaxCol) then
                      Left "Column address out of range"
                  else if (any (\x -> (col x) == magic_cellMaxCol) (cells s)) then
                      Left "Non-empty cells in last column"
                  else
                      Right (return s { cells = map moveOneRight (cells s) })
                      where
                          moveOneRight = \x -> if (col x) >= ind then (x {col = chr(ord(col x)+1)}) else x

-- add a new row to spreadsheet
-- The index 'ind' indicates what address will the newly created row have,
-- i.e. addRow s 3 adds a new row 3, moving previous row 3 and
-- all the successors one row down. If there are non-empty cells in row
-- magic_cellMaxRow, an error is returned.
addRow :: Spreadsheet -> Int -> Either String (IO Spreadsheet)
addRow None _ = Left "No spreadsheet. Create or open a spreadsheet first"
addRow s ind = if not (inRange ind magic_cellMinRow magic_cellMaxRow) then
                   Left "Row address out of range"
               else if (any (\x -> (row x) == 50) (cells s)) then
                   Left "Non-empty cells in last row"
               else
                   Right (return s { cells = map moveOneDown (cells s) })
                   where
                       moveOneDown = \x -> if (row x) >= ind then (x {row = (row x)+1}) else x

-- modifies value of a cell (c,r). If v is StringVal "" (i.e. just an empty string),
-- it removes the cell from spreadsheet list. Otherwise, it sets a value of the cell
-- to v.
modifyCell :: Spreadsheet -> Char -> Int -> CellVal -> Either String (IO Spreadsheet)
modifyCell None _ _ _ = Left "No spreadsheet. Create or open a spreadsheet first"
modifyCell s c r v = if not (inRange c magic_cellMinCol magic_cellMaxCol) then
                         Left "Column address not in range"
                     else if not (inRange r magic_cellMinRow magic_cellMaxRow) then
                         Left "Row address not in range"
                     else
                         case v of
                             StringVal "" -> Right (return (removeCell s c r))
                             _            -> Right (return (setCellValue s c r v))

-- ---------------------------------------------------------------------------
-- Utils
-- ---------------------------------------------------------------------------

replaceItem :: Eq a => [a] -> a -> a -> [a]
replaceItem [] _ _ = []
replaceItem (x:xs) old new = if x == old then (new : xs) else (x : replaceItem xs old new)

inRange :: Ord a => a -> a -> a -> Bool
inRange a lower upper = (a >= lower) && (a <= upper)



