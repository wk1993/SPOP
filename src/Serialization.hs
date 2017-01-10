module Serialization where

import Data.Binary
import Model
import CellValParser


instance Binary CellVal where
    put (StringVal a) = do put ('s' :: Char)
                           put a

    put (NumVal a)    = do put ('n' :: Char)
                           put a

    put (SumFunc _ s) = do put ('f' :: Char)
                           put s

    put (MulFunc _ s) = do put ('f' :: Char)
                           put s

    put (AvgFunc _ s) = do put ('f' :: Char)
                           put s

    get = do t <- get :: Get Char
             case t of
                 's' -> do a <- get
                           return (StringVal a)

                 'n' -> do a <- get
                           return (NumVal a)

                 'f' -> do s <- get
                           return (evalFromString s)

                 _ -> error "Malformed input"


instance Binary Cell where
    put c = do put (col c)
               put (row c)
               put (val c)

    get = do _c <- get
             _r <- get
             _v <- get
             return Cell {col = _c, row = _r, val = _v}


instance Binary Spreadsheet where
    put s = put (cells s)

    get = do _cells <- get
             return Spreadsheet {io_handle = Nothing, cells = _cells}