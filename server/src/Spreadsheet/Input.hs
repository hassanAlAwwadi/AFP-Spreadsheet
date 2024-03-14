{-# LANGUAGE TypeData, TypeFamilies, AllowAmbiguousTypes #-}
module Spreadsheet.Input where
import Formula 

data Input
  = Cell (Int, Int) (Formula Int)
--  | Table (Int, Int) (Int, Int) [[Formula]] -- ^ mass update, ignored for now
--  | Aether -- ^ time driven updates?