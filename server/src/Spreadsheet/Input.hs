{-# LANGUAGE TypeData, TypeFamilies, AllowAmbiguousTypes #-}
module Spreadsheet.Input where
import Spreadsheet.Unit 

data Input
  = Aether  
  | Cell (Int, Int) Formula
--  | Table (Int, Int) (Int, Int) [[Formula]] -- ^ mass update, ignored for now