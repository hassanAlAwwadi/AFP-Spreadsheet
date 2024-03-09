{-# LANGUAGE TypeData, TypeFamilies, AllowAmbiguousTypes #-}
module Spreadsheet.Input where
import Spreadsheet.Unit 

type data Input
  = Aether -- ^ time events and the like, if we want to support that
  | Cell   -- ^ single cell update
  | Table  -- ^ continuous cells update

type INPUT_REP :: forall t a. t -> a  
type family INPUT_REP t where
  INPUT_REP (Aether) = () 
  INPUT_REP (Cell  ) = Formula
  INPUT_REP (Table ) = [[Formula]]

type INPUT_LOC :: forall t a. t -> a  
type family INPUT_LOC t where
  INPUT_LOC (Aether) = () 
  INPUT_LOC (Cell  ) = (Int, Int)
  INPUT_LOC (Table ) = ((Int,Int), (Int,Int))