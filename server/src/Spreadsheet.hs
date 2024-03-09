{-# LANGUAGE TypeData, TypeFamilies, AllowAmbiguousTypes #-}

module Spreadsheet where
import Spreadsheet.Input
import Spreadsheet.Unit 

import Data.Map(Map)
import Data.List(foldl')
import qualified Data.Map as M

-- Placeholders
type Arr   = Map (Int, Int) Formula
type Graph = Map (Int, Int) [(Int, Int)]

data Spreadsheet = S Arr Graph 

class Handle a where 
  handle :: INPUT_LOC a -> INPUT_REP a -> Spreadsheet -> Spreadsheet 

instance Handle (Aether) where 
  handle () () = id 

instance Handle (Cell) where 
  handle (x,y) v (S a g) = S (M.insert (x,y) v a) g

instance Handle (Table) where 
  handle ((t,l), (b,r)) vvs (S a g) = let 
    x_keyed   = map (zip [l..r]) vvs  -- [[(x,v)]]
    x_y_keyed = zipWith (\y vs -> zip (repeat y) vs) [t..b] x_keyed -- [[(y,(x,v))]]
    collapsed = concat x_y_keyed -- [(y,(x,v))]
    arr = foldl' (\m (y,(x, v)) -> M.insert (x,y) v m) a collapsed
    in S arr g 