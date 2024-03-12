module Spreadsheet.Unit where 

data Formula 
  = Raw Int 
  | Reference Ref Ref
  | Plus Formula Formula
  -- | Reference Int Int -- Will need a more complex constructor for this, I fear.

data Ref 
  = Loc Int 
  | Rel Int 