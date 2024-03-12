module Spreadsheet.Unit where 

data Formula 
  = Raw Int 
  | Reference Ref Ref
  | Plus Formula Formula
  -- | Reference Int Int -- Will need a more complex constructor for this, I fear.
  deriving Show

data Ref 
  = Loc Int 
  | Rel Int
  deriving Show 

locate :: Int -> Ref -> Int 
locate _ (Loc i)           = i 
locate (lodestone) (Rel i) = lodestone + i
