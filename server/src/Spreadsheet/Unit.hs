module Spreadsheet.Unit where 

data Formula 
  = Raw Int 
  -- | Reference Int Int -- Will need a more complex constructor for this, I fear.