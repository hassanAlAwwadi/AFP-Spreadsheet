module Spreadsheet.Example where 
import Util 
import Spreadsheet
import Spreadsheet.Input 
import Formula 
import qualified Data.Map as M 

example :: Spreadsheet
example = let 
  s   = S M.empty M.empty M.empty
  (s' , _) = handle (Cell (0,0) (Raw 1)) s
  (s'', _) = handle (Cell (1,1) (Op (+) (direct 0 0) (direct 0 0))) s'
  in s''