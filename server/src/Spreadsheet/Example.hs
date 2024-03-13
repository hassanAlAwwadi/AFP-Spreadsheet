module Spreadsheet.Example where 
import Util 
import Spreadsheet
import Spreadsheet.Input 
import Spreadsheet.Unit 
import qualified Data.Map as M 

example :: Spreadsheet
example = let 
  s   = S M.empty M.empty M.empty
  s'  = handle (Cell (0,0) (Raw 1)) s
  s'' = handle (Cell (1,1) (Plus (direct 0 0) (direct 0 0))) s'
  in s''
