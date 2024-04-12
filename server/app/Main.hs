module Main where 
import API (changeInCell)
import Spreadsheet(empty)
import Data.IORef

main :: IO ()
main = do 
  let spreadsheet = empty 
  sVar <- newIORef spreadsheet
  changeInCell 31415 sVar