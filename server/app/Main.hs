{-# LANGUAGE OverloadedStrings #-}
module Main where
import API 
import Network.Wai.Handler.Warp
import qualified Spreadsheet as S()

main :: IO ()
main = run 8081 app1