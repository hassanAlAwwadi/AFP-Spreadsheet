{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}

module API where

import Web.Scotty
import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON, FromJSON )
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Spreadsheet.Input as I 
import qualified Formula.Parser as P
import Formula ( Formula(Raw) )
import qualified Spreadsheet as SS
import qualified Control.Monad.State.Strict as S
import Network.Wai.Middleware.Cors
import Data.List
import Network.HTTP.Types.Status
import Data.IORef
import Data.String(fromString)

type Port = Int

data Input = Input 
    {
        max_x :: Int,
        max_y :: Int,
        cell :: CellData
    } deriving (Eq, Show, Generic)

instance ToJSON Input
instance FromJSON Input 

data CellData = CellData
    {
        pos_x :: Int,
        pos_y :: Int,
        content :: String
    } deriving (Eq, Show, Generic)

instance ToJSON CellData
instance FromJSON CellData    

-- whole table sent for now
-- check if formulas are creating cycles
-- if yes, send error
-- otherwise, calculate result in topological order
-- send that as response


changeInCell :: Port -> IORef SS.Spreadsheet -> IO ()
changeInCell pNo ref = scotty pNo $ do
    middleware $ cors $ const $ Just corsRP
    options "/raw" $ do
        status ok200
    post "/raw" $ do -- currently using literal, do we need captures?
        spreadsheet <- liftIO $ readIORef ref
        mutCellData :: Input <- jsonData
        let input =  parse_date mutCellData
        case input of 
          Nothing -> text $ fromString "malformed input."
          Just i  -> case SS.cycleCheck i spreadsheet of
            Nothing -> text $ fromString "cycle detected."
            Just ss -> do 
              let (ss'@(SS.S _ _ _), diff) = SS.handle i ss
              liftIO $ putStrLn $ "Changes: " ++ show diff 
              liftIO $ writeIORef ref ss'
              json diff
    notFound $ text "SOMETHING DIDN'T WORK. ROUTE NOT MATCHED"


parse_date :: Input -> Maybe I.Input 
parse_date (Input _ _ (CellData x y str)) = I.Cell (x,y) <$> P.run_parser str

corsRP :: CorsResourcePolicy
corsRP = simpleCorsResourcePolicy
  { corsOrigins = Nothing
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  , corsExposedHeaders = Nothing
  }