{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}

module API where

import Web.Scotty
import GHC.Generics ( Generic )
import Data.Aeson ( ToJSON, FromJSON )
import Data.Maybe (fromMaybe)
import Spreadsheet.Input (Input(..))
import Formula ( Formula(Raw) )
import qualified Spreadsheet as SS
import qualified Control.Monad.State.Strict as S
import Network.Wai.Middleware.Cors
import Data.List
import Network.HTTP.Types.Status

type Port = Int

data Table = Table 
    {
        max_x :: Int,
        max_y :: Int,
        cells :: [[CellData]]
    } deriving (Eq, Show, Generic)

instance ToJSON Table
instance FromJSON Table 

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

changeInCell :: Port -> IO ()
changeInCell pNo = scotty pNo $ do
    middleware $ cors $ const $ Just corsRP
    options "/raw" $ do
        status ok200
    post "/raw" $ do -- currently using literal, do we need captures?
        mutCellData :: Table <- jsonData
        liftIO $ print $ "These are our changes:\n" <> show mutCellData
        let retCD = mutCellData -- fromMaybe defaultModel validateChange
        json retCD
    notFound $ text "SOMETHING DIDN'T WORK. ROUTE NOT MATCHED"

validateChange :: Maybe Table
validateChange = undefined
-- should probably be an Either

defaultModel :: Table
defaultModel = undefined
-- model to be sent back if nothing changes?
-- or maybe no response at all

-- for now let's assume that the change in the elm model 
-- only happens with Raw Formulas
-- also, we could think about clever diffing later
-- for now let's just send the entire spreadsheet as payload

type Coordinates = (Int, Int)
type RawData     = String
newtype RawTable = RawTable [(Coordinates, RawData)] deriving (Eq, Show, Generic)

rt2Input :: RawTable -> [Input]
rt2Input (RawTable rcs) = map (\(c, d) -> Cell c (Raw (read d :: Int))) rcs
-- using read is iffy

handleChanges :: [Input] -> S.State SS.Spreadsheet SS.Spreadsheet
handleChanges is = do
    currSS <- S.get
    let afterSS = foldl' (flip SS.handle) currSS is
    S.put afterSS
    return afterSS
-- execState (handleChanges is) (S M.empty M.empty M.empty)

-- TODO: Need to decide on Models for all 3 places (Front, Middle, Back)

corsRP :: CorsResourcePolicy
corsRP = simpleCorsResourcePolicy
            { corsOrigins = Nothing
            , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
            , corsRequestHeaders = ["Authorization", "Content-Type"]
            , corsExposedHeaders = Nothing
            }