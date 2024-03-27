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
import Data.List (foldl')

type Port = Int

data TableModel = TableModel [[String]] deriving (Eq, Show, Generic)
-- needs to be actual model
-- might need a topologically sorted graph
-- or just a model representing the change between cells (not entire graph)

instance ToJSON TableModel
instance FromJSON TableModel

changeInCell :: Port -> IO ()
changeInCell pNo = scotty pNo $ do
    post "mutCell" $ do -- currently using literal, do we need captures?
        mutCellData :: TableModel <- jsonData
        liftIO $ print $ "These are our changes:\n" <> show mutCellData
        let retCD = fromMaybe defaultModel validateChange
        json retCD
    notFound $ text "SOMETHING DIDN'T WORK. ROUTE NOT MATCHED"

validateChange :: Maybe TableModel
validateChange = undefined
-- should probably be an Either

defaultModel :: TableModel
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