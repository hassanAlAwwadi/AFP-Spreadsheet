{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}
module API where

import Web.Scotty
import GHC.Generics ( Generic )
import Data.Aeson
import Data.Maybe (fromMaybe)

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
-- or maybe dont make a req at all