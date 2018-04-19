{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Clay as C
import Control.Monad.Trans (liftIO)
import Data.Aeson (ToJSON)
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Database.SQLite.Simple as SQL
import Database.SQLite.Simple.FromRow (FromRow, fromRow, field)
import GHC.Generics (Generic)
import Lucid
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Web.Scotty (middleware, scotty, get, json, html, param, ActionM)

data Tracker = Tracker 
  { site :: T.Text
  , page :: T.Text
  , hits :: Int
  } deriving (Show, Generic)

instance FromRow Tracker where
  fromRow = Tracker <$> field <*> field <*> field

instance ToJSON Tracker 

tableCss = C.div C.# C.byClass "tableCss" C.? do
  C.border           C.solid (C.px 1) C.black
  
dbName :: String
dbName = "mytracker.db"

selectAll :: IO [Tracker]
selectAll = do
  conn <- SQL.open dbName
  let sql = "SELECT * FROM tracker"
  res <- SQL.query_ conn sql :: IO [Tracker]
  return res


mkpage :: Lucid.Html () -> Lucid.Html () -> L.Text
mkpage titleStr page = renderText $ do
  doctype_
  html_ $ do
    header_ $ do
      style_ $ L.toStrict $ C.render tableCss
      title_ titleStr
    body_ $ do
      table_ page
      a_ [href_ "/json"] "get json"

homeRoute :: [Tracker] -> Lucid.Html ()
homeRoute trackers = do
  h1_ "mytracker"
  div_[class_ "tableCss"] $ mapM_ (div_[class_ "tableCss"] . toHtml . formatTracker) trackers
  where formatTracker tracker =  T.concat [ site tracker, " | ",  page tracker, " | ", getHitsText tracker]
  
getHitsText :: Tracker -> T.Text
getHitsText = T.pack . show . hits

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  middleware simpleCors

  get "/" $ do
    allData <- liftIO selectAll
    html $ mkpage "mytracker" $ homeRoute allData

  get "/json" $ do
    allData <- liftIO selectAll
    json (allData)
    

