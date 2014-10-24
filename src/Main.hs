{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Lens.TH
import           Data.Time
import           Data.Maybe
import           Snap
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.ByteString.Char8 as B

import Converter

data App = App
    {
        _db   :: Snaplet Postgres
    }

makeLenses ''App

--instance HasPostgres (Handler App App) where
--    getPostgresState = with db get

routes :: [(B.ByteString, Handler App App ())]
routes = [ ("/", indexHandler),
           ("/", linkHandler)
         ]

appInit :: SnapletInit App App
appInit = makeSnaplet "ald.li" "ald.li URL shortener" Nothing $ do
    d <- nestSnaplet "db" db $ pgsInit
    addRoutes routes
    return $ App d

indexHandler :: Handler App App ()
indexHandler = ifTop $ writeText "Sure..."

-- Function to load everything
linkHandler :: Handler App App ()
linkHandler = do
    request <- getRequest
    case (decode $ rqPathInfo request) of
        Nothing -> modifyResponse $ setResponseStatus 400 "Invalid URI"
        Just v  -> load v >>= parse
    where decode        = decodeR . B.unpack
          load x        = with db $ query "SELECT url, url_id FROM url WHERE url_id = ?" [x]
          -- HEY WTF HOW CAN THIS WORK??? ALSO HOW CAN I GET THE DATE FROM IO -- I CAN USE getCurrentTime or getUtcTime or something like that from Data.Time.Clock, but it's IO UTCTime and I want a damn UTCTime
          parse :: MonadSnap m => [(B.ByteString, Int)] -> m ()
          parse [(url, id)] = getRequest >>= (\req -> (logRedirect req id) >> redirect url)
          parse _       = modifyResponse $ setResponseCode 404
          logRedirect request id = with db $ execute "INSERT INTO url_access (url_id, datetime, ip_address) VALUES (?, ?, ?)" [id, "2014-01-01", "test"]
--          parse [[url]] = do logError ("Resolved to url: " `B.append` url)
--                             redirect url
--          parse x       = do logError ("Resolved to x: " `B.append` (B.pack $ show x))
--                             modifyResponse $ setResponseCode 404

main :: IO ()
main = serveSnaplet defaultConfig appInit
