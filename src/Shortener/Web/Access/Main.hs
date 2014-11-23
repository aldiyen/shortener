{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Shortener.Web.Access.Main where

-- Library imports
import           Control.Applicative
import           Control.Lens.TH
import           Data.Time.Clock
import           Data.Maybe
import           GHC.Int
import           Snap
import           Snap.Core
import           Snap.Http.Server
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.FileServe
import qualified Data.ByteString.Char8 as B

-- Local imports
import           Shortener.Converter

data App = App
    {
        _db   :: Snaplet Postgres
    }

-- Magic!
makeLenses ''App

-- Not a lot of routes here, since we only really forward people
routes :: [(B.ByteString, Handler App App ())]
routes = [ ("/", indexHandler),
           ("/", linkHandler)
         ]

-- Magic happens here
appInit :: SnapletInit App App
appInit = makeSnaplet "ald.li" "ald.li URL shortener" Nothing $ do
    d <- nestSnaplet "db" db $ pgsInit
    addRoutes routes
    return $ App d

-- So we don't have to call with db all the time
instance HasPostgres (Handler b App) where
    getPostgresState = with db get

-- Should probably replace this with some HTML page, eh?
indexHandler :: Handler App App ()
indexHandler = ifTop $ writeText "This is a URL shortener. Why are you here?"

-- The main request handling logic:
-- 1. Treat the URI path as the encoded url ID
-- 2. If the URI path can be decoded, load the corresponding url_id from DB
-- 3. If the url_id existed, log that the user visited it back to the DB, then forward them on
-- FIXME should probably have 400 and 404 handlers
linkHandler :: Handler App App ()
linkHandler = do
    request <- getRequest
    case (decode $ rqPathInfo request) of
        Nothing -> modifyResponse $ setResponseStatus 400 "Invalid URI"
        Just v  -> loadUrl v >>= (forwardTo request v)
    where decode        = decodeR . B.unpack
          forwardTo request urlId [Only url] = (logRedirect (rqRemoteAddr request) urlId) >>= (\_ -> redirect url)
          forwardTo _       _  _       = modifyResponse $ setResponseCode 404

-- Helper function to load a URL from the DB
loadUrl :: (HasPostgres m) => Int -> m [Only B.ByteString]
loadUrl x = query "SELECT url FROM url WHERE url_id = ?" [x]

-- Helper function to insert a row into url_access, recording who we sent where
logRedirect :: (HasPostgres m) => B.ByteString -> Int -> m Int64
logRedirect clientIp urlId = do
    currentTime <- liftIO getCurrentTime
    execute "INSERT INTO url_access (ip_address, datetime, url_id) VALUES (?, ?, ?)" (clientIp, currentTime, urlId)

-- You know, a main function...
main :: IO ()
main = serveSnaplet defaultConfig appInit
