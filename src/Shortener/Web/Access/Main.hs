{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- Library imports
import           Control.Lens.TH
import           Snap
import qualified Data.ByteString.Char8 as B

-- Local imports
import           Shortener.Converter
import           Shortener.DataAccess

data App = App
    { _da   :: Snaplet DataAccess
    }

-- Magic!
makeLenses ''App

-- Not a lot of routes here, since we only really forward people
routes :: [(B.ByteString, Handler App App ())]
routes = [ ("/", indexHandler),
           ("/", linkHandler)
         ]

appInit :: SnapletInit App App
appInit = makeSnaplet "ald.li" "ald.li URL shortener" Nothing $ do
    d <- nestSnaplet "da" da $ daInit
    addRoutes routes
    return $ App d

-- Should probably replace this with some HTML page, eh?
indexHandler :: Handler App App ()
indexHandler = ifTop $ writeText "This is a URL shortener. Why are you here?"

-- The main request handling logic:
-- 1. Treat the URI path as the encoded url ID
-- 2. If the URI path can be decoded, load the corresponding url_id from DB
-- 3. If the url_id existed, log that the user visited it back to the DB, then forward them on
-- FIXME should probably have proper 400 and 404 handlers, i.e. display real web pages
-- TODO: track user agent and maybe add a session cookie as well
linkHandler :: Handler App App ()
linkHandler = do
    request <- getRequest
    case (decode $ rqPathInfo request) of
        Nothing -> modifyResponse $ setResponseStatus 400 "Invalid URI"
        Just v  -> (with da $ loadUrl v) >>= (forwardTo request v)
    where decode        = decodeR . B.unpack
          forwardTo request urlPk (Just url) = (with da $ logRedirect (rqRemoteAddr request) urlPk) >> redirect url
          forwardTo _       _      _         = modifyResponse $ setResponseCode 404

-- You know, a main function...
main :: IO ()
main = serveSnaplet defaultConfig appInit

