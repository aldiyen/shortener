{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

-- Library imports
import           Control.Lens.TH (makeLenses)
import           Control.Applicative ((<$>))
import           Control.Monad (mzero)
import qualified Data.ByteString.Char8 as B
import           Data.Maybe (Maybe(Just, Nothing))
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import           Snap
    ( Handler, Snaplet, SnapletInit, Method(GET, POST), serveSnaplet
    , defaultConfig, makeSnaplet, nestSnaplet, rqPathInfo, addRoutes
    , getRequest, modifyResponse, with, method, setResponseStatus, setResponseCode
    )
import           Snap.Core (writeText, writeLBS)
import           Snap.Http.Server (defaultConfig)
--import           Snap.Util.FileServe

-- Not sure what all I need here but Ross uess these har har har
import           Data.Aeson ((.:), (.=), Value(Object), Object, FromJSON(parseJSON), ToJSON(toJSON), encode, object)

-- Local imports
import           Shortener.Converter (decodeR, encodeR)
import           Shortener.DataAccess (DataAccess, UserLogin(UserLogin), UrlInfo(UrlInfo), daInit, loadUrl, loadUrlInfo, logRedirect)

data App = App
    { _da   :: Snaplet DataAccess
    }

makeLenses ''App

--
-- TODO move these API classes to their own module
data UrlInfoResponse = UrlInfoResponse
    { urlInfoResponseMessage :: Text
    , urlInfoResponseCode    :: Text
    , urlInfoResponseUrlInfo :: Maybe UrlInfo
    } deriving Show

data AddUrlResponse = AddUrlResponse
    { addUrlResponseMessage :: Text
    , addUrlResponseCode    :: Text
    , addUrlResponseNewId   :: Maybe Text
    } deriving Show

data AddUrlRequest = AddUrlRequest
    { addUrlRequestUrl :: Text
    } deriving Show

{- TBD: permissions? how do they work? need to figure out how to load user data from the DB and use it for auth'ing
        and also
data AddUserRequest = AddUserRequest
    {
        username    :: String,
        password    :: String,
        permissions :: ???
    }
-}

-- DataAccess doesn't know how to turn these into JSON (and shouldn't), so define it here
instance ToJSON UrlInfo where
    toJSON (UrlInfo pk url creator created ip lastAccess) = object ["urlPk" .= pk, "url" .= url, "creator" .= creator, "createdDatetime" .= created, "creatorIp" .= ip, "lastAccessDatetime" .= lastAccess]

instance ToJSON UserLogin where
    toJSON (UserLogin pk username) = object ["userPk" .= pk, "username" .= username]

instance ToJSON UrlInfoResponse where
    toJSON (UrlInfoResponse msg code url) = object ["message" .= msg, "code" .= code, "urlInfo" .= url]

instance ToJSON AddUrlResponse where
    toJSON (AddUrlResponse msg code newId) = object ["message" .= msg, "code" .= code, "newId" .= newId]

instance FromJSON AddUrlRequest where
    parseJSON (Object o) = AddUrlRequest <$> o .: "url"
    parseJSON _ = mzero

routes :: [(B.ByteString, Handler App App ())]
routes = [ ("/urlInfo", method GET urlInfoHandler)
         , ("/addUrl",  method POST addUrlHandler)
         ]

appInit :: SnapletInit App App
appInit = makeSnaplet "ald.li" "ald.li URL shortener API" Nothing $ do
    d <- nestSnaplet "da" da $ daInit
    addRoutes routes
    return $ App d

-- TODO: Make this return metadata about the URL (creator IP/username, created datetime)
-- and also make it return JSON on failure
urlInfoHandler :: Handler App App ()
urlInfoHandler = do
    request <- getRequest
    case (decode $ rqPathInfo request) of
        Nothing -> modifyResponse $ setResponseStatus 400 (B.append (B.pack "Invalid URI: ") (rqPathInfo request))
        Just v  -> (with da $ loadUrlInfo v) >>= (forwardTo request)
    where decode        = decodeR . B.unpack
          forwardTo request (Just urlInfo) = writeLBS $ encode (UrlInfoResponse "Success" "success" (Just urlInfo))
          forwardTo _       _              = notFound $ encode (UrlInfoResponse "URL not found" "not_found" Nothing)
          notFound responseBody = (modifyResponse (setResponseCode 404)) >> (writeLBS body)
-- TODO: add authentication
addUrlHandler :: Handler App App ()
addUrlHandler = do
    writeText "NO!"

main :: IO ()
main = serveSnaplet defaultConfig appInit
