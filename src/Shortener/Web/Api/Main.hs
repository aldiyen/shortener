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
import qualified Data.Text as T
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           GHC.Int (Int64)
import           Snap
    ( Handler, Snaplet, SnapletInit, Method(GET, POST), serveSnaplet
    , defaultConfig, makeSnaplet, nestSnaplet, rqPathInfo, addRoutes
    , getRequest, modifyResponse, with, method, setResponseStatus, setResponseCode
    )
import           Snap.Core (writeText, writeLBS, readRequestBody, rqRemoteAddr)
import           Snap.Http.Server (defaultConfig)
--import           Snap.Util.FileServe

-- Not sure what all I need here but Ross uess these har har har
import           Data.Aeson ((.:), (.=), Value(Object), Object, FromJSON(parseJSON), ToJSON(toJSON), decode, encode, object)

-- Local imports
import           Shortener.Converter (decodeR, encodeR)
import           Shortener.DataAccess (DataAccess, UserLogin(UserLogin), UrlInfo(UrlInfo), InsertUrlResult(NewUrlPk, ExistingUrlPk, InsertUrlError), daInit, insertUrl, loadUrl, loadUrlInfo, logRedirect)

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
    toJSON (AddUrlResponse msg code encodedIdentifier) = object ["message" .= msg, "code" .= code, "identifier" .= encodedIdentifier]

instance FromJSON AddUrlRequest where
    parseJSON (Object o) = AddUrlRequest <$> o .: "url"
    parseJSON _ = mzero

-- Mapping of paths to their handler functions
routes :: [(B.ByteString, Handler App App ())]
routes = [ ("/urlInfo", method GET urlInfoHandler)
         , ("/addUrl",  method POST addUrlHandler)
         ]

-- FIXME: make this return proper content type
appInit :: SnapletInit App App
appInit = makeSnaplet "ald.li" "ald.li URL shortener API" Nothing $ do
    d <- nestSnaplet "da" da $ daInit
    addRoutes routes
    return $ App d

maxAllowedRequestBytes :: Int64
maxAllowedRequestBytes = 1000 -- ~1KB

-- Handler for a request for information on a given URL
urlInfoHandler :: Handler App App ()
urlInfoHandler = do
    request <- getRequest
    case (intFromEncodedString $ rqPathInfo request) of
        Nothing -> invalidUrl
        Just v  -> (with da $ loadUrlInfo v) >>= (forwardTo request)
    where intFromEncodedString        = decodeR . B.unpack -- TODO: make decode and encode funcs use Text instead of string?
          forwardTo request (Just urlInfo) = writeLBS $ encode (UrlInfoResponse "Success" "success" (Just urlInfo))
          forwardTo _       _              = notFound
          notFound   = (modifyResponse (setResponseCode 404)) >> (writeLBS $ encode (UrlInfoResponse "URL not found" "not_found" Nothing))
          invalidUrl = (modifyResponse (setResponseCode 400)) >> (writeLBS $ encode (UrlInfoResponse "Invalid URL" "invalid_url" Nothing))

-- handle invalid JSON input more gracefully
-- TODO: (optionally -- config parameter?) use ipHeaderFilter to extract X-Forwarded-For
-- TODO: add authentication
-- TODO: At least consider the possibility that encodeR will return something bad
addUrlHandler :: Handler App App ()
addUrlHandler = do
    requestBody <- readRequestBody maxAllowedRequestBytes
    clientIp    <- fmap rqRemoteAddr getRequest
    case (decode requestBody :: Maybe AddUrlRequest) of
         -- this next line is totally wrong. if the insert somehow does something weird it can return success with a Nothing
         Just (AddUrlRequest url) -> (with da $ insertUrl clientIp 1 url) >>= handleInsertResult
         Nothing -> writeText "WHAT?!?!"
    where handleInsertResult (NewUrlPk pk)             = writeLBS $ encode (AddUrlResponse "Success" "success" (pkAsEncodedText pk))
          handleInsertResult (ExistingUrlPk pk)        = writeLBS $ encode (AddUrlResponse "Duplicate" "duplicate" (pkAsEncodedText pk))
          handleInsertResult (InsertUrlError errorMsg) = writeLBS $ encode (AddUrlResponse "Failed" "failed" Nothing)
          pkAsEncodedText = (fmap T.pack) . encodeR

main :: IO ()
main = serveSnaplet defaultConfig appInit

