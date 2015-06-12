{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Shortener.DataAccess (
    daInit,
    loadUrl,
    loadUrlInfo,
    logRedirect,
    insertUrl,
    DataAccess,
    UserLogin(UserLogin),
    UrlInfo(UrlInfo),
    InsertUrlResult(NewUrlPk, ExistingUrlPk, InsertUrlError)
) where

import           Control.Applicative ((<*>), (<$>))
import           Control.Lens.TH (makeLenses)
import           Control.Monad (liftM2)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (Maybe(Just, Nothing), listToMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           GHC.Int (Int64)
import           Snap (Handler, Snaplet, SnapletInit, get, makeSnaplet, nestSnaplet, with)
import           Snap.Snaplet.PostgresqlSimple (Only(Only), FromRow, HasPostgres, Postgres, Query(Query), field, fromRow, getPostgresState, execute, pgsInit, query)
import qualified Data.ByteString as B

data DataAccess = DataAccess
    { _db   :: Snaplet Postgres
    }

makeLenses ''DataAccess

data UserLogin = UserLogin
    { userLoginUserPk :: Int64
    , userLoginUsername :: Text
    } deriving Show

data UrlInfo = UrlInfo
    { urlInfoUrlPk  :: Int64
    , urlInfoUrl :: Text
    , urlInfoCreator :: UserLogin
    , urlInfoCreatedDatetime :: UTCTime
    , urlInfoCreationIp :: Text
    , urlInfoLastAccessDatetime :: Maybe UTCTime
    } deriving Show

data InsertUrlResult = NewUrlPk Int64 | ExistingUrlPk Int64 | InsertUrlError Text

instance FromRow UrlInfo where
    fromRow = UrlInfo <$> field <*> field <*> (liftM2 UserLogin field field) <*> field <*> field <*> field

daInit :: SnapletInit b DataAccess
daInit = makeSnaplet "ald.li data access" "ald.li URL shortener data access module" Nothing $ do
    d <- nestSnaplet "db" db $ pgsInit
    return $ DataAccess d

-- So we don't have to call with db all the time
instance HasPostgres (Handler b DataAccess) where
    getPostgresState = with db get

-- Helper function to load a URL from the DB
loadUrl :: (HasPostgres m, Functor m) => Int64 -> m (Maybe B.ByteString)
loadUrl x = fmap (listToMaybe . concat) $ query "SELECT url FROM url WHERE url_pk = ?" [x]

loadUrlInfo :: (HasPostgres m, Functor m) => Int64 -> m (Maybe UrlInfo)
loadUrlInfo x = fmap listToMaybe $ query urlInfoByPkQuery [x]
    where urlInfoByPkQuery = "SELECT url_pk, url, user_login_pk, username, datetime, ip_address, ua.datetime FROM load_url_info(?)" :: Query

-- Helper function to insert a row into url_access, recording who we sent where
logRedirect :: (HasPostgres m) => B.ByteString -> Int64 -> m Int64
logRedirect clientIp urlPk = do
    currentTime <- liftIO getCurrentTime
    execute "INSERT INTO url_access (ip_address, datetime, url_pk) VALUES (?, ?, ?)" (clientIp, currentTime, urlPk)

-- Inserts a new URL
insertUrl :: (HasPostgres m, Functor m) => B.ByteString -> Int64 -> Text -> m InsertUrlResult
insertUrl clientIp userPk url = do
    currentTime <- liftIO getCurrentTime
    maybeResut <- fmap (listToMaybe) $ query "SELECT was_created, url_pk FROM add_url(?, ?, ?, ?)" $ (url, currentTime, clientIp, userPk)
    case maybeResut of
        Just (True, pk)  -> return (NewUrlPk pk)
        Just (False, pk) -> return (ExistingUrlPk pk)
        Nothing -> return $ InsertUrlError "Got no result from input query?"

