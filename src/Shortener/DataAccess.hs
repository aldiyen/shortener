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
    UrlInfo(UrlInfo)
) where

import           Control.Lens.TH (makeLenses)
import           Control.Monad(liftM2)
import           Data.Maybe (Maybe(Just, Nothing), listToMaybe)
import           Data.Text (Text)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           GHC.Int (Int64)
import           Snap
import           Snap.Snaplet.PostgresqlSimple (Only(Only), FromRow, HasPostgres, Postgres, Query(Query), field, fromRow, getPostgresState, execute, pgsInit, query)
import qualified Data.ByteString.Char8 as B

data DataAccess = DataAccess
    { _db   :: Snaplet Postgres
    }

makeLenses ''DataAccess

data UserLogin = UserLogin
    { userLoginUserPk :: Int
    , userLoginUsername :: Text
    } deriving Show

data UrlInfo = UrlInfo
    { urlInfoUrlPk  :: Int
    , urlInfoUrl :: Text
    , urlInfoCreator :: UserLogin
    , urlInfoCreatedDatetime :: UTCTime
    , urlInfoCreationIp :: Text
    , urlInfoLastAccessDatetime :: Maybe UTCTime
    } deriving Show

instance FromRow UrlInfo where
    fromRow = UrlInfo <$> field <*> field <*> (liftM2 UserLogin field field) <*> field <*> field <*> field

urlInfoByPkQuery :: Query
urlInfoByPkQuery =
    "SELECT u.url_pk,\
\           u.url,\
\           ul.user_login_pk,\
\           ul.username,\
\           uc.datetime,\
\           uc.ip_address,\
\           MAX(ua.datetime)\
\    FROM url u\
\    JOIN url_create uc ON uc.url_pk = u.url_pk\
\    JOIN user_login ul ON ul.user_login_pk = uc.user_login_pk\
\    LEFT JOIN url_access ua ON ua.url_pk = u.url_pk\
\    WHERE u.url_pk = ?\
\    GROUP BY u.url_pk,\
\             u.url,\
\             uc.datetime,\
\             ul.user_login_pk,\
\             ul.username,\
\             uc.ip_address"

daInit :: SnapletInit b DataAccess
daInit = makeSnaplet "ald.li data access" "ald.li URL shortener data access module" Nothing $ do
    d <- nestSnaplet "db" db $ pgsInit
    return $ DataAccess d

-- TODO: Maybe try to use esquelito and PErsistent, though they are apparently made for Yesod?

-- So we don't have to call with db all the time
instance HasPostgres (Handler b DataAccess) where
    getPostgresState = with db get

-- Helper function to load a URL from the DB
loadUrl :: (HasPostgres m, Functor m) => Int -> m (Maybe B.ByteString)
loadUrl x = fmap (listToMaybe . concat) $ query "SELECT url FROM url WHERE url_pk = ?" [x]

loadUrlInfo :: (HasPostgres m, Functor m) => Int -> m (Maybe UrlInfo)
loadUrlInfo x = fmap listToMaybe $ query urlInfoByPkQuery [x]

-- Helper function to insert a row into url_access, recording who we sent where
logRedirect :: (HasPostgres m) => B.ByteString -> Int -> m Int64
logRedirect clientIp urlPk = do
    currentTime <- liftIO getCurrentTime
    execute "INSERT INTO url_access (ip_address, datetime, url_pk) VALUES (?, ?, ?)" (clientIp, currentTime, urlPk)

-- Inserts a new URL
insertUrl :: (HasPostgres m) => B.ByteString -> Int -> B.ByteString -> m Int64
insertUrl clientIp userPk url = do
    currentTime <- liftIO getCurrentTime
    urlPk <- execute "INSERT INTO url (url) VALUES (?)" $ Only url
    execute "INSERT INTO url_create (url_pk, datetime, ip_address, user_login_pk) VALUES (?, ?, ?, ?)" (urlPk, currentTime, clientIp, userPk)
    return urlPk
