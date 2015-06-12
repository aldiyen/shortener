{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Shortener.UserManager (
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

import           Control.Lens.TH (makeLenses)
import           Data.Maybe (Maybe(Just, Nothing), listToMaybe)
import           Data.Text (Text)
import           GHC.Int (Int64)
import           Snap (Handler, Snaplet, SnapletInit, makeSnaplet, nestSnaplet)
import qualified Data.ByteString as B
import           Shortener.DataAccess (DataAccess, UserLogin(UserLogin), UrlInfo(UrlInfo), InsertUrlResult(NewUrlPk, ExistingUrlPk, InsertUrlError), daInit, insertUrl, loadUrl, loadUrlInfo, logRedirect)
import           Crypto.BCrypt (HashingPolicy, hashPasswordUsingPolicy)

data DataAccess = DataAccess
    { _da   :: Snaplet DataAccess
    }

makeLenses ''DataAccess

data UserToken = UserToken Text

umInit :: SnapletInit b DataAccess
umInit = makeSnaplet "ald.li user manager" "ald.li URL shortener user manager module" Nothing $ do
    d <- nestSnaplet "da" da $ daInit
    return $ DataAccess d

encryptPassword :: Text -> IO Maybe Text
encryptPassword p = hashPasswordUsingPolicy (HashingPolicy 14) (encodeUtf8 p)

-- TODO: make this compile; figure out how to inline the processing rather than making the functions call each other
authenticateUser :: Text -> Text -> IO Either Text UserToken
authenticateUser username password = (encryptPassword password)
    where handleEncryption (Just e) = handleUser $ with da $ loadUser username e
          handleEncryption Nothing  = Left "Failed to encrypt password"
          handleUser (Just u) = Right $ with da $ generateToken u
          handleUser Nothing  = Left "Invalid username or password"

{-
Okay!

Should depend on DataAccess snaplet, I think?
Should be a snaplet itself, pretty sure (otherwise DataAccess is a pain to deal with)

Need functions for:
- Get a login token for a given user/pass combination
- Load user from given request (from login token)
- Create new user
- Update existing user
-
-}
