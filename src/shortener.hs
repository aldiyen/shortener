{-# LANGUAGE OverloadedStrings #-}

module Shortener (
    Shortener,
    shortenerInit,
    linkHandler
) where

import Snap
import Snap.Snaplet.PostgresqlSimple
import Data.Maybe

data Shortener = Shortener
    {
        _db :: Snaplet Postgres
    }

shortenerInit :: (Snaplet Postgres) -> SnapletInit Shortener Shortener
shortenerInit db = makeSnaplet "shortener" "A URL shortener" Nothing $ do
    addRoutes [
                ("/:linkShortcut", linkHandler)
              ]
    return $ Shortener db


