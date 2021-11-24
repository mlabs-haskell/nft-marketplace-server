{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Schema where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

share
    [mkPersist sqlSettings, mkMigrate "migrateAll"]
    [persistLowerCase|
Image
    title Text
    path Text
    sha256hash Text
    createdAt UTCTime default=now()
    UniqueSha256hash sha256hash
    deriving Show
Artist
    name Text
    pubKeyHash Text
    createdAt UTCTime default=now()
    UniquePubKeyHash pubKeyHash
    UniqueName name
    deriving Show
Purchase
    imageHash Text
    authorPubKeyHash Text
    ownerPubKeyHash Text
    price Text
    wasAuctioned Bool
    createdAt UTCTime
    deriving Show
AdminToken
    token Text
    UniqueToken token
    deriving Show
|]
