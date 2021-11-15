{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Schema where

import Prelude
import Database.Persist
import Database.Persist.TH
import Data.Text (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Image
    title Text
    path Text
    sha256hash Text
    UniqueSha256hash sha256hash
    deriving Show
Artist
    name Text
    pubKeyHash Text
    UniquePubKeyHash pubKeyHash
    UniqueName name
    deriving Show
|]
