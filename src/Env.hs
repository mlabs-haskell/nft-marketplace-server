module Env (
    Env (..),
    NftDbEnv (..),
) where

import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.SqlBackend (SqlBackend)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Servant.Client (ClientEnv)

type PgConnectionPool = Pool SqlBackend

data NftDbEnv = IpfsNftDbEnv ClientEnv | NftStorageNftDbEnv ClientEnv String

data Env = Env
    { envDbConnPool :: PgConnectionPool
    , envImageStoreFolder :: Text
    , envNftDb :: NftDbEnv
    , envMaxImgSizeMb :: Int64
    }
    deriving stock (Generic)
