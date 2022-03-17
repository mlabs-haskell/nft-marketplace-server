module Env (
    Env (..),
    NftDbEnv (..),
) where

import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.SqlBackend (SqlBackend)
import GHC.Generics (Generic)
import Servant.Client (ClientEnv)

type PgConnectionPool = Pool SqlBackend

data NftDbEnv = IpfsNftDbEnv ClientEnv | NftStorageNftDbEnv ClientEnv String

data Env = Env
    { envDbConnPool :: PgConnectionPool
    , envImageStoreFolder :: Text
    , envNftDb :: NftDbEnv
    }
    deriving stock (Generic)
