module Env (
  Env (..),
) where

import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.SqlBackend (SqlBackend)
import GHC.Generics (Generic)
import Servant.Client (ClientEnv)

type PgConnectionPool = Pool SqlBackend

data Env = Env
    { envDbConnPool :: PgConnectionPool
    , envImageStoreFolder :: Text
    , envIpfsClientEnv :: ClientEnv
    }
    deriving stock (Generic)
