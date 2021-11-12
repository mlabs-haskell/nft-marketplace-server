module App (
    App (..),
    Env (..),
) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.SqlBackend (SqlBackend)
import GHC.Generics (Generic)

newtype App a = App {unApp :: ReaderT Env IO a}
    deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadThrow)

type PgConnectionPool = Pool SqlBackend

data Env = Env
    { dbConnPool :: PgConnectionPool
    , imageStoreFolder :: Text
    }
    deriving stock (Generic)
