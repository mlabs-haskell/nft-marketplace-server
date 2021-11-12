module App
  ( App(..)
  , Env(..)
  ) where

import GHC.Generics (Generic)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Data.Pool (Pool)
import Database.Persist.SqlBackend
import Data.Text (Text)
import           Control.Monad.Catch   (MonadThrow)

newtype App a = App {unApp :: ReaderT Env IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadThrow)

type PgConnectionPool = Pool SqlBackend

data Env = Env
  { dbConnPool ::  PgConnectionPool
  , imageStoreFolder :: Text
  } deriving (Generic)
