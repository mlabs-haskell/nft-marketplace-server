module App (
    App (..),
) where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)

import Env (Env)

newtype App a = App {unApp :: ReaderT Env IO a}
    deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadThrow)

-- instance Ipfs.MonadRemoteIPFS App where
--   runRemote query = do
--     url <- parseBaseUrl "localhost:5001"
--     manager <- liftIO $ HttpClient.newManager HttpClient.defaultManagerSettings
--     let clientEnv = mkClientEnv manager url
--     liftIO $ runClientM query clientEnv
