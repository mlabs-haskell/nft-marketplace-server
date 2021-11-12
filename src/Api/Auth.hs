module Api.Auth (authHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai (Request, requestHeaders)
import Servant (err401, throwError)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)

import Database.Esqueleto.Experimental
import Database.Persist.Postgresql qualified as P

import App (Env (..))
import Schema

authHandler :: Env -> AuthHandler Request ()
authHandler env = mkAuthHandler handler
  where
    throwUnathorized = throwError err401

    handler req = do
        let Env{..} = env
        authHeader <- maybe throwUnathorized pure . lookup "Authorization" $ requestHeaders req
        -- TODO: change to `decodeUft8'`
        let authHeaderText = decodeUtf8 authHeader
        adminToken <- liftIO $
            flip P.runSqlPersistMPool dbConnPool $ do
                selectOne $ do
                    token <- from $ table @AdminToken
                    where_ (token ^. AdminTokenToken ==. val authHeaderText)
        case adminToken of
            Nothing -> throwUnathorized
            Just _ -> pure ()
