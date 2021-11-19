module Api.Auth (authHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (decodeUtf8')
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
    throwUnauthorized = throwError err401

    handler req = do
        let Env{..} = env
        authHeader <- maybe throwUnauthorized pure . lookup "Authorization" $ requestHeaders req
        -- TODO: change to `decodeUft8'`
        authHeaderText <- either (const throwUnauthorized) pure $ decodeUtf8' authHeader
        adminToken <- liftIO $
            flip P.runSqlPersistMPool dbConnPool $ do
                selectOne $ do
                    token <- from $ table @AdminToken
                    where_ (token ^. AdminTokenToken ==. val authHeaderText)
        case adminToken of
            Nothing -> throwUnauthorized
            Just _ -> pure ()
