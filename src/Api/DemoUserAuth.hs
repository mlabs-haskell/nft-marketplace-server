module Api.DemoUserAuth (demoUserAuthHandler) where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (decodeUtf8')
import Data.Text.Encoding (decodeUtf8)
import Network.Wai (Request, requestHeaders)
import Servant (err401, throwError, BasicAuthCheck (..), basicAuthUsername, basicAuthPassword)
import Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)
import Servant.Server (BasicAuthResult (..))

import Database.Esqueleto.Experimental
import Database.Persist.Postgresql qualified as P

import App (Env (..))
import Api.Types (DemoUserData (..))
import Schema

demoUserAuthHandler :: Env -> BasicAuthCheck DemoUserData
demoUserAuthHandler env = BasicAuthCheck $ \basicAuthData -> do
  let username = decodeUtf8 $ basicAuthUsername basicAuthData
  let password = decodeUtf8 $ basicAuthPassword basicAuthData
  handler username password
  where
    handler username password = do
      let Env{..} = env

      mdemoUser <- flip P.runSqlPersistMPool dbConnPool $
              selectOne $ do
                  user <- from $ table @DemoUser
                  where_ (user ^. DemoUserToken ==. val password
                         &&. user ^. DemoUserName ==. val username)
                  pure user
      case mdemoUser of
        Nothing ->
          pure Unauthorized
        Just dbDemoUser ->
          pure $ Authorized demoUser
          where
            DemoUser name token = entityVal dbDemoUser
            demoUser = DemoUserData name token
