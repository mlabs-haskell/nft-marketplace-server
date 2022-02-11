module Main (main) where

import Control.Monad.Catch (try)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (liftIO)

-- import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as Text
import Database.Persist.Postgresql (runMigration, runSqlPersistMPool, withPostgresqlPool)

-- import Database.Persist.Sql.Migration (addMigration)

import Network.HTTP.Client qualified as HttpClient
import Network.Wai (Request)
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Parse (defaultParseRequestBodyOptions, setMaxRequestFileSize)
import Servant (Application, Context (..), Handler (..), Proxy (..), ServerT, hoistServerWithContext, serveWithContext)
import Servant.API.Generic (ToServantApi)
import Servant.Client (mkClientEnv, parseBaseUrl)
import Servant.Multipart (Tmp, defaultMultipartOptions, generalOptions)
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Generic (genericServerT)
import System.Directory (createDirectoryIfMissing)

import Api (Routes, marketplaceApi)
import Api.Auth (authHandler)
import Api.Handler (handlers)
import App (App (..))
import Env (Env (..))
import Options (Options (..))
import Options qualified
import Schema (migrateAll)

appService :: Env -> Application
appService env = serveWithContext marketplaceApi ctx appServer
  where
    ctx = authHandler env :. multipartOpts :. EmptyContext

    multipartOpts =
        (defaultMultipartOptions (Proxy :: Proxy Tmp))
            { -- Disallow files > 2MiB
              generalOptions = setMaxRequestFileSize (2 * 1024 * 1024) defaultParseRequestBodyOptions
            }

    hoistApp :: App a -> Handler a
    hoistApp = Handler . ExceptT . try . flip runReaderT env . unApp

    appServerT :: ServerT (ToServantApi Routes) App
    appServerT = genericServerT handlers

    appServer :: ServerT (ToServantApi Routes) Handler
    appServer = hoistServerWithContext marketplaceApi hoistCtx hoistApp appServerT

    hoistCtx :: Proxy '[AuthHandler Request ()]
    hoistCtx = Proxy

main :: IO ()
main = do
    Options{..} <- Options.parseOptions
    let imageFolderText = Text.pack imageFolder
    let connStr = BSC.pack dbConnectionString

    createDirectoryIfMissing False imageFolder

    ipfsNodeUrl <- parseBaseUrl ipfsNodeAddress
    manager <- HttpClient.newManager HttpClient.defaultManagerSettings
    let clientEnv = mkClientEnv manager ipfsNodeUrl

    runNoLoggingT $
        withPostgresqlPool connStr 10 $ \pool ->
            liftIO $
                flip runSqlPersistMPool pool $ do
                    runMigration $ do
                        migrateAll
                    -- addMigration True "CREATE INDEX CONCURRENTLY IF NOT EXISTS image_created_at_index ON image (created_at)"
                    -- addMigration True "CREATE INDEX CONCURRENTLY IF NOT EXISTS artist_created_at_index ON artist (created_at)"
                    let env = Env pool imageFolderText clientEnv

                    liftIO $
                        withStdoutLogger $ \logger -> do
                            let warpSettings = W.setPort serverPort $ W.setLogger logger W.defaultSettings
                            W.runSettings warpSettings (appService env)
