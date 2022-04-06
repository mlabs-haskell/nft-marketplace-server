module Main (main) where

-- import Control.Monad.Logger (runStderrLoggingT)

-- import Database.Persist.Sql.Migration (addMigration)

import Api (Routes, marketplaceApi)
import Api.Auth (authHandler)
import Api.Handler (handlers)
import App (App (..))
import Control.Monad.Catch (try)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as Text
import Database.Persist.Postgresql (
    runMigration,
    runSqlPersistMPool,
    withPostgresqlPool,
 )
import Env (Env (..), NftDbEnv (..))
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Request)
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Middleware.Cors (
    cors,
    corsExposedHeaders,
    corsRequestHeaders,
    simpleCorsResourcePolicy,
    simpleHeaders,
    simpleResponseHeaders,
 )
import Network.Wai.Parse (defaultParseRequestBodyOptions, setMaxRequestFileSize)
import Options (NftDbOptions (..), Options (..))
import Options qualified
import Schema (migrateAll)
import Servant (Application, Context (..), Handler (..), Proxy (..), ServerT, hoistServerWithContext, serveWithContext)
import Servant.API.Generic (ToServantApi)
import Servant.Client (mkClientEnv, parseBaseUrl)
import Servant.Multipart (Tmp, defaultMultipartOptions, generalOptions)
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Generic (genericServerT)
import System.Directory (createDirectoryIfMissing)

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

    clientEnv <- case nftDb of
        NftDbIpfsAddress ipfsNodeAddress -> do
            manager <- HttpClient.newManager HttpClient.defaultManagerSettings
            ipfsNodeUrl <- parseBaseUrl ipfsNodeAddress
            pure $ IpfsNftDbEnv $ mkClientEnv manager ipfsNodeUrl
        NftDbNftStorageKey apiKey -> do
            manager <- HttpClient.newManager tlsManagerSettings
            nftStorageUrl <- parseBaseUrl "https://api.nft.storage"
            pure $ NftStorageNftDbEnv (mkClientEnv manager nftStorageUrl) apiKey

    runNoLoggingT $
        withPostgresqlPool connStr 10 $ \pool -> do
            liftIO $ -- run migrations
                flip runSqlPersistMPool pool $ do
                    runMigration $ do
                        migrateAll
            -- addMigration True "CREATE INDEX CONCURRENTLY IF NOT EXISTS image_created_at_index ON image (created_at)"
            -- addMigration True "CREATE INDEX CONCURRENTLY IF NOT EXISTS artist_created_at_index ON artist (created_at)"

            liftIO $ putStrLn $ "Starting server on port " <> show serverPort
            liftIO $ -- start server
                withStdoutLogger $ \logger -> do
                    let env = Env pool imageFolderText clientEnv
                        warpSettings = W.setPort serverPort $ W.setLogger logger W.defaultSettings
                        customCorsPolicy =
                            simpleCorsResourcePolicy
                                { corsRequestHeaders =
                                    [ "Authorization"
                                    , "Range"
                                    ]
                                        <> simpleHeaders
                                , corsExposedHeaders =
                                    Just
                                        [ "Total-Count"
                                        , "Accept-Ranges"
                                        , "Content-Range"
                                        , "Next-Range"
                                        ]
                                        <> simpleResponseHeaders
                                }
                        customCors = cors (const $ Just customCorsPolicy)
                    W.runSettings warpSettings $ customCors (appService env)
