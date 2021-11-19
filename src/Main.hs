module Main (main) where

import Control.Monad.Catch (try)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (runReaderT)
import Data.ByteString.Char8 qualified as BSC
import Data.Text qualified as Text
import Database.Persist.Postgresql (createPostgresqlPool, runMigration, runSqlPersistMPool, withPostgresqlPool)
import Network.Wai (Request)
import Network.Wai.Handler.Warp qualified as W
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Parse (defaultParseRequestBodyOptions, setMaxRequestFileSize)
import Servant (Application, Context (..), Handler (..), Proxy (..), ServerT, hoistServerWithContext, serveWithContext)
import Servant.API.Generic (ToServantApi)
import Servant.Multipart (Tmp, defaultMultipartOptions, generalOptions)
import Servant.Server.Experimental.Auth (AuthHandler)
import Servant.Server.Generic (genericServerT)

import Api (Routes, marketplaceApi)
import Api.Auth (authHandler)
import Api.Handler (handlers)
import App (App (..), Env (..))
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

    -- let connStr = "host=localhost dbname=marketplacedb user=aske port=5432"

    runStderrLoggingT $
        withPostgresqlPool connStr 1 $ \pool ->
            liftIO $
                flip runSqlPersistMPool pool $ do
                    runMigration migrateAll

    connPool <- runStderrLoggingT $ createPostgresqlPool connStr 10

    let env = Env connPool imageFolderText

    withStdoutLogger $ \logger -> do
        let warpSettings = W.setPort serverPort $ W.setLogger logger W.defaultSettings
        W.runSettings warpSettings (appService env)

{-

1. new person comeins in and uploads, upload and create artwork
   index by hash

2. authentican comes from wallet

3. admin panel offchain to be able to unlist nft

4. limit to jpg and png only
   limit to the same resolution
   516x516
   limit to 2 megabytes

5. purchase history (start with last purchase)

6.

Artwork
  id
  title
  content
  contentHash / nftId ?
  authorId
  ownerId
  createdAt
  updatedAt
  imageId
  -- thumbnailId
  auctionStateId

AuctionState
  id
  highestBid
  highestBidderId
  deadline
  minimumBid

Purchase ?
  id
  artworkId

Image
  id
  path
  resolution
  thumbnailId

Thumbnail
  id
  path
  resolution

User --TODO: how to authenticate -- TODO: roles?
  id
  name
  pubKeyHash
  createdAt
  updatedAt

PubKeyHash
  id
  hash

GET /artwork -- paginated
GET /artwork/<id>
POST /artwork/<id>/unlist
POST /artwork/create

GET /image/<id>
POST /image/upload

???
POST /artwork/<id>/set-price
POST /artwork/<id>/buy
POST /artwork/<id>/auction-open
POST /artwork/<id>/auction-close
POST /artwork/<id>/auction-bid

GET /nft/<nftId>

GET /nft/<nftId>/query-current-owner
GET /nft/<nftId>/query-current-price
GET /nft/query-all

POST /nft/<nftId>/unlist
or? POST /artwork/<id>/unlist ?

-}
