module Api.Handler where

import Prelude

import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString.Base16 qualified as Base16
import Control.Monad.IO.Class (liftIO)
import Servant.Multipart
import Control.Monad.Reader (ask)
import qualified Database.Persist.Postgresql as P
import Database.Esqueleto.Experimental
import qualified Data.ByteString as BS
import Servant (err422)
import Control.Monad (when, unless)
import Data.Maybe (isJust)

import Api
import App
import Schema
import Api.Error
import Api.Types

handlers :: Routes (AsServerT App)
handlers = Routes {..}
  where
    -- image handlers
    image :: ToServant ImageApi (AsServerT App)
    image = genericServerT ImageApi {..}

    uploadImage :: MultipartData Tmp -> App Text
    uploadImage multipartData = do
      liftIO . print . length $ files multipartData
      let img = head $ files multipartData
      let imgTmpPath = fdPayload img
      let imgFilename = fdFileName img

      liftIO $ print $ imgTmpPath
      liftIO $ print $ imgFilename

      imgData <- liftIO . BS.readFile $ imgTmpPath

      let imgHash = SHA256.hash imgData
      let imgHashHex = Base16.encodeBase16 imgHash
      liftIO $ Text.putStrLn imgHashHex

      liftIO . print . length $ inputs multipartData
      -- let imageTitle = either ... pure lookupInput "title" multipartData
      let imageTitle = iValue $ head $ inputs multipartData

      Env{..} <- ask

      imageAlreadyExists <- liftIO $ flip P.runSqlPersistMPool dbConnPool $ do
        selectOne $ do
          image <- from $ table @Image
          where_ (image ^. ImageSha256hash ==. (val imgHashHex))

      when (isJust imageAlreadyExists) $
        throwJsonError err422 (JsonError "Image already exists")

      let imgPath = imageStoreFolder <> "/" <> imgHashHex <> "_" <> imgFilename
      liftIO $ BS.writeFile (Text.unpack imgPath) imgData

      liftIO $ flip P.runSqlPersistMPool dbConnPool $ do
        a <- insert $ Image imageTitle imgPath imgHashHex
        liftIO $ print a
      pure imgHashHex

    -- admin handlers
    admin :: ToServant AdminApi (AsServerT App)
    admin = genericServerT AdminApi {..}

    unlistImage :: Text -> App UnlistImageResponse
    unlistImage imageHash = do
      Env{..} <- ask

      imageExists <- liftIO $ flip P.runSqlPersistMPool dbConnPool $ do
        selectOne $ do
          image <- from $ table @Image
          where_ (image ^. ImageSha256hash ==. (val imageHash))

      unless (isJust imageExists) $
        throwJsonError err422 (JsonError "Image does not exists")

      -- TODO: transaction
      liftIO $ flip P.runSqlPersistMPool dbConnPool $ do
        numDeleted <- deleteCount $ do
          images <- from $ table @Image
          where_ (images ^. ImageSha256hash ==. val imageHash)
        liftIO $ print numDeleted
      pure (UnlistImageResponse "successfully removed image")
