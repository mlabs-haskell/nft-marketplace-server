module Api.Handler where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (getCurrentTime)
import Database.Esqueleto.Experimental
import Database.Persist.Postgresql qualified as P
import Servant (err422)
import Servant.API.Generic (ToServant)
import Servant.Multipart
import Servant.Server.Generic (AsServerT, genericServerT)

import Api
import Api.Error
import Api.Types
import App
import Schema

handlers :: Routes (AsServerT App)
handlers = Routes{..}
  where
    -- image handlers
    image :: ToServant ImageApi (AsServerT App)
    image = genericServerT ImageApi{..}

    runDB = flip P.runSqlPersistMPool

    uploadImage :: MultipartData Tmp -> App UploadImageResponse
    uploadImage multipartData = do
        when (null $ files multipartData) $
            throwJsonError err422 (JsonError "No files uploaded")
        when (null $ inputs multipartData) $
            throwJsonError err422 (JsonError "Missing inputs")

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

        -- let imageTitle = either ... pure lookupInput "title" multipartData
        let imageTitle = iValue $ head $ inputs multipartData

        Env{..} <- ask

        imageAlreadyExists <- liftIO $
            runDB dbConnPool $ do
                selectOne $ do
                    image <- from $ table @Image
                    where_ (image ^. ImageSha256hash ==. (val imgHashHex))

        when (isJust imageAlreadyExists) $
            throwJsonError err422 (JsonError "Image already exists")

        let imgPath = imageStoreFolder <> "/" <> imgHashHex <> "_" <> imgFilename
        liftIO $ BS.writeFile (Text.unpack imgPath) imgData

        liftIO $
            runDB dbConnPool $ do
                a <- insert $ Image imageTitle imgPath imgHashHex
                liftIO $ print a
        pure $ UploadImageResponse imgHashHex

    -- TODO: pagination
    listImages :: App ListImagesResponse
    listImages = do
        Env{..} <- ask

        dbImages <- liftIO $
            runDB dbConnPool $ do
                select $ do
                    image <- from $ table @Image
                    pure image

        let toApiImage (Image title path hash) = ListImage title path hash
        let images = map (toApiImage . entityVal) dbImages
        pure $ ListImagesResponse images

    -- artist handlers
    artist :: ToServant ArtistApi (AsServerT App)
    artist = genericServerT ArtistApi{..}

    lookupArtist :: Text -> App LookupArtistResponse
    lookupArtist pubKeyHash = do
        Env{..} <- ask
        martist <- liftIO $
            runDB dbConnPool $ do
                selectOne $ do
                    artist <- from $ table @Artist
                    where_ (artist ^. ArtistPubKeyHash ==. (val pubKeyHash))
                    pure artist

        Artist artistName _ <-
            maybe
                (throwJsonError err422 (JsonError "No artist with such pubKeyHash"))
                (pure . entityVal)
                martist
        pure (LookupArtistResponse artistName)

    listArtists :: App ListArtistsResponse
    listArtists = do
        Env{..} <- ask
        dbArtists <- liftIO $
            runDB dbConnPool $ do
                select $ do
                    artist <- from $ table @Artist
                    pure artist

        let toApiArtist (Artist name pubKeyHash) = ListArtist name pubKeyHash
        let artists = map (toApiArtist . entityVal) dbArtists
        pure $ ListArtistsResponse artists

    -- purchase handlers
    purchase :: ToServant PurchaseApi (AsServerT App)
    purchase = genericServerT PurchaseApi{..}

    getPurchase :: Text -> App GetPurchaseResponse
    getPurchase imageHash = do
        Env{..} <- ask

        dbPurchases <- liftIO $
            runDB dbConnPool $ do
                select $ do
                    purchase <- from $ table @Purchase
                    where_ (purchase ^. PurchaseImageHash ==. (val imageHash))
                    pure purchase

        let toApiPurchases (Purchase imageHash authorPkh ownerPkh price wasAuctioned createdAt) = GetPurchase imageHash authorPkh ownerPkh price wasAuctioned createdAt
        let purchases = map (toApiPurchases . entityVal) dbPurchases
        pure $ GetPurchaseResponse purchases

    -- admin handlers
    admin :: () -> ToServant AdminApi (AsServerT App)
    admin () = genericServerT AdminApi{..}

    unlistImage :: Text -> App UnlistImageResponse
    unlistImage imageHash = do
        Env{..} <- ask

        imageExists <- liftIO $
            runDB dbConnPool $ do
                selectOne $ do
                    image <- from $ table @Image
                    where_ (image ^. ImageSha256hash ==. (val imageHash))

        unless (isJust imageExists) $
            throwJsonError err422 (JsonError "Image does not exists")

        -- TODO: transaction
        liftIO $
            runDB dbConnPool $ do
                numDeleted <- deleteCount $ do
                    images <- from $ table @Image
                    where_ (images ^. ImageSha256hash ==. val imageHash)
                liftIO $ print numDeleted
        pure (UnlistImageResponse "successfully removed image")

    createArtist :: CreateArtistRequest -> App CreateArtistResponse
    createArtist (CreateArtistRequest name pubKeyHash) = do
        Env{..} <- ask

        artistExists <- liftIO $
            runDB dbConnPool $ do
                selectOne $ do
                    artist <- from $ table @Artist
                    where_
                        ( artist ^. ArtistPubKeyHash ==. (val pubKeyHash)
                            ||. artist ^. ArtistName ==. (val name)
                        )
        when (isJust artistExists) $
            throwJsonError err422 (JsonError "Artist already exists")

        liftIO $
            runDB dbConnPool $ do
                a <- insert $ Artist name pubKeyHash
                liftIO $ print a

        pure $ CreateArtistResponse name pubKeyHash

    createPurchase :: CreatePurchaseRequest -> App CreatePurchaseResponse
    createPurchase (CreatePurchaseRequest imageHash authorPkh ownerPkh price wasAuctioned) = do
        Env{..} <- ask

        -- TODO: get from a request?
        currentTime <- liftIO $ getCurrentTime
        liftIO $
            runDB dbConnPool $ do
                p <- insert $ Purchase imageHash authorPkh ownerPkh price wasAuctioned currentTime
                liftIO $ print p

        pure $ CreatePurchaseResponse imageHash authorPkh ownerPkh price wasAuctioned currentTime
