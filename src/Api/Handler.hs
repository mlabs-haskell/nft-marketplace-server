module Api.Handler (handlers) where

import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Database.Esqueleto.Experimental
import Database.Esqueleto.Pagination qualified as DbPagination
import Database.Persist.Postgresql qualified as P
import Servant (Headers, Proxy (..), addHeader, err422, err500)
import Servant.API.Generic (ToServant)
import Servant.Multipart (MultipartData, Tmp, fdFileName, fdPayload, files, inputs, lookupInput)
import Servant.Pagination (Range (..), RangeOrder (..), Ranges, extractRange, getDefaultRange, returnRange)
import Servant.Server.Generic (AsServerT, genericServerT)

import Api (
    AdminApi (..),
    ArtistApi (..),
    ArtistPaginationHeaders,
    ImageApi (..),
    ImagePaginationHeaders,
    PurchaseApi (..),
    Routes (..),
 )
import Api.Error (JsonError (..), throwJsonError)
import App (App)
import Env (Env (..), NftDbEnv (..))

import Api.Types
import Ipfs qualified
import NftStorage qualified
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

        liftIO $ print imgTmpPath
        liftIO $ print imgFilename

        imgData <- liftIO . BS.readFile $ imgTmpPath

        let imgHash = SHA256.hash imgData
        let imgHashHex = Base16.encodeBase16 imgHash
        liftIO $ Text.putStrLn imgHashHex

        let throwTitle = throwJsonError err422 (JsonError "Missing title")
        let throwDesc = throwJsonError err422 (JsonError "Missing description")

        let getInput inputName onError =
                either (const onError) pure $ lookupInput inputName multipartData

        imageTitle <- getInput "title" throwTitle
        imageDesc <- getInput "description" throwDesc

        Env{..} <- ask

        imageAlreadyExists <- liftIO $
            runDB envDbConnPool $ do
                selectOne $ do
                    image' <- from $ table @Image
                    where_ (image' ^. ImageSha256hash ==. val imgHashHex)

        when (isJust imageAlreadyExists) $
            throwJsonError err422 (JsonError "Image already exists")

        let imgPath = envImageStoreFolder <> "/" <> imgHashHex <> "_" <> imgFilename
        liftIO $ BS.writeFile (Text.unpack imgPath) imgData

        let saveNftInDb = case envNftDb of
                IpfsNftDbEnv env' -> Ipfs.ipfsAdd env'
                NftStorageNftDbEnv env' key -> NftStorage.nftStorageAdd env' key

        Ipfs.CID ipfsHash <- saveNftInDb imgData >>= \case
            Right h -> pure h
            Left e -> throwJsonError err500 (JsonError $ "Error adding image to DB: " <> Text.pack e)

        currentTime <- liftIO getCurrentTime
        liftIO $
            runDB envDbConnPool $ do
                a <- insert $ Image imageTitle imageDesc imgPath imgHashHex ipfsHash currentTime
                liftIO $ print a
        pure $ UploadImageResponse imgHashHex

    listImages :: Maybe (Ranges '["createdAt"] ListImage) -> App (Headers ImagePaginationHeaders [ListImage])
    listImages mrange = do
        Env{..} <- ask

        let listImageDefaultRange :: Range "createdAt" UTCTime
            listImageDefaultRange = getDefaultRange (Proxy @ListImage)

        let range =
                fromMaybe listImageDefaultRange (mrange >>= extractRange)

        (imageCountValue :: [Single Int]) <-
            liftIO $
                runDB envDbConnPool $
                    rawSql "SELECT reltuples FROM pg_class WHERE relname = 'image'" []
        let (imageCount :: Int) = unSingle $ head imageCountValue

        let query = DbPagination.emptyQuery
        let (paginationOrder, desiredRange) = case rangeOrder range of
                RangeDesc -> (DbPagination.Descend, DbPagination.Range Nothing (rangeValue range))
                RangeAsc -> (DbPagination.Ascend, DbPagination.Range (rangeValue range) Nothing)
        let pageSize = DbPagination.PageSize $ rangeLimit range

        mpage <-
            liftIO $
                runDB envDbConnPool $
                    DbPagination.getPage query ImageCreatedAt pageSize paginationOrder desiredRange

        let dbImages = maybe [] DbPagination.pageRecords mpage

        let toApiImage dbImg =
                let Image title description path hash ipfsHash createdAt = entityVal dbImg
                    imgId = fromSqlKey $ entityKey dbImg
                 in ListImage imgId title description path hash ipfsHash createdAt

        let images = map toApiImage dbImages
        addHeader imageCount <$> returnRange range images

    -- artist handlers
    artist :: ToServant ArtistApi (AsServerT App)
    artist = genericServerT ArtistApi{..}

    lookupArtist :: Text -> App LookupArtistResponse
    lookupArtist pubKeyHash = do
        Env{..} <- ask
        martist <- liftIO $
            runDB envDbConnPool $ do
                selectOne $ do
                    artist' <- from $ table @Artist
                    where_ (artist' ^. ArtistPubKeyHash ==. val pubKeyHash)
                    pure artist'

        Artist artistName _ _ <-
            maybe
                (throwJsonError err422 (JsonError "No artist with such pubKeyHash"))
                (pure . entityVal)
                martist
        pure (LookupArtistResponse artistName)

    listArtists :: Maybe (Ranges '["createdAt"] ListArtist) -> App (Headers ArtistPaginationHeaders [ListArtist])
    listArtists mrange = do
        Env{..} <- ask

        let listArtistDefaultRange :: Range "createdAt" UTCTime
            listArtistDefaultRange = getDefaultRange (Proxy @ListArtist)

        let range =
                fromMaybe listArtistDefaultRange (mrange >>= extractRange)

        (artistCountValue :: [Single Int]) <-
            liftIO $
                runDB envDbConnPool $
                    rawSql "SELECT reltuples FROM pg_class WHERE relname = 'artist'" []
        let (artistCount :: Int) = unSingle $ head artistCountValue

        let query = DbPagination.emptyQuery
        let (paginationOrder, desiredRange) = case rangeOrder range of
                RangeDesc -> (DbPagination.Descend, DbPagination.Range Nothing (rangeValue range))
                RangeAsc -> (DbPagination.Ascend, DbPagination.Range (rangeValue range) Nothing)
        let pageSize = DbPagination.PageSize $ rangeLimit range

        mpage <-
            liftIO $
                runDB envDbConnPool $
                    DbPagination.getPage query ArtistCreatedAt pageSize paginationOrder desiredRange

        let dbArtists = maybe [] DbPagination.pageRecords mpage

        let toApiArtist dbArtist =
                let Artist name pubKeyHash createdAt = entityVal dbArtist
                    artistId = fromSqlKey $ entityKey dbArtist
                 in ListArtist artistId name pubKeyHash createdAt

        let artists = map toApiArtist dbArtists
        addHeader artistCount <$> returnRange range artists

    -- purchase handlers
    purchase :: ToServant PurchaseApi (AsServerT App)
    purchase = genericServerT PurchaseApi{..}

    getPurchase :: Text -> App GetPurchaseResponse
    getPurchase imageHash = do
        Env{..} <- ask

        dbPurchases <- liftIO $
            runDB envDbConnPool $ do
                select $ do
                    purchase' <- from $ table @Purchase
                    where_ (purchase' ^. PurchaseImageHash ==. val imageHash)
                    pure purchase'

        let toApiPurchase dbPurchase =
                let Purchase imgHash authorPkh ownerPkh price wasAuctioned createdAt = entityVal dbPurchase
                    purchaseId = fromSqlKey $ entityKey dbPurchase
                 in GetPurchase purchaseId imgHash authorPkh ownerPkh price wasAuctioned createdAt

        let purchases = map toApiPurchase dbPurchases
        pure $ GetPurchaseResponse purchases

    -- admin handlers
    admin :: () -> ToServant AdminApi (AsServerT App)
    admin () = genericServerT AdminApi{..}

    unlistImage :: Text -> App UnlistImageResponse
    unlistImage imageHash = do
        Env{..} <- ask

        imageExists <- liftIO $
            runDB envDbConnPool $ do
                selectOne $ do
                    image' <- from $ table @Image
                    where_ (image' ^. ImageSha256hash ==. val imageHash)

        unless (isJust imageExists) $
            throwJsonError err422 (JsonError "Image does not exists")

        liftIO $
            runDB envDbConnPool $ do
                numDeleted <- deleteCount $ do
                    images <- from $ table @Image
                    where_ (images ^. ImageSha256hash ==. val imageHash)
                liftIO $ print numDeleted
        pure (UnlistImageResponse "successfully removed image")

    createArtist :: CreateArtistRequest -> App CreateArtistResponse
    createArtist (CreateArtistRequest name pubKeyHash) = do
        Env{..} <- ask

        artistExists <- liftIO $
            runDB envDbConnPool $ do
                selectOne $ do
                    artist' <- from $ table @Artist
                    where_
                        ( artist' ^. ArtistPubKeyHash ==. val pubKeyHash
                            ||. artist' ^. ArtistName ==. val name
                        )
        when (isJust artistExists) $
            throwJsonError err422 (JsonError "Artist already exists")

        currentTime <- liftIO getCurrentTime

        liftIO $
            runDB envDbConnPool $ do
                a <- insert $ Artist name pubKeyHash currentTime
                liftIO $ print a

        pure $ CreateArtistResponse name pubKeyHash

    deleteArtist :: Text -> App DeleteArtistResponse
    deleteArtist pubKeyHash = do
        Env{..} <- ask

        artistExists <- liftIO $
            runDB envDbConnPool $ do
                selectOne $ do
                    artist' <- from $ table @Artist
                    where_ (artist' ^. ArtistPubKeyHash ==. val pubKeyHash)

        unless (isJust artistExists) $
            throwJsonError err422 (JsonError "Artist does not exists")

        liftIO $
            runDB envDbConnPool $ do
                numDeleted <- deleteCount $ do
                    artists <- from $ table @Artist
                    where_ (artists ^. ArtistPubKeyHash ==. val pubKeyHash)
                liftIO $ print numDeleted
        pure (DeleteArtistResponse "successfully removed artist")

    createPurchase :: CreatePurchaseRequest -> App CreatePurchaseResponse
    createPurchase (CreatePurchaseRequest imageHash authorPkh ownerPkh price wasAuctioned) = do
        Env{..} <- ask

        -- TODO: get from a request?
        currentTime <- liftIO getCurrentTime
        liftIO $
            runDB envDbConnPool $ do
                p <- insert $ Purchase imageHash authorPkh ownerPkh price wasAuctioned currentTime
                liftIO $ print p

        pure $ CreatePurchaseResponse imageHash authorPkh ownerPkh price wasAuctioned currentTime

    -- health check
    healthz = return "OK"
