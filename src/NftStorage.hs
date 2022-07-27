module NftStorage (
    nftStorageAdd,
    CID (..),
) where

import App (App)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Text (pack, unpack)
import Data.Typeable (Typeable)
import Ipfs (CID (..), encodeBase32InBase36)
import Network.HTTP.Media ((//))
import Servant (Accept, Header, JSON, MimeRender, Post, Proxy (..), ReqBody, contentType, mimeRender, (:>))
import Servant.Client (ClientEnv, client, runClientM)

data PNG deriving stock (Typeable)

instance Accept PNG where
    contentType _ = "image" // "png"

instance MimeRender PNG BS.ByteString where
    mimeRender _ = BSL.fromStrict

data JPEG deriving stock (Typeable)

instance Accept JPEG where
    contentType _ = "image" // "jpeg"

instance MimeRender JPEG BS.ByteString where
    mimeRender _ = BSL.fromStrict

type NftStorageApi =
    "upload"
        :> ReqBody '[PNG, JPEG] BS.ByteString
        :> Header "Authorization" String
        :> Post '[JSON] Value

nftStorageApi :: Proxy NftStorageApi
nftStorageApi = Proxy

nftStorageAdd :: ClientEnv -> String -> BS.ByteString -> App (Either String CID)
nftStorageAdd nftStorageClientEnv apiKey fileContents = do
    result <- liftIO $ runClientM query nftStorageClientEnv
    let result' = do
            body <- case result of
                Left e -> Left ("Error making an nft.storage client request: " <> show e)
                Right (Object body) -> pure body
                _ -> Left "Error making an nft.storage client request: wrong response format"
            val <- case KeyMap.lookup "value" body of
                Just (Object val) -> pure val
                _ -> Left "Error making an ipfs client request: wrong response format"
            case KeyMap.lookup "cid" val of
                Just (String cid) -> pure cid
                _ -> Left "Error making an ipfs client request: wrong response format"
    case result' of
        Left e -> do
            liftIO $ putStrLn e
            pure $ Left e
        Right v -> case encodeBase32InBase36 (unpack v) of
            Right x -> pure $ Right $ CID (pack x)
            Left err -> pure $ Left $ "error while enconding CID: " ++ err
  where
    nftStorageClient = client nftStorageApi
    query = nftStorageClient fileContents (pure ("Bearer " <> apiKey))
