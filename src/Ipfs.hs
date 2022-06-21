module Ipfs (
    ipfsAdd,
    CID (..),
) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Network.IPFS.API (ApiV0Add)
import Servant (Proxy (..))
import Servant.Client (ClientEnv, client, runClientM)

import Data.HashMap.Strict qualified as HM

import App (App)

ipfsAddApi :: Proxy ApiV0Add
ipfsAddApi = Proxy

newtype CID = CID {unCID :: Text}

ipfsAdd :: ClientEnv -> ByteString -> App (Either String CID)
ipfsAdd envIpfsClientEnv fileContents = do
    result <- liftIO $ runClientM query envIpfsClientEnv
    case result of
        Left e -> do
            let msg = "Error making an ipfs client request: " <> show e
            liftIO $ putStrLn msg
            pure $ Left msg
        Right (Object obj) | Just (String hash) <- obj HM.!? ("Hash" :: Text)
            -> pure . Right $ CID hash
        Right json -> do
          let msg = "Error making an ipfs client request: wrong response format: " <> show json
          liftIO $ putStrLn msg
          pure $ Left msg
  where
    ipfsClientAdd = client ipfsAddApi

    query =
        ipfsClientAdd
            (BSL.fromStrict fileContents)
            Nothing -- recursive
            Nothing -- quiet
            Nothing -- quieter
            Nothing -- silent
            Nothing -- progress
            Nothing -- trickle
            Nothing -- only-hash
            Nothing -- wrap-with-directory
            Nothing -- hidden
            Nothing -- chunker
            (Just True) -- pin
            Nothing -- raw-leaves
            Nothing -- nocopy
            Nothing -- fscache
            (Just 1) -- cid-version
            Nothing -- hash
