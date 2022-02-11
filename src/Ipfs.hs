module Ipfs (
    ipfsAdd,
    CID (..),
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Aeson (Value (..))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.Text (Text)
import Network.IPFS.API (ApiV0Add)
import Servant (Proxy (..))
import Servant.Client (client, runClientM)

import Data.HashMap.Strict qualified as HM

import App (App)
import Env (Env (..))

ipfsAddApi :: Proxy ApiV0Add
ipfsAddApi = Proxy

newtype CID = CID {unCID :: Text}

ipfsAdd :: ByteString -> App (Maybe CID)
ipfsAdd fileContents = do
    Env{..} <- ask
    result <- liftIO $ runClientM query envIpfsClientEnv
    case result of
        Left _ -> do
            liftIO $ putStrLn "Error making an ipfs client request"
            pure Nothing
        Right (Object obj) ->
            case obj HM.!? ("Hash" :: Text) of
                Just (String hash) ->
                    pure . Just $ CID hash
                _ -> do
                    liftIO $ putStrLn "Error making an ipfs client request: wrong response format"
                    pure Nothing
        Right _ -> do
            liftIO $ putStrLn "Error making an ipfs client request: wrong response format"
            pure Nothing
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
