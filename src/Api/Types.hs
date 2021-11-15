{-# LANGUAGE DuplicateRecordFields      #-}
module Api.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

data UnlistImageResponse = UnlistImageResponse
  { message :: Text
  } deriving (Generic, ToJSON)

data UploadImageResponse = UploadImageResponse
  { sha256hash :: Text
  } deriving (Generic, ToJSON)

data ListImagesResponse = ListImagesResponse
  { images :: [ListImage]
  } deriving (Generic, ToJSON)

data ListImage = ListImage
  { title :: Text
  , path :: Text
  , sha256hash :: Text
  } deriving Generic
    deriving anyclass ToJSON

data LookupArtistResponse = LookupArtistResponse
  { name :: Text
  } deriving Generic
    deriving anyclass ToJSON

data ListArtist = ListArtist
  { name :: Text
  , pubKeyHash :: Text
  } deriving Generic
    deriving anyclass ToJSON

data ListArtistsResponse = ListArtistsResponse
  { artists :: [ListArtist]
  } deriving Generic
    deriving anyclass ToJSON

data CreateArtistRequest = CreateArtistRequest
  { name :: Text
  , pubKeyHash :: Text
  } deriving Generic
    deriving anyclass FromJSON

data CreateArtistResponse = CreateArtistResponse
  { name :: Text
  , pubKeyHash :: Text
  } deriving Generic
    deriving anyclass ToJSON
