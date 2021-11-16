{-# LANGUAGE DuplicateRecordFields #-}

module Api.Types (
    UnlistImageResponse (..),
    UploadImageResponse (..),
    ListImagesResponse (..),
    ListImage (..),
    LookupArtistResponse (..),
    ListArtist (..),
    ListArtistsResponse (..),
    CreateArtistRequest (..),
    CreateArtistResponse (..),
    CreatePurchaseRequest (..),
    CreatePurchaseResponse (..),
    GetPurchase (..),
    GetPurchaseResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data UnlistImageResponse = UnlistImageResponse
    { message :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data UploadImageResponse = UploadImageResponse
    { sha256hash :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data ListImagesResponse = ListImagesResponse
    { images :: [ListImage]
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data ListImage = ListImage
    { title :: Text
    , path :: Text
    , sha256hash :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data LookupArtistResponse = LookupArtistResponse
    { name :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data ListArtist = ListArtist
    { name :: Text
    , pubKeyHash :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data ListArtistsResponse = ListArtistsResponse
    { artists :: [ListArtist]
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data CreateArtistRequest = CreateArtistRequest
    { name :: Text
    , pubKeyHash :: Text
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

data CreateArtistResponse = CreateArtistResponse
    { name :: Text
    , pubKeyHash :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data CreatePurchaseRequest = CreatePurchaseRequest
    { imageHash :: Text
    , authorPubKeyHash :: Text
    , ownerPubKeyHash :: Text
    , price :: Text
    , wasAuctioned :: Bool
    -- , createdAt :: Text
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

data CreatePurchaseResponse = CreatePurchaseResponse
    { imageHash :: Text
    , authorPubKeyHash :: Text
    , ownerPubKeyHash :: Text
    , price :: Text
    , wasAuctioned :: Bool
    , createdAt :: UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data GetPurchase = GetPurchase
    { imageHash :: Text
    , authorPubKeyHash :: Text
    , ownerPubKeyHash :: Text
    , price :: Text
    , wasAuctioned :: Bool
    , createdAt :: UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data GetPurchaseResponse = GetPurchaseResponse
    { purchases :: [GetPurchase]
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)
