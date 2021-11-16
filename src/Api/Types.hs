{-# LANGUAGE DuplicateRecordFields #-}

module Api.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data UnlistImageResponse = UnlistImageResponse
    { message :: Text
    }
    deriving (Generic, ToJSON)

data UploadImageResponse = UploadImageResponse
    { sha256hash :: Text
    }
    deriving (Generic, ToJSON)

data ListImagesResponse = ListImagesResponse
    { images :: [ListImage]
    }
    deriving (Generic, ToJSON)

data ListImage = ListImage
    { title :: Text
    , path :: Text
    , sha256hash :: Text
    }
    deriving (Generic)
    deriving anyclass (ToJSON)

data LookupArtistResponse = LookupArtistResponse
    { name :: Text
    }
    deriving (Generic)
    deriving anyclass (ToJSON)

data ListArtist = ListArtist
    { name :: Text
    , pubKeyHash :: Text
    }
    deriving (Generic)
    deriving anyclass (ToJSON)

data ListArtistsResponse = ListArtistsResponse
    { artists :: [ListArtist]
    }
    deriving (Generic)
    deriving anyclass (ToJSON)

data CreateArtistRequest = CreateArtistRequest
    { name :: Text
    , pubKeyHash :: Text
    }
    deriving (Generic)
    deriving anyclass (FromJSON)

data CreateArtistResponse = CreateArtistResponse
    { name :: Text
    , pubKeyHash :: Text
    }
    deriving (Generic)
    deriving anyclass (ToJSON)

data CreatePurchaseRequest = CreatePurchaseRequest
    { imageHash :: Text
    , authorPubKeyHash :: Text
    , ownerPubKeyHash :: Text
    , price :: Text
    , wasAuctioned :: Bool
    -- , createdAt :: Text
    }
    deriving (Generic)
    deriving anyclass (FromJSON)

data CreatePurchaseResponse = CreatePurchaseResponse
    { imageHash :: Text
    , authorPubKeyHash :: Text
    , ownerPubKeyHash :: Text
    , price :: Text
    , wasAuctioned :: Bool
    , createdAt :: UTCTime
    }
    deriving (Generic)
    deriving anyclass (ToJSON)

data GetPurchase = GetPurchase
    { imageHash :: Text
    , authorPubKeyHash :: Text
    , ownerPubKeyHash :: Text
    , price :: Text
    , wasAuctioned :: Bool
    , createdAt :: UTCTime
    }
    deriving (Generic)
    deriving anyclass (ToJSON)

data GetPurchaseResponse = GetPurchaseResponse
    { purchases :: [GetPurchase]
    }
    deriving (Generic)
    deriving anyclass (ToJSON)
