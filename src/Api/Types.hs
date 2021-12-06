{-# LANGUAGE DuplicateRecordFields #-}

module Api.Types (
    UnlistImageResponse (..),
    UploadImageResponse (..),
    ListImage (..),
    LookupArtistResponse (..),
    ListArtist (..),
    CreateArtistRequest (..),
    CreateArtistResponse (..),
    CreatePurchaseRequest (..),
    CreatePurchaseResponse (..),
    GetPurchase (..),
    GetPurchaseResponse (..),
    DeleteArtistResponse (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Servant.Pagination

newtype UnlistImageResponse = UnlistImageResponse
    { message :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype UploadImageResponse = UploadImageResponse
    { sha256hash :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data ListImage = ListImage
    { id :: Int64
    , title :: Text
    , description :: Text
    , path :: Text
    , sha256hash :: Text
    , createdAt :: UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

instance HasPagination ListImage "createdAt" where
    type RangeType ListImage "createdAt" = UTCTime
    getFieldValue _ (ListImage _ _ _ _ _ createdAt) = createdAt

newtype LookupArtistResponse = LookupArtistResponse
    { name :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

data ListArtist = ListArtist
    { id :: Int64
    , name :: Text
    , pubKeyHash :: Text
    , createdAt :: UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

instance HasPagination ListArtist "createdAt" where
    type RangeType ListArtist "createdAt" = UTCTime
    getFieldValue _ (ListArtist _ _ _ createdAt) = createdAt

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
    { id :: Int64
    , imageHash :: Text
    , authorPubKeyHash :: Text
    , ownerPubKeyHash :: Text
    , price :: Text
    , wasAuctioned :: Bool
    , createdAt :: UTCTime
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype GetPurchaseResponse = GetPurchaseResponse
    { purchases :: [GetPurchase]
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)

newtype DeleteArtistResponse = DeleteArtistResponse
    { message :: Text
    }
    deriving stock (Generic)
    deriving anyclass (ToJSON)
