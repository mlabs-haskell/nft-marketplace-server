module Api (marketplaceApi, Routes (..), ImageApi (..), PurchaseApi (..), AdminApi (..), ArtistApi (..), ImagePaginationHeaders) where

import Data.Text (Text)
import Servant (
    AuthProtect,
    Capture,
    Delete,
    Get,
    GetPartialContent,
    Header,
    Headers,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    Summary,
    (:>),
 )
import Servant.API.Generic (Generic, ToServantApi, genericApi, (:-))
import Servant.Multipart (MultipartData, MultipartForm, Tmp)
import Servant.Pagination
import Servant.Server.Experimental.Auth (AuthServerData)

import Api.Types

type ImagePaginationHeaders =
    Header "Total-Count" Int
        ': PageHeaders '["createdAt"] ListImage

data ImageApi route = ImageApi
    { uploadImage ::
        route
            :- Summary "Create a new image returning its id."
            :> MultipartForm Tmp (MultipartData Tmp)
            :> Post '[JSON] UploadImageResponse
    , listImages ::
        route
            :- Summary "Get all images"
            :> Header "Range" (Ranges '["createdAt"] ListImage)
            :> GetPartialContent '[JSON] (Headers ImagePaginationHeaders [ListImage])
    }
    deriving stock (Generic)

data ArtistApi route = ArtistApi
    { lookupArtist ::
        route
            :- Summary "Look up artist name by pubKeyHash"
            :> Capture "hash" Text
            :> Get '[JSON] LookupArtistResponse
    , listArtists ::
        route
            :- Summary "Get all artists"
            :> Get '[JSON] ListArtistsResponse
    }
    deriving stock (Generic)

newtype PurchaseApi route = PurchaseApi
    { getPurchase ::
        route
            :- Summary "Get purchases by image hash"
            :> Capture "hash" Text
            :> Get '[JSON] GetPurchaseResponse
    }
    deriving stock (Generic)

data AdminApi route = AdminApi
    { unlistImage ::
        route
            :- "unlist_image"
            :> Summary "Remove image from the marketplace."
            :> Capture "hash" Text
            :> Post '[JSON] UnlistImageResponse
    , createArtist ::
        route
            :- "create_artist"
            :> Summary "Create a new artist"
            :> ReqBody '[JSON] CreateArtistRequest
            :> Post '[JSON] CreateArtistResponse
    , deleteArtist ::
        route
            :- "delete_artist"
            :> Summary "Delete an existing artist by pubKeyHash"
            :> Capture "hash" Text
            :> Delete '[JSON] DeleteArtistResponse
    , createPurchase ::
        route
            :- "create_purchase"
            :> Summary "Create a new purchase"
            :> ReqBody '[JSON] CreatePurchaseRequest
            :> Post '[JSON] CreatePurchaseResponse
    }
    deriving stock (Generic)

data Routes route = Routes
    { image :: route :- "images" :> ToServantApi ImageApi
    , artist :: route :- "artists" :> ToServantApi ArtistApi
    , purchase :: route :- "purchases" :> ToServantApi PurchaseApi
    , admin :: route :- "admin" :> AuthProtect "header-auth" :> ToServantApi AdminApi
    }
    deriving stock (Generic)

type instance AuthServerData (AuthProtect "header-auth") = ()

marketplaceApi :: Proxy (ToServantApi Routes)
marketplaceApi = genericApi (Proxy :: Proxy Routes)
