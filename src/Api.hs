module Api where

import Servant (
  Capture,
  Delete,
  Get,
  JSON,
  NoContent,
  Post,
  Proxy (..),
  Put,
  PutCreated,
  ReqBody,
  Summary,
  (:>),
 )
import Servant.API.Generic (Generic, ToServantApi, genericApi, (:-))
import Servant.Multipart
import Prelude
import Data.Text (Text)

import Api.Types

data ImageApi route = ImageApi
  { uploadImage ::
      route
        :- Summary "Create a new image returning its id."
        :> MultipartForm Tmp (MultipartData Tmp)
        :> Post '[JSON] UploadImageResponse

   -- TODO: pagination
  , listImages ::
      route
        :- Summary "Get all images"
        :> Get '[JSON] ListImagesResponse
  }
  deriving Generic

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
  deriving Generic

data PurchaseApi route = PurchaseApi
  { getPurchase ::
      route
        :- Summary "Get purchases by image hash"
        :> Capture "hash" Text
        :> Get '[JSON] GetPurchaseResponse
  }
  deriving Generic

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
  , createPurchase ::
      route
        :- "create_purchase"
        :> Summary "Create a new purchase"
        :> ReqBody '[JSON] CreatePurchaseRequest
        :> Post '[JSON] CreatePurchaseResponse
  }
  deriving Generic

data Routes route = Routes
  { image :: route :- "images" :> ToServantApi ImageApi
  , artist :: route :- "artists" :> ToServantApi ArtistApi
  , purchase :: route :- "purchases" :> ToServantApi PurchaseApi
  , admin :: route :- "admin" :> ToServantApi AdminApi
  }
  deriving Generic

marketplaceApi :: Proxy (ToServantApi Routes)
marketplaceApi = genericApi (Proxy :: Proxy Routes)