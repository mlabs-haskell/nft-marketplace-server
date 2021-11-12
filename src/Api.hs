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
        :> Post '[JSON] Text
  }
  deriving (Generic)

data AdminApi route = AdminApi
  { unlistImage ::
      route
        :- Summary "Remove image from the marketplace."
        :> Capture "hash" Text
        :> Post '[JSON] UnlistImageResponse
  }
  deriving (Generic)

data Routes route = Routes
  { image :: route :- "image" :> ToServantApi ImageApi
  , admin :: route :- "admin" :> ToServantApi AdminApi
  }
  deriving (Generic)

marketplaceApi :: Proxy (ToServantApi Routes)
marketplaceApi = genericApi (Proxy :: Proxy Routes)
