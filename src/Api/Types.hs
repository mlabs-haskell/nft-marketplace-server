module Api.Types where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Text (Text)

data UnlistImageResponse = UnlistImageResponse
  { message :: Text
  } deriving (Generic, ToJSON)
