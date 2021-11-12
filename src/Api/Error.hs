module Api.Error where

import           Control.Monad.Catch   (MonadThrow, throwM)
import GHC.Generics (Generic)
import           Servant               (ServerError, err400, err404, err500,
                                        errBody, errHeaders)
import           Data.Aeson            (ToJSON, encode)
import           Network.HTTP.Types    (hContentType)
import Data.Text (Text)

data JsonError = JsonError
  { error :: Text
  } deriving (Generic, ToJSON)

throwJsonError :: (MonadThrow m, ToJSON a) => ServerError -> a -> m b
throwJsonError err json = throwM err
  { errBody = encode json
  , errHeaders = [ jsonHeader ]
  }
  where
    jsonHeader = (hContentType, "application/json;charset=utf-8")
