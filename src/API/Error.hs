{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Error where

import           Control.Exception     (Exception,
                                        SomeException (SomeException))
import           Data.Aeson            (ToJSON (toJSON), encode)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe            (fromMaybe)
import           GHC.Generics          (Generic)
import           Models                (HealthCheck (HealthCheck))
import           Network.HTTP.Types    (HeaderName, hContentType)
import qualified Servant               as S

data Error =
  Error
    { status  :: String
    , message :: String
    }
  deriving (Show, Eq, Generic)

instance ToJSON Error

data ServiceError
  = ServiceError Error
  | HealthError HealthCheck
  | MismatchError Error
  deriving (Show, Generic)

instance Exception ServiceError

instance ToJSON ServiceError where
  toJSON (ServiceError e)  = toJSON e
  toJSON (HealthError e)   = toJSON e
  toJSON (MismatchError e) = toJSON e

headers :: [(HeaderName, BS.ByteString)]
headers = [(hContentType, BS.pack "application/json")]

healthError :: SomeException -> S.Handler a
healthError e = do
  S.throwError
    S.err500
      { S.errBody = (encode . HealthCheck "Health error" . show) e
      , S.errHeaders = headers
      }

serviceError :: SomeException -> S.Handler a
serviceError e = do
  S.throwError
    S.err500
      { S.errBody = (encode . ServiceError . Error "Service error" . show) e
      , S.errHeaders = headers
      }

mismatchError :: SomeException -> S.Handler a
mismatchError e = do
  S.throwError
    S.err500
      { S.errBody = (encode . MismatchError . Error "Mismatch error" . show) e
      , S.errHeaders = headers
      }

-- Should avoid such a hack
toEither :: Maybe (Either SomeException a) -> Either SomeException a
toEither =
  fromMaybe
    (Left . SomeException . ServiceError . Error "Service error" $ "Empty graph")
