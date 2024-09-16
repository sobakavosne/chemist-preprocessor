{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Error where

import           Control.Exception     (Exception,
                                        SomeException (SomeException))
import           Data.Aeson            (FromJSON, KeyValue ((.=)),
                                        ToJSON (toJSON), encode)
import           Data.Aeson.Key        (fromString)
import           Data.Aeson.Types      (object)
import qualified Data.ByteString.Char8 as BS
import           GHC.Generics          (Generic)
import           Models                (HealthCheck (HealthCheck),
                                        MechanismDetails)
import           Network.HTTP.Types    (HeaderName, hContentType)
import qualified Servant               as S

data Error =
  Error
    { status  :: String
    , message :: String
    }
  deriving (Show, Eq, Generic)

instance FromJSON Error

instance ToJSON Error

data ServiceError
  = ServiceError Error
  | HealthError HealthCheck
  | MismatchError Error
  deriving (Show, Generic)

instance Exception ServiceError

instance ToJSON ServiceError where
  toJSON (ServiceError Error {status, message}) =
    object [fromString "status" .= status, fromString "message" .= message]
  toJSON (HealthError e) = toJSON e
  toJSON (MismatchError e) = toJSON e

headers :: [(HeaderName, BS.ByteString)]
headers = [(hContentType, BS.pack "application/json")]

healthErr :: SomeException -> S.Handler a
healthErr e = do
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

toEither ::
     Maybe (Either SomeException MechanismDetails)
  -> Either SomeException MechanismDetails
toEither x =
  case x of
    Nothing ->
      (Left . SomeException . ServiceError . Error "Service error")
        "Empty mechanism graph"
    Just (Left e) ->
      (Left . SomeException . ServiceError . Error "Service error" . show) e
    Just value -> value
