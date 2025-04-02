{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module Api.Apis (AccountApi, UserApi, LoginRequest, RegisterRequest (email), MeResponse (..)) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant.API
    ( Get,
      JSON,
      PostNoContent,
      QueryParam,
      ReqBody,
      (:<|>),
      (:>),
    )

data LoginRequest = LoginRequest
    { username :: T.Text
    , password :: T.Text
    }
    deriving (Show, Generic)

instance FromJSON LoginRequest

data RegisterRequest = RegisterRequest
    { email :: T.Text
    , username :: T.Text
    , password :: T.Text
    }
    deriving (Show, Generic)

instance FromJSON RegisterRequest

data MeResponse = MeResponse
    {email :: T.Text}
    deriving (Show, Generic)

instance ToJSON MeResponse

type AccountApi =
    "login" :> ReqBody '[JSON] LoginRequest :> PostNoContent
        :<|> "register" :> ReqBody '[JSON] RegisterRequest :> PostNoContent
        :<|> "logout" :> QueryParam "returnUrl" T.Text :> PostNoContent

type UserApi = "me" :> Get '[JSON] MeResponse