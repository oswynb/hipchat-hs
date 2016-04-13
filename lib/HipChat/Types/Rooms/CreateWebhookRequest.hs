{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HipChat.Types.Rooms.CreateWebhookRequest where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.String
import           Data.Text            (Text)
import           GHC.Generics
import           Servant.API

import           HipChat.Types.Common

data CreateWebhookRequest = CreateWebhookRequest
  { createWebhookRequestName           :: Maybe Text
  , createWebhookRequestUrl            :: Text
  , createWebhookRequestPattern        :: Maybe Text
  , createWebhookRequestAuthentication :: Maybe WebhookAuth
  , createWebhookRequestKey            :: Maybe WebhookKey
  , createWebhookRequestEvent          :: RoomEvent
  } deriving (Generic, Show)

newtype WebhookKey = WebhookKey Text
  deriving (Generic, IsString, Show, ToHttpApiData)

instance ToJSON WebhookKey where
  toJSON = genericToJSON $ aesonDrop 20 camelCase

createWebhookRequest :: Text -> RoomEvent -> CreateWebhookRequest
createWebhookRequest url = CreateWebhookRequest Nothing url Nothing Nothing Nothing

instance ToJSON CreateWebhookRequest where
  toJSON = genericToJSON $ aesonPrefix snakeCase
