{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Hipchat.Types.Rooms.CreateWebhookRequest where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.String
import           Data.Text                    (Text)
import           GHC.Generics
import           Servant.API

import           Network.Hipchat.Types.Common

data CreateWebhookRequest = CreateWebhookRequest
  { cwrName           :: Maybe Text
  , cwrUrl            :: Text
  , cwrPattern        :: Maybe Text
  , cwrAuthentication :: Maybe WebhookAuth
  , cwrKey            :: Maybe WebhookKey
  , cwrEvent          :: RoomEvent
  } deriving (Generic, Show)

data RoomEvent = RoomArchived
               | RoomCreated
               | RoomDeleted
               | RoomEnter
               | RoomExit
               | RoomFileUpload
               | RoomMessage
               | RoomNotification
               | RoomTopicChange
               | RoomUnarchived
  deriving (Generic, Show)

newtype WebhookKey = WebhookKey Text
  deriving (FromText, Generic, ToText, IsString, Show)

instance ToJSON WebhookKey

instance ToJSON RoomEvent where
  toJSON = genericToJSON defaultOptions{constructorTagModifier=camelTo '_'}

createWebhookRequest :: Text -> RoomEvent -> CreateWebhookRequest
createWebhookRequest url event = CreateWebhookRequest Nothing url Nothing Nothing Nothing event

instance ToJSON CreateWebhookRequest where
  toJSON = genericHipchatToJSON 3
