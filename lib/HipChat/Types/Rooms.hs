{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module HipChat.Types.Rooms
  ( CreateRoomRequest(..)
  , CreateRoomResponse(..)
  , CreateRoomResponseLinks(..)
  , CreateWebhookRequest(..)
  , CreateWebhookResponse(..)
  , CreateWebhookResponseLinks(..)
  , GetAllMembersResponse(..)
  , GetAllRoomsResponse(..)
  , Message(..)
  , RoomEvent(..)
  , RoomStatistics(..)
  , SendMessageResponse(..)
  , UserItem(..)
  , WebhookKey(..)
  , createWebhookRequest
  , NotificationColor(..)
  , SendNotificationRequest(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Char
import           Data.String
import           Data.Text                                 (Text)
import           GHC.Generics

import           HipChat.Types.Common
import           HipChat.Types.Rooms.CreateRoomRequest
import           HipChat.Types.Rooms.CreateRoomResponse
import           HipChat.Types.Rooms.CreateWebhookRequest
import           HipChat.Types.Rooms.CreateWebhookResponse
import           HipChat.Types.Rooms.GetAllMembersResponse
import           HipChat.Types.Rooms.GetAllRoomsResponse


newtype Message = Message {
  message :: Text
} deriving (Generic, IsString, Show)
instance ToJSON Message

data RoomStatistics = RoomStatistics
  { roomStatisticsMessagesSent :: Int
  , roomStatisticsLastActive   :: String
  } deriving (Show, Generic)

instance FromJSON RoomStatistics where
  parseJSON = genericParseJSON $ aesonDrop 14 snakeCase

data SendMessageResponse = SendMessageResponse
  { timestamp :: Text
  , id        :: Text
  } deriving (Show, Generic)
instance FromJSON SendMessageResponse

data NotificationColor = Yellow
                       | Green
                       | Red
                       | Purple
                       | Gray
                       | Random
  deriving (Show, Generic)

instance ToJSON NotificationColor where
  toJSON = genericToJSON defaultOptions{constructorTagModifier=map toLower}

data SendNotificationRequest = SendNotificationRequest
  { color   :: NotificationColor
  , message :: Text
  } deriving (Generic, Show)

instance ToJSON SendNotificationRequest
