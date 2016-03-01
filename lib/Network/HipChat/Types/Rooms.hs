{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Network.Hipchat.Types.Rooms
  ( CreateRoomRequest(..)
  , CreateRoomResponse(..)
  , CreateRoomResponseLinks(..)
  , Message(..)
  , RoomEvent(..)
  , CreateWebhookRequest(..)
  , createWebhookRequest
  , CreateWebhookResponse(..)
  , CreateWebhookResponseLinks(..)
  , SendMessageResponse(..)
  , RoomStatistics(..)
  , WebhookKey(..)
  , GetAllMembersResponse(..)
  , UserItem(..)
  ) where

import           Data.Aeson
import           Data.String
import           Data.Text                                         (Text)
import           GHC.Generics

import           Network.Hipchat.Types.Rooms.CreateRoomRequest
import           Network.Hipchat.Types.Rooms.CreateRoomResponse
import           Network.Hipchat.Types.Rooms.CreateWebhookRequest
import           Network.Hipchat.Types.Rooms.CreateWebhookResponse
import           Network.Hipchat.Types.Rooms.GetAllMembersResponse


newtype Message = Message {
  message :: Text
} deriving (Generic, IsString, Show)
instance ToJSON Message

data RoomStatistics = RoomStatistics
  { messages_sent :: Int
  , last_active   :: String
  } deriving (Show, Generic)
instance FromJSON RoomStatistics

data SendMessageResponse = SendMessageResponse
  { timestamp :: Text
  , id        :: Text
  } deriving (Show, Generic)
instance FromJSON SendMessageResponse
