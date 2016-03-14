{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
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
  , Message(..)
  , RoomEvent(..)
  , RoomStatistics(..)
  , SendMessageResponse(..)
  , UserItem(..)
  , WebhookKey(..)
  , createWebhookRequest
  ) where

import           Data.Aeson
import           Data.String
import           Data.Text                                 (Text)
import           GHC.Generics

import           HipChat.Types.Rooms.CreateRoomRequest
import           HipChat.Types.Rooms.CreateRoomResponse
import           HipChat.Types.Rooms.CreateWebhookRequest
import           HipChat.Types.Rooms.CreateWebhookResponse
import           HipChat.Types.Rooms.GetAllMembersResponse


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
