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
  , CreateWebhookResponse(..)
  , CreateWebhookResponseLinks(..)
  , GetAllMembersResponse(..)
  , GetAllRoomsResponse(..)
  , Message(..)
  , RoomEvent(..)
  , RoomStatistics(..)
  , SendMessageResponse(..)
  , UserItem(..)
  , NotificationColor(..)
  , SendNotificationRequest(..)
  , MessageFormat(..)
  , Card(..)
  , CardAttribute(..)
  , CardAttributeValue(..)
  , CardFormat(..)
  , CardStyle(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Char
import           Data.String
import           Data.Text                                 (Text)
import           GHC.Generics

import           HipChat.Types.Common
import           HipChat.Types.Icon
import           HipChat.Types.Rooms.CreateRoomRequest
import           HipChat.Types.Rooms.CreateRoomResponse
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
  deriving (Show, Eq, Generic)

instance ToJSON NotificationColor where
  toJSON = genericToJSON defaultOptions{constructorTagModifier=map toLower}

instance FromJSON NotificationColor where
  parseJSON = genericParseJSON defaultOptions{constructorTagModifier=map toLower}

data MessageFormat = Text
                   | Html
  deriving (Show, Eq, Generic)

instance ToJSON MessageFormat where
  toJSON = genericToJSON (aesonDrop 0 snakeCase){constructorTagModifier=map toLower}

instance FromJSON MessageFormat where
  parseJSON = genericParseJSON (aesonDrop 0 snakeCase){constructorTagModifier=map toLower}

data SendNotificationRequest = SendNotificationRequest
  { messageFormat :: Maybe MessageFormat
  , notify        :: Maybe Bool
  , color         :: NotificationColor
  , message       :: Text
  , card          :: Maybe Card
  } deriving (Generic, Show)

instance ToJSON SendNotificationRequest where
  toEncoding = genericToEncoding (aesonDrop 0 snakeCase){omitNothingFields = True}

data CardStyle = File
               | Image
               | Application
               | Link
               | Media
  deriving (Generic, Show)

instance ToJSON CardStyle where
  toEncoding = genericToEncoding defaultOptions{constructorTagModifier=map toLower}

data CardFormat = Compact
                | Medium
  deriving (Generic, Show)

instance ToJSON CardFormat where
  toEncoding = genericToEncoding defaultOptions{constructorTagModifier=map toLower}

data CardAttributeValue = CardAttributeValue
  { url   :: Maybe Text
  , label :: Text
  , icon  :: Maybe CompoundIcon
  } deriving (Generic, Show)

instance ToJSON CardAttributeValue where
  toEncoding = genericToEncoding (aesonDrop 0 snakeCase){omitNothingFields = True}

data CardAttribute = CardAttribute
  { label :: Maybe Text
  , value :: CardAttributeValue
  } deriving (Generic, Show)

instance ToJSON CardAttribute where
  toEncoding = genericToEncoding (aesonDrop 0 snakeCase){omitNothingFields = True}

data Card = Card
  { style      :: CardStyle
  , format     :: Maybe CardFormat
  , url        :: Maybe Text
  , title      :: Text
  , attributes :: [CardAttribute]
  , id         :: Text
  , icon       :: Maybe CompoundIcon
  } deriving (Generic, Show)

instance ToJSON Card where
  toEncoding = genericToEncoding (aesonDrop 0 snakeCase){omitNothingFields = True}
