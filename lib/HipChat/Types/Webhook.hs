{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HipChat.Types.Webhook where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Text         (Text)
import           GHC.Generics

data WebhookResponse = RoomMessageResponse
  { event :: Text
  , item  :: WebhookItem
  } deriving (Eq, Generic, Show)

instance FromJSON WebhookResponse where
  parseJSON = genericParseJSON defaultOptions

data WebhookItem = RoomMessageItem' RoomMessageItem
                 | RoomNotificationItem' RoomNotificationItem
  deriving (Eq, Generic, Show)

instance ToJSON WebhookItem where
  toJSON = genericToJSON defaultOptions{sumEncoding=UntaggedValue}

instance FromJSON WebhookItem where
  parseJSON = genericParseJSON defaultOptions{sumEncoding=UntaggedValue}

data RoomNotificationItem = RoomNotificationItem
  { rniMessage :: MessageObject
  , rniRoom    :: RoomObject
  } deriving (Eq, Generic, Show)

instance ToJSON RoomNotificationItem where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON RoomNotificationItem where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data RoomMessageItem = RoomMessageItem
  { rmiMessage :: MessageObject
  , rmiRoom    :: RoomObject
  } deriving (Eq, Generic, Show)

instance ToJSON RoomMessageItem where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON RoomMessageItem where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data MessageObject = MessageObject
  { moMessage       :: Text
  , moType          :: Text
  , moMessageFormat :: Text
  , moDate          :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON MessageObject where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON MessageObject where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype RoomObject = RoomObject
  { roId :: Int
  } deriving (Eq, Generic, Show)

instance ToJSON RoomObject where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON RoomObject where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
