{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HipChat.Types.Webhook where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Monoid
import           Data.Text         (Text)
import qualified Data.Text         as T
import           GHC.Generics

data WebhookResponse = WebhookResponse
  { event :: Text
  , item  :: WebhookItem
  } deriving (Eq, Generic, Show)

instance FromJSON WebhookResponse where
  parseJSON v = flip (withObject "WebhookResponse") v $ \o -> do
    event <- o .: "event"
    rawItem  <- o .: "item"
    item <- case event of
      "room_message"      -> RoomMessageItem'      <$> parseJSON rawItem
      "room_notification" -> RoomNotificationItem' <$> parseJSON rawItem
      x -> fail $ "Unsupported webhook type " <> T.unpack x
    return WebhookResponse{..}

data WebhookItem = RoomMessageItem' RoomMessageItem
                 | RoomNotificationItem' RoomNotificationItem
  deriving (Eq, Generic, Show)

instance ToJSON WebhookItem where
  toJSON = genericToJSON defaultOptions{sumEncoding=UntaggedValue}

instance FromJSON WebhookItem where
  parseJSON = genericParseJSON defaultOptions{sumEncoding=UntaggedValue}

data RoomNotificationItem = RoomNotificationItem
  { rniMessage :: NotificationObject
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

newtype MessageObject = MessageObject
  { moMessage :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON MessageObject where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON MessageObject where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data NotificationObject = NotificationObject
  { noMessage       :: Text
  , noType          :: Text
  , noMessageFormat :: Text
  , noDate          :: Text
  , noFrom          :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON NotificationObject where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON NotificationObject where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

newtype RoomObject = RoomObject
  { roId :: Int
  } deriving (Eq, Generic, Show)

instance ToJSON RoomObject where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON RoomObject where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
