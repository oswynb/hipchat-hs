{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HipChat.Types.Webhook where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           GHC.Generics

data WebhookResponse = RoomMessageResponse
  { rmrEvent :: Text
  , rmrItem  :: RoomMessageItem
  } deriving (Eq, Generic, Show)

instance FromJSON WebhookResponse where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data RoomMessageItem = RoomMessageItem
  { rmiMessage :: MessageObject
  , rmiRoom    :: RoomObject
  } deriving (Eq, Generic, Show)

instance FromJSON RoomMessageItem where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data MessageObject = MessageObject
  { moMessage :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON MessageObject where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data RoomObject = RoomObject
  { roId :: Int
  } deriving (Eq, Generic, Show)

instance FromJSON RoomObject where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
