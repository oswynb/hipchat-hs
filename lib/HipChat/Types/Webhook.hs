{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HipChat.Types.Webhook where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           GHC.Generics

data WebhookResponse = WebhookRoomMessageResponse RoomMessageResponse

instance FromJSON WebhookResponse where
  parseJSON = withObject "WebhookResponse" $ \x -> do
    ty :: Text <- x .: "event"
    case ty of
      "room_message" -> WebhookRoomMessageResponse <$> parseJSON (Object x)
      _ -> fail "NYI"

data RoomMessageResponse = RoomMessageResponse
  { rmrItem :: RoomMessageItem
  } deriving (Eq, Generic, Show)

instance FromJSON RoomMessageResponse where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data RoomMessageItem = RoomMessageItem
  { rmiMessage :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON RoomMessageItem where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
