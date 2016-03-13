{-# LANGUAGE DeriveGeneric #-}

module HipChat.Types.Rooms.CreateWebhookResponse where

import           Data.Aeson
import           Data.Text            (Text)
import           GHC.Generics

import           HipChat.Types.Common

data CreateWebhookResponse = CreateWebhookResponse
  { cwrId    :: Either String Int
  , cwrLinks :: CreateWebhookResponseLinks
  } deriving (Generic, Show)

data CreateWebhookResponseLinks = CreateWebhookResponseLinks
  { cwrlSelf :: Text
  } deriving (Generic, Show)

instance FromJSON CreateWebhookResponse where
  parseJSON = genericHipChatParseJSON 3

instance FromJSON CreateWebhookResponseLinks where
  parseJSON = genericHipChatParseJSON 4
