{-# LANGUAGE DeriveGeneric #-}

module HipChat.Types.Rooms.CreateWebhookResponse where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text            (Text)
import           GHC.Generics

data CreateWebhookResponse = CreateWebhookResponse
  { cwrId    :: Either String Int
  , cwrLinks :: CreateWebhookResponseLinks
  } deriving (Generic, Show)

data CreateWebhookResponseLinks = CreateWebhookResponseLinks
  { cwrlSelf :: Text
  } deriving (Generic, Show)

instance FromJSON CreateWebhookResponse where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

instance FromJSON CreateWebhookResponseLinks where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
