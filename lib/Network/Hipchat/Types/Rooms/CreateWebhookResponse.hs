{-# LANGUAGE DeriveGeneric #-}

module Network.Hipchat.Types.Rooms.CreateWebhookResponse where

import           Data.Aeson
import           Data.Text                    (Text)
import           GHC.Generics

import           Network.Hipchat.Types.Common

data CreateWebhookResponse = CreateWebhookResponse
  { cwrId    :: Either String Int
  , cwrLinks :: CreateWebhookResponseLinks
  } deriving (Generic, Show)

data CreateWebhookResponseLinks = CreateWebhookResponseLinks
  { cwrlSelf :: Text
  } deriving (Generic, Show)

instance FromJSON CreateWebhookResponse where
  parseJSON = genericHipchatParseJSON 3

instance FromJSON CreateWebhookResponseLinks where
  parseJSON = genericHipchatParseJSON 4
