{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module HipChat.Types.Common
  ( IdOrName(..)
  , Link(..)
  , PaginatedLink(..)
  , RoomEvent(..)
  , Token(..)
  , TokenAuth
  , WebhookAuth(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Monoid
import           Data.String
import           Data.Text        (Text)
import           GHC.Generics
import           Servant.API

data RoomEvent = RoomArchived
               | RoomCreated
               | RoomDeleted
               | RoomEnter
               | RoomExit
               | RoomFileUpload
               | RoomMessage
               | RoomNotification
               | RoomTopicChange
               | RoomUnarchived
  deriving (Eq, Generic, Show)

instance ToJSON RoomEvent where
  toJSON = genericToJSON defaultOptions{constructorTagModifier=camelTo2 '_'}

instance FromJSON RoomEvent where
  parseJSON = genericParseJSON defaultOptions{constructorTagModifier=camelTo2 '_'}
data Link = Link
  { linkSelf :: Text
  } deriving (Generic, Show)

instance FromJSON Link where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data PaginatedLink = PaginatedLink
  { paginatedSelf :: Text
  , paginatedPrev :: Maybe Text
  , paginatedNext :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON PaginatedLink where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data WebhookAuth = JWT
                 | None
  deriving (Generic, Show)

instance ToJSON WebhookAuth where
  toJSON JWT  = String "jwt"
  toJSON None = String "none"

-- | Auth Token
newtype Token = Token Text
  deriving (Show, IsString)

instance ToText Token where
  toText (Token tok) = "Bearer " <> tok

type family TokenAuth a where
  TokenAuth (x :<|> y) = TokenAuth x :<|> TokenAuth y
  TokenAuth x = Header "Authorization" Token :> x

newtype IdOrName = IdOrName Text
  deriving (FromText, Generic, Show, ToText, IsString)

instance FromJSON IdOrName
