{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
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
  , HipchatAuth(..)
  , Theme(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Monoid
import           Data.String
import           Data.Text         (Text)
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

data HipchatAuth = JWT
                 | None
  deriving (Generic, Eq, Show)

instance ToJSON HipchatAuth where
  toJSON JWT  = String "jwt"
  toJSON None = String "none"

instance FromJSON HipchatAuth where
  parseJSON = withText "HipchatAuth" $
    \case
      "jwt"  -> pure JWT
      "none" -> pure None
      x      -> fail (show x <> " is not a valid auth method")

-- | Auth Token
newtype Token = Token Text
  deriving (Show, IsString)

instance ToHttpApiData Token where
  toQueryParam (Token tok) = "Bearer " <> tok

type family TokenAuth a where
  TokenAuth (x :<|> y) = TokenAuth x :<|> TokenAuth y
  TokenAuth x = Header "Authorization" Token :> x

newtype IdOrName = IdOrName Text
  deriving (Generic, Show, IsString, ToHttpApiData)

instance FromJSON IdOrName

data Theme = Light
           | Dark

instance FromHttpApiData Theme where
  parseQueryParam "light" = Right Light
  parseQueryParam "dark"  = Right Dark
  parseQueryParam x       = Left x

instance Show Theme where
  show Light = "light"
  show Dark  = "dark"
