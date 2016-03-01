{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Network.Hipchat.Types.Common
  ( genericHipchatToJSON
  , genericHipchatParseJSON
  , WebhookAuth(..)
  , Token(..)
  , TokenAuth
  , IdOrName(..)
  , Link(..)
  , PaginatedLink(..)
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid
import           Data.String
import           Data.Text        (Text)
import           GHC.Generics
import           Servant.API

genericHipchatToJSON :: (Generic a, GToJSON (Rep a)) => Int -> a -> Value
genericHipchatToJSON len = genericToJSON defaultOptions{fieldLabelModifier=processField len, omitNothingFields = True}

genericHipchatParseJSON :: (Generic a, GFromJSON (Rep a)) => Int -> Value -> Parser a
genericHipchatParseJSON len = genericParseJSON defaultOptions{fieldLabelModifier=processField len}

processField :: Int -> String -> String
processField len x = camelTo '_' $  drop len x

data Link = Link
  { linkSelf :: Text
  } deriving (Generic, Show)

instance FromJSON Link where
  parseJSON = genericHipchatParseJSON 4

data PaginatedLink = PaginatedLink
  { plSelf :: Text
  , plPrev :: Maybe Text
  , plNext :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON PaginatedLink where
  parseJSON = genericHipchatParseJSON 2

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
  TokenAuth x = Header "Authorization" Token :> x

newtype IdOrName = IdOrName Text
  deriving (FromText, Generic, Show, ToText, IsString)

instance FromJSON IdOrName
