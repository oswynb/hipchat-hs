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
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid
import           Data.String
import           Data.Text        (Text)
import           GHC.Generics
import           Servant.API

genericHipchatToJSON :: (Generic a, GToJSON (Rep a)) => Int -> a -> Value
genericHipchatToJSON len = genericToJSON defaultOptions{fieldLabelModifier=processField len}

genericHipchatParseJSON :: (Generic a, GFromJSON (Rep a)) => Int -> Value -> Parser a
genericHipchatParseJSON len = genericParseJSON defaultOptions{fieldLabelModifier=processField len}

processField len x = camelTo '_' $  drop len x

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
