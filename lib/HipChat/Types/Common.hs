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
  , Token(..)
  , TokenAuth
  , WebhookAuth(..)
  , camelParseJSON
  , camelToJSON
  , snakeParseJSON
  , snakeToJSON
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Monoid
import           Data.String
import           Data.Text        (Text)
import           GHC.Generics
import           Servant.API

snakeToJSON :: (Generic a, GToJSON (Rep a)) => Int -> a -> Value
snakeToJSON len = genericToJSON defaultOptions{fieldLabelModifier=processSnake len, omitNothingFields = True}

snakeParseJSON :: (Generic a, GFromJSON (Rep a)) => Int -> Value -> Parser a
snakeParseJSON len = genericParseJSON defaultOptions{fieldLabelModifier=processSnake len}

camelToJSON :: (Generic a, GToJSON (Rep a)) => Int -> a -> Value
camelToJSON len = genericToJSON defaultOptions{fieldLabelModifier=processCamel len, omitNothingFields = True}

camelParseJSON :: (Generic a, GFromJSON (Rep a)) => Int -> Value -> Parser a
camelParseJSON len = genericParseJSON defaultOptions{fieldLabelModifier=processCamel len}

processCamel :: Int -> String -> String
processCamel len xs = let (x:xs') = drop len xs
                      in toLower x : xs'

processSnake :: Int -> String -> String
processSnake len xs = camelTo '_' $  drop len xs

data Link = Link
  { linkSelf :: Text
  } deriving (Generic, Show)

instance FromJSON Link where
  parseJSON = snakeParseJSON 4

data PaginatedLink = PaginatedLink
  { plSelf :: Text
  , plPrev :: Maybe Text
  , plNext :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON PaginatedLink where
  parseJSON = snakeParseJSON 2

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
