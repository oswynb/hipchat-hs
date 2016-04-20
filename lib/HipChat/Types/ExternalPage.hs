{-# LANGUAGE DeriveGeneric #-}

module HipChat.Types.ExternalPage where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text            (Text)
import           GHC.Generics

import           HipChat.Types.Name

data ExternalPage = ExternalPage
  { externalPageKey  :: Text
  , externalPageName :: Name
  , externalPageUrl  :: Text
  } deriving (Eq, Generic, Show)

instance ToJSON ExternalPage where
  toJSON = genericToJSON $ aesonDrop 12 camelCase

instance FromJSON ExternalPage where
  parseJSON = genericParseJSON $ aesonDrop 12 camelCase
