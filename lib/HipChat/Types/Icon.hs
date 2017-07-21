{-# LANGUAGE OverloadedStrings #-}

module HipChat.Types.Icon where

-- Common icon objects

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text        (Text)

simpleCompoundIcon :: Text -> CompoundIcon
simpleCompoundIcon url = CompoundIcon $ Left url

newtype CompoundIcon = CompoundIcon
  { unCompoundIcon :: Either Text Icon
  } deriving Show

instance ToJSON CompoundIcon where
  toJSON (CompoundIcon (Left  x)) = toJSON x
  toJSON (CompoundIcon (Right x)) = toJSON x

data Icon = Icon
  { iconUrl   :: Text -- ^ Url for the icon.
  , iconUrl2x :: Text -- ^ Url for the retina version of the icon.
  } deriving (Show, Eq)

instance ToJSON Icon where
  toJSON (Icon url url2x) = object
    [ "url"    .= url
    , "url@2x" .= url2x
    ]

instance FromJSON Icon where
  parseJSON (Object x) = Icon <$> x .: "url" <*> x .: "url@2x"
  parseJSON x = typeMismatch "Icon" x
