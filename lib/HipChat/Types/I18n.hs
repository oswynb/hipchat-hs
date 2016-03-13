{-# LANGUAGE OverloadedStrings #-}

module HipChat.Types.I18n where

-- Common name object

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Monoid
import           Data.Text        (Text)

data I18n = I18n
  { _I18n  :: Maybe Text -- ^ The optional localization key, used to look up the localized value. Valid length range: 1 - 40.
  , _Value :: Text       -- ^ The default text. Valid length range: 1 - 100.
  } deriving (Show, Eq)

instance ToJSON I18n where
  toJSON (I18n i18n value) =
    object $ maybe [] (\x -> [ "i18n" .= x ]) i18n <> [ "value" .= value ]

instance FromJSON I18n where
  parseJSON (Object x) = I18n <$> x .:? "i18n" <*> x .: "value"
  parseJSON x = typeMismatch "I18n" x

