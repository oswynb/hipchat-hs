{-# LANGUAGE OverloadedStrings #-}

module HipChat.Types.Key where

-- Common length restricted key

import           Control.Lens
import           Data.Aeson
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Data.Text    (Text)
import qualified Data.Text    as T

newtype Key = Key
  { unKey :: Text
  } deriving (Eq, Show)

key :: Prism' Text Key
key = prism' enc dec
  where
    enc = unKey
    dec x = let l = T.length x
            in if l >= 1 && l <= 40 then
              Just $ Key x
            else
              Nothing

instance IsString Key where
  fromString x = fromJust (T.pack x ^? key)

instance ToJSON Key where
  toJSON (Key x) = toJSON x

instance FromJSON Key where
  parseJSON x = do
    rawKey <- parseJSON x
    case rawKey ^? key of
      Nothing -> fail $ "Invalid key length, expected [1,40], got: " <> show (T.length rawKey)
      Just k  -> return k
