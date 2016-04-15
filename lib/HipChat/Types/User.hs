{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HipChat.Types.User where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           GHC.Generics

data User = User
  { userXmppJid    :: Text
  , userIsDeleted  :: Bool
  , userName       :: Text
  , userLastActive :: Text
  , userEmail      :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON User where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
