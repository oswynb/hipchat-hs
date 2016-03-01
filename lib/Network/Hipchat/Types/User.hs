{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Hipchat.Types.User where

import           Data.Aeson
import           Data.Text                    (Text)
import           GHC.Generics

import           Network.Hipchat.Types.Common

data User = User
  { userXmppJid    :: Text
  , userIsDeleted  :: Bool
  , userName       :: Text
  , userLastActive :: Text
  , userEmail      :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON User where
  parseJSON = genericHipchatParseJSON 4
