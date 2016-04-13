{-# LANGUAGE DeriveGeneric #-}

module HipChat.Types.InstallInfo where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           GHC.Generics

data InstallInfo = InstallInfo
  { installInfoOauthId         :: Text
  , installInfoCapabilitiesUrl :: Text
  , installInfoRoomId          :: Maybe Int
  , installInfoGroupId         :: Int
  , installInfoOauthSecret     :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON InstallInfo where
  parseJSON = genericParseJSON $ aesonDrop 11 camelCase
