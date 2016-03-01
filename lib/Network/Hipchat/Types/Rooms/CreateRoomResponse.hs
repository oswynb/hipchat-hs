{-# LANGUAGE DeriveGeneric #-}

module Network.Hipchat.Types.Rooms.CreateRoomResponse where

import           Data.Aeson
import           Data.Text                    (Text)
import           GHC.Generics

import           Network.Hipchat.Types.Common

data CreateRoomResponse = CreateRoomResponse
  { crrId    :: IdOrName
  , crrLinks :: CreateRoomResponseLinks
  } deriving (Generic, Show)

data CreateRoomResponseLinks = CreateRoomResponseLinks
  { crrlSelf :: Text
  } deriving (Generic, Show)

instance FromJSON CreateRoomResponse where
  parseJSON = genericHipchatParseJSON 3

instance FromJSON CreateRoomResponseLinks where
  parseJSON = genericHipchatParseJSON 4
