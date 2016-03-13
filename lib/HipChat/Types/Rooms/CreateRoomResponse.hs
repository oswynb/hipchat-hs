{-# LANGUAGE DeriveGeneric #-}

module HipChat.Types.Rooms.CreateRoomResponse where

import           Data.Aeson
import           Data.Text            (Text)
import           GHC.Generics

import           HipChat.Types.Common

data CreateRoomResponse = CreateRoomResponse
  { crrId    :: IdOrName
  , crrLinks :: CreateRoomResponseLinks
  } deriving (Generic, Show)

data CreateRoomResponseLinks = CreateRoomResponseLinks
  { crrlSelf :: Text
  } deriving (Generic, Show)

instance FromJSON CreateRoomResponse where
  parseJSON = genericHipChatParseJSON 3

instance FromJSON CreateRoomResponseLinks where
  parseJSON = genericHipChatParseJSON 4
