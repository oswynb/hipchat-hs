{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HipChat.Types.Rooms.CreateRoomRequest where

import           Data.Aeson
import           Data.Text            (Text)
import           GHC.Generics

import           HipChat.Types.Common

data RoomPrivacy = Public
                 | Private

instance ToJSON RoomPrivacy where
  toJSON Public  = String "public"
  toJSON Private = String "private"

data CreateRoomRequest = CreateRoomRequest
  { crrName                    :: Text
  , crrPrivacy                 :: Maybe RoomPrivacy
  , crrDelegateAdminVisibility :: Maybe Bool
  , crrTopic                   :: Maybe Text
  , crrOwnerUserId             :: Maybe (Either Text Int)
  , crrGuestAccess             :: Maybe Bool
  } deriving (Generic)

instance ToJSON CreateRoomRequest where
  toJSON = snakeToJSON 3
