{-# LANGUAGE DeriveGeneric #-}

module HipChat.Types.RoomAddonUIUpdateRequest where

import           Data.Aeson
import           Data.Aeson.Casing
import           GHC.Generics

import           HipChat.Types.Glance

data RoomAddonUIUpdateRequest = RoomAddonUIUpdateRequest
  { roomUIUpdateGlance :: [GlanceUpdate]
  } deriving (Eq, Generic, Show)

instance ToJSON RoomAddonUIUpdateRequest where
  toJSON = genericToJSON $ aesonDrop 12 camelCase
