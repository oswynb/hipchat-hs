{-# LANGUAGE DeriveGeneric #-}

module HipChat.Types.Rooms.GetAllMembersResponse where


import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text            (Text)
import           GHC.Generics

import           HipChat.Types.Common

data UserItem = UserItem
  { uiMentionName :: Text
  , uiVersion     :: Text
  , uiId          :: Int
  , uiLinks       :: Maybe Link
  , uiName        :: Text
  } deriving (Generic, Show)

instance FromJSON UserItem where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

data GetAllMembersResponse = GetAllMembersResponse
  { gamrItems      :: [UserItem]
  , gamrStartIndex :: Int
  , gamrMaxResults :: Int
  , gamrLinks      :: PaginatedLink
  } deriving (Generic, Show)

instance FromJSON GetAllMembersResponse where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
