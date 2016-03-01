{-# LANGUAGE DeriveGeneric #-}

module Network.Hipchat.Types.Rooms.GetAllMembersResponse where


import           Data.Aeson
import           Data.Aeson.Types
import Data.Char
import           Data.Text                    (Text)
import GHC.Generics

import Network.Hipchat.Types.Common

data UserItem = UserItem
  { uiMentionName :: Text
  , uiVersion :: Text
  , uiId :: Int 
  , uiLinks :: Maybe Link
  , uiName :: Text
  } deriving (Generic, Show)


instance FromJSON UserItem where
  parseJSON = genericHipchatParseJSON 2

data GetAllMembersResponse = GetAllMembersResponse
  { gamrItems :: [UserItem]
  , gamrStartIndex :: Int
  , gamrMaxResults :: Int
  , gamrLinks :: PaginatedLink
  } deriving (Generic, Show)

instance FromJSON GetAllMembersResponse where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 4 x in toLower y:ys}