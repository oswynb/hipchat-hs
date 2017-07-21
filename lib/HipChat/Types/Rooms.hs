{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module HipChat.Types.Rooms
  ( CreateRoomRequest(..)
  , CreateRoomResponse(..)
  , CreateRoomResponseLinks(..)
  , CreateWebhookResponse(..)
  , CreateWebhookResponseLinks(..)
  , GetAllMembersResponse(..)
  , GetAllRoomsResponse(..)
  , Message(..)
  , RoomEvent(..)
  , RoomStatistics(..)
  , SendMessageResponse(..)
  , UserItem(..)
  , NotificationColor(..)
  , SendNotificationRequest(..)
  , MessageFormat(..)
  , CompoundDescription(..)
  , Description(..)
  , Thumbnail(..)
  , Card(..)
  , CardAttribute(..)
  , CardAttributeValue(..)
  , CardFormat(..)
  , CardStyle(..)
  , simpleThumbnail
  ) where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Char
import           Data.String
import           Data.Text                                 (Text)
import           GHC.Generics

import           HipChat.Types.Common
import           HipChat.Types.Icon
import           HipChat.Types.Rooms.CreateRoomRequest
import           HipChat.Types.Rooms.CreateRoomResponse
import           HipChat.Types.Rooms.CreateWebhookResponse
import           HipChat.Types.Rooms.GetAllMembersResponse
import           HipChat.Types.Rooms.GetAllRoomsResponse


newtype Message = Message {
  message :: Text
} deriving (Generic, IsString, Show)
instance ToJSON Message

data RoomStatistics = RoomStatistics
  { roomStatisticsMessagesSent :: Int
  , roomStatisticsLastActive   :: String
  } deriving (Show, Generic)

instance FromJSON RoomStatistics where
  parseJSON = genericParseJSON $ aesonDrop 14 snakeCase

data SendMessageResponse = SendMessageResponse
  { timestamp :: Text
  , id        :: Text
  } deriving (Show, Generic)
instance FromJSON SendMessageResponse

data NotificationColor = Yellow
                       | Green
                       | Red
                       | Purple
                       | Gray
                       | Random
  deriving (Show, Eq, Generic)

instance ToJSON NotificationColor where
  toJSON = genericToJSON defaultOptions{constructorTagModifier=map toLower}

instance FromJSON NotificationColor where
  parseJSON = genericParseJSON defaultOptions{constructorTagModifier=map toLower}

data MessageFormat = Text
                   | Html
  deriving (Show, Eq, Generic)

instance ToJSON MessageFormat where
  toJSON = genericToJSON (aesonDrop 0 snakeCase){constructorTagModifier=map toLower}

instance FromJSON MessageFormat where
  parseJSON = genericParseJSON (aesonDrop 0 snakeCase){constructorTagModifier=map toLower}

data SendNotificationRequest = SendNotificationRequest
  { messageFormat :: Maybe MessageFormat
  , notify        :: Maybe Bool
  , color         :: NotificationColor
  , message       :: Text
  , card          :: Maybe Card
  } deriving (Generic, Show)

instance ToJSON SendNotificationRequest where
  toEncoding = genericToEncoding (aesonDrop 0 snakeCase){omitNothingFields = True}

data CardStyle = File
               | Image
               | Application
               | Link
               | Media
  deriving (Generic, Show)

instance ToJSON CardStyle where
  toEncoding = genericToEncoding defaultOptions{constructorTagModifier=map toLower}

data CardFormat = Compact
                | Medium
  deriving (Generic, Show)

instance ToJSON CardFormat where
  toEncoding = genericToEncoding defaultOptions{constructorTagModifier=map toLower}

data CardAttributeValue = CardAttributeValue
  { url   :: Maybe Text
  , label :: Text
  , icon  :: Maybe CompoundIcon
  } deriving (Generic, Show)

instance ToJSON CardAttributeValue where
  toEncoding = genericToEncoding (aesonDrop 0 snakeCase){omitNothingFields = True}

data CardAttribute = CardAttribute
  { label :: Maybe Text
  , value :: CardAttributeValue
  } deriving (Generic, Show)

instance ToJSON CardAttribute where
  toEncoding = genericToEncoding (aesonDrop 0 snakeCase){omitNothingFields = True}

data Description = Description
  { value  :: Text
  , format :: MessageFormat
  } deriving (Generic, Show)

instance ToJSON Description where
  toEncoding = genericToEncoding (aesonDrop 0 snakeCase){omitNothingFields = True}

newtype CompoundDescription = CompoundDescription
  { unCompoundDescription :: Either Text Description
  } deriving Show

instance ToJSON CompoundDescription where
  toJSON (CompoundDescription (Left  x)) = toJSON x
  toJSON (CompoundDescription (Right x)) = toJSON x

data Thumbnail = Thumbnail
  { url    :: Text       -- ^ Url for the thumbnail.
  , url2x  :: Maybe Text -- ^ Url for the retina version of the thumbnail.
  , width  :: Maybe Int  -- ^ The original width of the image
  , height :: Maybe Int  -- ^ The original height of the image
  } deriving (Show, Eq)

simpleThumbnail :: Text -> Thumbnail
simpleThumbnail url = Thumbnail url Nothing Nothing Nothing

instance ToJSON Thumbnail where
  toJSON (Thumbnail _url _url2x _width _height) = object
    [ "url"    .= _url
    , "url@2x" .= _url2x
    , "width"  .= _width
    , "height" .= _height
    ]

instance FromJSON Thumbnail where
  parseJSON (Object x) = Thumbnail <$> x .: "url"
                                   <*> x .: "url@2x"
                                   <*> x .: "width"
                                   <*> x .: "height"
  parseJSON x = typeMismatch "Thumbnail" x

data Card = Card
  { style       :: CardStyle
  , description :: CompoundDescription
  , format      :: Maybe CardFormat
  , url         :: Maybe Text
  , title       :: Text
  , thumbnail   :: Maybe Thumbnail
  , attributes  :: [CardAttribute]
  , id          :: Text
  , icon        :: Maybe CompoundIcon
  } deriving (Generic, Show)

instance ToJSON Card where
  toEncoding = genericToEncoding (aesonDrop 0 snakeCase){omitNothingFields = True}
