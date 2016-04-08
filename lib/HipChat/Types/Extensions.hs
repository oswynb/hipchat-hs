{-# LANGUAGE DeriveGeneric #-}

module HipChat.Types.Extensions where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Text            (Text)
import           GHC.Generics

import           HipChat.Types.Common

type AddonAction = ()

data CapabilitiesAdminPage = CapabilitiesAdminPage
  { capUrl :: Text
  } deriving (Generic, Show)

instance ToJSON CapabilitiesAdminPage where
  toJSON = snakeToJSON 3

instance FromJSON CapabilitiesAdminPage where
  parseJSON = snakeParseJSON 3

data CapabilitiesInstallable = CapabilitiesInstallable
  { ciAllowGlobal :: Maybe Bool
  , ciAllowRoom   :: Maybe Bool
  -- | The URL to receive a confirmation of an integration installation. The message will be an HTTP POST with the
  --   following fields in a JSON-encoded body: 'capabilitiesUrl', 'oauthId', 'oauthSecret', and optionally 'roomId'.
  --   The installation of the integration will only succeed if the POST response is a 200.
  , ciCallbackUrl :: Maybe Text
 -- , ciInstalledUrl :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON CapabilitiesInstallable where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys, omitNothingFields = True}

instance FromJSON CapabilitiesInstallable where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys, omitNothingFields = True}

data Capabilities = Capabilities
  { cAction             :: Maybe [AddonAction]
  , cAdminPage          :: Maybe CapabilitiesAdminPage
  , cConfigurable       :: Maybe ()
  , cDialog             :: Maybe [()]
  , cExternalPage       :: Maybe [()]
  , cGlance             :: Maybe [()]
  , cHipChatApiConsumer :: Maybe HipChatApiConsumer
  , cInstallable        :: Maybe CapabilitiesInstallable
  } deriving (Generic, Show)

instance ToJSON Capabilities where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 1 x in toLower y:ys, omitNothingFields = True}

instance FromJSON Capabilities where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 1 x in toLower y:ys, omitNothingFields = True}

data HipChatApiConsumer = HipChatApiConsumer
  { hacAvatar   :: Maybe Text
  , hacFromName :: Maybe Text
  , hacScopes   :: [Text]
  } deriving (Generic, Show)

instance ToJSON HipChatApiConsumer where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 3 x in toLower y:ys, omitNothingFields = True}

instance FromJSON HipChatApiConsumer where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 3 x in toLower y:ys, omitNothingFields = True}

data CapabilitiesLinks = CapabilitiesLinks
  { clHomepage :: Maybe Text
  , clSelf     :: Text
  } deriving (Generic, Show)

capabilitiesLinks :: Text -> CapabilitiesLinks
capabilitiesLinks = CapabilitiesLinks Nothing

instance ToJSON CapabilitiesLinks where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys, omitNothingFields = True}

instance FromJSON CapabilitiesLinks where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys, omitNothingFields = True}

data CapabilitiesVendor = CapabilitiesVendor
  {

  } deriving (Generic, Show)

instance ToJSON CapabilitiesVendor where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys, omitNothingFields = True}

instance FromJSON CapabilitiesVendor where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys, omitNothingFields = True}

data CapabilitiesDescriptor = CapabilitiesDescriptor
  { cdApiVersion   :: Maybe Text
  , cdCapabilities :: Maybe Capabilities
  , cdDescription  :: Text
  , cdKey          :: Text
  , cdLinks        :: CapabilitiesLinks
  , cdName         :: Text
  , cdVendor       :: Maybe CapabilitiesVendor
  } deriving (Generic, Show)

capabilitiesDescriptor :: Text -> Text -> CapabilitiesLinks -> Text -> CapabilitiesDescriptor
capabilitiesDescriptor desc key links name = CapabilitiesDescriptor Nothing Nothing desc key links name Nothing

instance ToJSON CapabilitiesDescriptor where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys, omitNothingFields = True}

instance FromJSON CapabilitiesDescriptor where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys, omitNothingFields = True}
