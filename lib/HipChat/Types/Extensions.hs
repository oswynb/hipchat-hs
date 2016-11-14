{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HipChat.Types.Extensions where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Maybe
import           Data.Monoid
import           Data.Text                          (Text)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics

import           HipChat.Types.Auth
import           HipChat.Types.Common
import           HipChat.Types.Dialog
import           HipChat.Types.ExternalPage
import           HipChat.Types.Glance
import           HipChat.Types.WebPanel

data CapabilitiesAdminPage = CapabilitiesAdminPage
  { capUrl :: Text
  } deriving (Generic, Show)

instance ToJSON CapabilitiesAdminPage where
  toJSON = genericToJSON $ aesonPrefix snakeCase

instance FromJSON CapabilitiesAdminPage where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase

--------------------------------------------------------------------------------

-- Installable

data Installable = Installable
  { -- | The URL to receive a confirmation of an integration installation. The message will be an HTTP POST with the
    --   following fields in a JSON-encoded body: 'capabilitiesUrl', 'oauthId', 'oauthSecret', and optionally 'roomId'.
    --   The installation of the integration will only succeed if the POST response is a 200.
    installableCallbackUrl :: Maybe Text
  , installableAllowRoom   :: Bool
  , installableAllowGlobal :: Bool
  } deriving (Show, Eq)

instance ToJSON Installable where
  toJSON (Installable cb r g) = object $ catMaybes
    [ ("callbackUrl" .=) <$> cb
    ] <>
    [ "allowRoom" .= r
    , "allowGlobal" .= g
    ]

instance FromJSON Installable where
  parseJSON = withObject "object" $ \o -> Installable
    <$> o .:? "callbackUrl"
    <*> o .:? "allowRoom" .!= True
    <*> o .:? "allowGlobal" .!= True

defaultInstallable :: Installable
defaultInstallable = Installable Nothing True True

--------------------------------------------------------------------------------

-- Capabilities

data Capabilities = Capabilities
  { capabilitiesInstallable        :: Maybe Installable
  , capabilitiesHipchatApiConsumer :: Maybe APIConsumer
  , capabilitiesOauth2Provider     :: Maybe OAuth2Provider
  , capabilitiesWebhooks           :: [Webhook]
  , capabilitiesConfigurable       :: Maybe Configurable
  , capabilitiesDialog             :: [Dialog]
  , capabilitiesWebPanel           :: [WebPanel]
  , capabilitiesGlance             :: [Glance]
  , capabilitiesExternalPage       :: [ExternalPage]
  } deriving (Show, Eq)

defaultCapabilities :: Capabilities
defaultCapabilities = Capabilities Nothing Nothing Nothing [] Nothing [] [] [] []

instance ToJSON Capabilities where
  toJSON (Capabilities is con o hs cfg dlg wp gl ep) = object $ catMaybes
    [ ("installable" .=) <$> is
    , ("hipchatApiConsumer" .=) <$> con
    , ("oauth2Provider" .=) <$> o
    , ("webhook" .=) <$> excludeEmptyList hs
    , ("configurable" .=) <$> cfg
    , ("dialog" .=) <$> excludeEmptyList dlg
    , ("webPanel" .=) <$> excludeEmptyList wp
    , ("glance" .=) <$> excludeEmptyList gl
    , ("externalPage" .=) <$> excludeEmptyList ep
    ]

excludeEmptyList :: [a] -> Maybe [a]
excludeEmptyList [] = Nothing
excludeEmptyList xs = Just xs

instance FromJSON Capabilities where
  parseJSON = withObject "object" $ \o -> Capabilities
    <$> o .:? "installable"
    <*> o .:? "hipchatApiConsumer"
    <*> o .:? "oauth2Provider"
    <*> o .:? "webhooks" .!= []
    <*> o .:? "configurable"
    <*> o .:? "dialog" .!= []
    <*> o .:? "webPanel" .!= []
    <*> o .:? "glance" .!= []
    <*> o .:? "externalPage" .!= []

--------------------------------------------------------------------------------


data CapabilitiesLinks = CapabilitiesLinks
  { clHomepage :: Maybe Text
  , clSelf     :: Text
  } deriving (Generic, Eq, Show)

defaultCapabilitiesLinks :: Text -> CapabilitiesLinks
defaultCapabilitiesLinks = CapabilitiesLinks Nothing

instance ToJSON CapabilitiesLinks where
  toJSON = genericToJSON (aesonPrefix camelCase){omitNothingFields = True}

instance FromJSON CapabilitiesLinks where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

--------------------------------------------------------------------------------

-- Vendor

data Vendor = Vendor
  { vendorUrl  :: Text
  , vendorName :: Text
  } deriving (Generic, Show, Eq)

instance ToJSON Vendor where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Vendor where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

--------------------------------------------------------------------------------

data CapabilitiesDescriptor = CapabilitiesDescriptor
  { capabilitiesDescriptorApiVersion   :: Maybe Text
  , capabilitiesDescriptorCapabilities :: Maybe Capabilities
  , capabilitiesDescriptorDescription  :: Text
  , capabilitiesDescriptorKey          :: Text
  , capabilitiesDescriptorLinks        :: CapabilitiesLinks
  , capabilitiesDescriptorName         :: Text
  , capabilitiesDescriptorVendor       :: Maybe Vendor
  } deriving (Generic, Show)

capabilitiesDescriptor :: Text -> Text -> CapabilitiesLinks -> Text -> CapabilitiesDescriptor
capabilitiesDescriptor desc key links name = CapabilitiesDescriptor Nothing Nothing desc key links name Nothing

instance ToJSON CapabilitiesDescriptor where
  toJSON = genericToJSON (aesonDrop 22 camelCase){omitNothingFields = True}

instance FromJSON CapabilitiesDescriptor where
  parseJSON = genericParseJSON $ aesonDrop 22 camelCase

--------------------------------------------------------------------------------

data Webhook = Webhook
  { webhookAuthentication :: Maybe HipchatAuth
  , webhookUrl            :: Text
  , webhookPattern        :: Maybe Text
  , webhookKey            :: Maybe Text
  , webhookName           :: Maybe Text
  , webhookEvent          :: RoomEvent
  } deriving (Generic, Show, Eq)

instance ToJSON Webhook where
  toJSON = genericToJSON (aesonPrefix camelCase){omitNothingFields = True}

instance FromJSON Webhook where
  parseJSON = genericParseJSON (aesonPrefix camelCase){omitNothingFields = True}

webhook :: Text -> RoomEvent -> Webhook
webhook url = Webhook Nothing url Nothing Nothing Nothing

--------------------------------------------------------------------------------

data OAuth2Provider = OAuth2Provider
  { oauth2ProviderAuthorizationUrl :: Text
  , oauth2ProviderTokenUrl         :: Text
  } deriving (Generic, Show, Eq)

instance ToJSON OAuth2Provider where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON OAuth2Provider where
  parseJSON = genericParseJSON $ aesonDrop 14 camelCase

--------------------------------------------------------------------------------

data Configurable = Configurable
  { configurableUrl :: Text
  } deriving (Generic, Show, Eq)

instance ToJSON Configurable where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Configurable where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

--------------------------------------------------------------------------------

data Registration = Registration
  { registrationOauthId         :: Text
  , registrationCapabilitiesUrl :: Text
  , registrationRoomId          :: Maybe Int
  , registrationGroupId         :: Int
  , registrationOauthSecret     :: Text
  } deriving (Generic, Show, Eq, Ord)

instance FromJSON Registration where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

instance ToRow Registration where
  toRow (Registration a b c d e) = toRow (a, b, c, d, e)

instance FromRow Registration where
  fromRow = Registration <$> field <*> field <*> field <*> field <*> field

--------------------------------------------------------------------------------

data AddOn = AddOn
  { addOnKey          :: Text
  , addOnName         :: Text
  , addOnDescription  :: Text
  , addOnLinks        :: CapabilitiesLinks
  , addOnCapabilities :: Maybe Capabilities
  , addOnVendor       :: Maybe Vendor
  } deriving (Generic, Show, Eq)

defaultAddOn
  :: Text -- ^ key
  -> Text -- ^ name
  -> Text -- ^ description
  -> CapabilitiesLinks
  -> AddOn
defaultAddOn k n d ls = AddOn k n d ls Nothing Nothing

--------------------------------------------------------------------------------
