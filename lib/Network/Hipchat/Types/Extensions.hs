{-# LANGUAGE DeriveGeneric #-}

module Network.Hipchat.Types.Extensions where

import Data.Aeson
import Data.Aeson.Types
import Data.Char
import GHC.Generics
import           Data.Text (Text)

import Network.Hipchat.Types.Common

type AddonAction = ()

data CapabilitiesAdminPage = CapabilitiesAdminPage
  { capUrl :: Text
  } deriving (Generic, Show)

instance ToJSON CapabilitiesAdminPage where
  toJSON = genericHipchatToJSON 3

data CapabilitiesInstallable = CapabilitiesInstallable
  { ciAllowGlobal :: Maybe Bool
  , ciAllowRoom   :: Maybe Bool

  } deriving (Generic, Show)

instance ToJSON CapabilitiesInstallable

data Capabilities = Capabilities
  { cAction    :: Maybe [AddonAction]
  , cAdminPage :: Maybe CapabilitiesAdminPage
  , cConfigurable :: Maybe ()
  , cDialog :: Maybe [()]
  , cExternalPage :: Maybe [()]
  , cGlance :: Maybe [()]
  , cHipchatApiConsumer :: Maybe ()
  , cInstallable :: Maybe CapabilitiesInstallable
  } deriving (Generic, Show)

instance ToJSON Capabilities where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 1 x in toLower y:ys, omitNothingFields = True}


data CapabilitiesLinks = CapabilitiesLinks
  { clHomepage :: Maybe Text
  , clSelf :: Text
  } deriving (Generic, Show)

capabilitiesLinks :: Text -> CapabilitiesLinks
capabilitiesLinks self = CapabilitiesLinks Nothing self

instance ToJSON CapabilitiesLinks where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys}

data CapabilitiesVendor = CapabilitiesVendor
  {

  } deriving (Generic, Show)

instance ToJSON CapabilitiesVendor where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys}

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
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 2 x in toLower y:ys}