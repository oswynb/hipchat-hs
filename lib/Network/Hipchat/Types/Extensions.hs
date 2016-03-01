{-# LANGUAGE DeriveGeneric #-}

module Network.Hipchat.Types.Extensions where

import Data.Aeson
import Data.Aeson.Types
import Data.Char
import GHC.Generics
import           Data.Text (Text)

data Capabilities = Capabilities
  {

  } deriving (Generic, Show)

instance ToJSON Capabilities where
  toJSON = genericToJSON defaultOptions{fieldLabelModifier = \x -> let (y:ys) = drop 1 x in toLower y:ys}


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