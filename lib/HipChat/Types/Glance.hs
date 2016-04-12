{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HipChat.Types.Glance where

-- https://www.hipchat.com/docs/apiv2/glances

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Char
import           Data.Text          (Text)
import           GHC.Generics

import           HipChat.Types.Icon
import           HipChat.Types.Key
import           HipChat.Types.Name

-- TODO make compound
data GlanceTarget = GlanceKey Text -- ^ The key of a dialog, glance or web panel that should  be opened in response to this action. Valid length range: 1 - 40.
  deriving (Eq, Generic, Show)

instance ToJSON GlanceTarget where
  toJSON (GlanceKey x) = String x

instance FromJSON GlanceTarget where
  parseJSON (String x) = pure (GlanceKey x)
  parseJSON x = typeMismatch "GlanceTarget" x

data Glance = Glance
  { glanceIcon     :: Icon               -- ^ Icon to display on the left side of the glance.
  , glanceKey      :: Key                -- ^ Unique key (in the context of the integration) to identify this glance. Valid length range: 1 - 40.
  , glanceName     :: Name               -- ^ The display name of the glance.
  , glanceQueryUrl :: Maybe Text         -- ^ The URL of the resource providing the glance content.
  , glanceTarget   :: Maybe GlanceTarget -- ^ Defines the behaviour when clicking on the glance.
  , glanceWeight   :: Maybe Integer      -- ^ Determines the order in which glances appear. Glances are displayed top to bottom in order of ascending weight. Defaults to 100.
  } deriving (Eq, Generic, Show)

defaultGlance :: Glance
defaultGlance =
  let uri  = "https://wiki.haskell.org/wikiupload/4/4a/HaskellLogoStyPreview-1.png"
      icon = Icon uri uri
      name = Name Nothing "Lambdabot"
  in Glance icon "hipbot.glance" name Nothing Nothing Nothing

instance ToJSON Glance where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Glance where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

data GlanceData = GlanceData

data GlanceConditions = GlanceConditions
