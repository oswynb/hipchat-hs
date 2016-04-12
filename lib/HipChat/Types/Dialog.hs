{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module HipChat.Types.Dialog where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Text          (Text)
import           GHC.Generics

import           HipChat.Types.Name

data Dialog = Dialog
  { dialogKey     :: Text
  , dialogTitle   :: Name
  , dialogUrl     :: Text
  , dialogOptions :: Maybe DialogOptions
  } deriving (Show, Eq, Generic)

defaultDialog :: Text -> Name -> Text -> Dialog
defaultDialog k t u = Dialog k t u Nothing

instance ToJSON Dialog where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON Dialog where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

data DialogStyle = Normal | Warning
  deriving (Show, Eq)

instance ToJSON DialogStyle where
  toJSON Normal = "normal"
  toJSON Warning = "warning"

instance FromJSON DialogStyle where
  parseJSON (String "normal") = return Normal
  parseJSON (String "warning") = return Warning
  parseJSON (String text) = fail ("Unexpected style string: " ++ (show text))
  parseJSON x = typeMismatch "Invalid style" x

data DialogOptions = DialogOptions
  { dialogoptionsStyle            :: Maybe DialogStyle
  , dialogoptionsPrimaryAction    :: Maybe DialogAction
  , dialogoptionsSecondaryActions :: Maybe [DialogAction]
  , dialogoptionsSize             :: Maybe DialogSize
  , dialogoptionsHint             :: Maybe Name
  , dialogoptionsFilter           :: Maybe DialogFilter
  } deriving (Show, Eq, Generic)

instance ToJSON DialogOptions where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON DialogOptions where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

defaultDialogOptions :: DialogOptions
defaultDialogOptions = DialogOptions Nothing Nothing Nothing Nothing Nothing Nothing

data DialogAction = DialogAction
  { dialogactionName    :: Name
  , dialogactionEnabled :: Bool
  , dialogactionKey     :: Maybe Text
  } deriving (Show, Eq, Generic)

instance ToJSON DialogAction where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON DialogAction where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

data DialogSize = DialogSize 
  { dialogsizeHeight :: Text -- Either 'px' or '%'
  , dialogsizeWidth  :: Text -- Either 'px' or '%'
  } deriving (Show, Eq, Generic)

instance ToJSON DialogSize where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON DialogSize where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

data DialogFilter = DialogFilter
  { dialogfilterPlaceholder :: Name
  } deriving (Show, Eq, Generic)

instance ToJSON DialogFilter where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON DialogFilter where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
