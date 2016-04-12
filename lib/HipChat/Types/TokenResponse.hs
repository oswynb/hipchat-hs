{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module HipChat.Types.TokenResponse where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text            (Text)
import           GHC.Generics

data TokenResponse = TokenResponse
  { accessToken  :: Text
  , expiresIn    :: Int
  , groupName    :: Text
  , tokenType    :: Text
  , scope        :: Text
  , groupId      :: Int
  , refreshToken :: Int
  } deriving (Generic, Show)

instance FromJSON TokenResponse where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase
