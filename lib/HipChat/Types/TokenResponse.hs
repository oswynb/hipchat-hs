{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module HipChat.Types.TokenResponse where

import           Data.Aeson
import           Data.Text            (Text)
import           GHC.Generics

import           HipChat.Types.Common

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
  parseJSON = snakeParseJSON 0
