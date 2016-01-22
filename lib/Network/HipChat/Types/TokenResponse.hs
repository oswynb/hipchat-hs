{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Network.HipChat.Types.TokenResponse where

import           Data.Aeson
import           Data.String
import           Data.String.Conversions
import           Data.Text               (Text)
import           GHC.Generics

import           Servant.API

data TokenResponse = TokenResponse
  { access_token  :: Text
  , expires_in    :: Int
  , group_name    :: Text
  , token_type    :: Text
  , scope         :: Text
  , group_id      :: Int
  , refresh_token :: Int
  } deriving (Generic, Show)

instance FromJSON TokenResponse
