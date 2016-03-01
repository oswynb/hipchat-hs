{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Network.Hipchat.Types.TokenResponse where

import           Data.Aeson
import           Data.Text               (Text)
import           GHC.Generics

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
