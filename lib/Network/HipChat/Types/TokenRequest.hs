{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Network.HipChat.Types.TokenRequest where

import           Control.Lens
import           Data.Aeson
import           Data.String
import           Data.String.Conversions
import           Data.Text               (Text)
import qualified Data.Text               as T
import           GHC.Generics

import           Servant.API

data GrantType = AuthorizationCode
               | RefreshToken
               | Password
               | ClientCredentials
               | Personal
               | RoomNotification

instance Show GrantType where
  show = show . (^. re grantType)

grantType :: Prism' Text GrantType
grantType = prism' display parse
  where
    display AuthorizationCode = "authorization_code"
    display RefreshToken      = "refresh_token"
    display Password          = "password"
    display ClientCredentials = "client_credentials"
    display Personal          = "personal"
    display RoomNotification  = "room_notification"

    parse "authorization_code" = Just AuthorizationCode
    parse "refresh_token"      = Just RefreshToken
    parse "password"           = Just Password
    parse "client_credentials" = Just ClientCredentials
    parse "personal"           = Just Personal
    parse "room_notification"  = Just RoomNotification
    parse _                    = Nothing

instance ToJSON GrantType where
  toJSON = toJSON . (^. re grantType)

data TokenRequest = TokenRequest
  { username      :: Maybe Text
  , grant_type    :: GrantType
  , user_id       :: Maybe Text
  , code          :: Maybe Text
  , client_name   :: Maybe Text
  , redirect_uri  :: Maybe Text
  , scope         :: Maybe Text
  , password      :: Maybe Text
  , group_id      :: Maybe Text
  , refresh_token :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON TokenRequest

tokenRequest :: GrantType -> TokenRequest
tokenRequest grant_type = TokenRequest Nothing grant_type Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
