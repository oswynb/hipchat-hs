{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module HipChat.Types.TokenRequest where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text            (Text)
import           GHC.Generics

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
  { trUsername     :: Maybe Text
  , trGrantType    :: GrantType
  , trUserId       :: Maybe Text
  , trCode         :: Maybe Text
  , trClientName   :: Maybe Text
  , trRedirectUri  :: Maybe Text
  , trScope        :: Maybe Text
  , trPassword     :: Maybe Text
  , trGroupId      :: Maybe Text
  , trRefreshToken :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON TokenRequest where
  toJSON = genericToJSON $ aesonPrefix snakeCase

tokenRequest :: GrantType -> TokenRequest
tokenRequest gt = TokenRequest Nothing gt Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
