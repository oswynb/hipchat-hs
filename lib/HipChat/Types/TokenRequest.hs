{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module HipChat.Types.TokenRequest where

import           Control.Lens
import           Data.Aeson
import           Data.Text            (Text)
import           GHC.Generics

import           HipChat.Types.Common

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
  { tokenRequestUsername     :: Maybe Text
  , tokenRequestGrantType    :: GrantType
  , tokenRequestUserId       :: Maybe Text
  , tokenRequestCode         :: Maybe Text
  , tokenRequestClientName   :: Maybe Text
  , tokenRequestRedirectUri  :: Maybe Text
  , tokenRequestScope        :: Maybe Text
  , tokenRequestPassword     :: Maybe Text
  , tokenRequestGroupId      :: Maybe Text
  , tokenRequestRefreshToken :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON TokenRequest where
  toJSON = snakeToJSON (length ("tokenRequest" :: String))

tokenRequest :: GrantType -> TokenRequest
tokenRequest gt = TokenRequest Nothing gt Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
