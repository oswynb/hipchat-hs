{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module HipChat.Client where

import           HipChat.Types.API

import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock
import           Data.Time.Format
import           GHC.Generics
import           Servant.API
import           Servant.Client
import           System.IO

import           HipChat.Types.Common
import           HipChat.Types.Rooms
import           HipChat.Types.TokenRequest
import           HipChat.Types.TokenResponse
import           HipChat.Types.User

hipChatAPI :: Proxy HipChatAPI
hipChatAPI = Proxy

sendMessage       :: Maybe Token -> Text -> Message -> EitherT ServantError IO SendMessageResponse
createRoom        :: Maybe Token -> CreateRoomRequest -> EitherT ServantError IO CreateRoomResponse
getRoomStatistics :: Maybe Token -> Text -> EitherT ServantError IO RoomStatistics
generateToken     :: Maybe Token -> TokenRequest -> EitherT ServantError IO TokenResponse
viewUser          :: Maybe Token -> Text -> EitherT ServantError IO User
createWebhook     :: Maybe Token -> IdOrName -> WebhookKey -> CreateWebhookRequest -> EitherT ServantError IO CreateWebhookResponse
getAllMembers     :: Maybe Token -> IdOrName -> Maybe Int -> Maybe Int -> EitherT ServantError IO GetAllMembersResponse
sendMessage :<|> createRoom :<|> getRoomStatistics :<|> generateToken :<|> viewUser :<|> createWebhook :<|> getAllMembers = client hipChatAPI host
  where
    host = BaseUrl Https "api.hipchat.com" 443
