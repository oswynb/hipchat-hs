{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Network.Hipchat.Client where

import           Network.Hipchat.Types.API

import           Control.Monad.Trans.Either
import           Data.Proxy
import           Data.Text                           (Text)

import           Servant.API
import           Servant.Client

import           Network.Hipchat.Types.Common
import           Network.Hipchat.Types.Rooms
import           Network.Hipchat.Types.TokenRequest
import           Network.Hipchat.Types.TokenResponse
import           Network.Hipchat.Types.User

hipChatAPI :: Proxy HipchatAPI
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
