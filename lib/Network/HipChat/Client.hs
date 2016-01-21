{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Network.HipChat.Client where

import           Network.HipChat.Types.API

import           Control.Monad.Trans.Either
import           Data.ByteString             (ByteString)
import           Data.Proxy
import           Data.Text                   (Text)

import           Servant.API
import           Servant.Client

import           Network.HipChat.Types.Rooms

hipChatAPI :: Proxy HipChatAPI
hipChatAPI = Proxy

sendMessage :: Text -> Message -> Maybe String -> EitherT ServantError IO SendMessageResponse
getRoomStatistics :: Text -> Maybe String -> EitherT ServantError IO RoomStatistics

sendMessage :<|> getRoomStatistics = client hipChatAPI host
  where
    host = BaseUrl Https "api.hipchat.com" 443
