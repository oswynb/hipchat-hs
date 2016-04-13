{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module HipChat.Client where

import           HipChat.Types.API

import           Control.Monad.Trans.Either
import           Data.Proxy
import           Data.Text                   (Text)
import           Servant.API
import           Servant.Client

import           HipChat.Types.Common
import           HipChat.Types.Rooms
import           HipChat.Types.TokenRequest
import           HipChat.Types.TokenResponse
import           HipChat.Types.User

hipChatAPI :: Proxy HipChatAPI
hipChatAPI = Proxy

sendMessage :<|> createRoom :<|> getRoomStatistics :<|> generateToken :<|> viewUser :<|> createWebhook :<|> getAllMembers = client hipChatAPI
  where
    host = BaseUrl Https "api.hipchat.com" 443
