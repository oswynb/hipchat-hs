{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module HipChat.Client where

import           Data.Proxy
import           Servant.API
import           Servant.Client

import           HipChat.Types.API

hipChatAPI :: Proxy HipChatAPI
hipChatAPI = Proxy

sendMessage :<|> createRoom :<|> getRoomStatistics :<|> generateToken :<|> viewUser :<|> createWebhook :<|> getAllMembers :<|> roomAddonUIUpdate = client hipChatAPI
  where
    host = BaseUrl Https "api.hipchat.com" 443
