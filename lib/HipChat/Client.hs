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

(sendMessage :<|> createRoom :<|> getRoomStatistics :<|> viewUser :<|> createWebhook :<|> getAllMembers :<|> roomAddonUIUpdate :<|> generateToken) = client hipChatAPI
