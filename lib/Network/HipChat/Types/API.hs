{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Network.HipChat.Types.API where

import           Data.Text                   (Text)

import           Servant.API

import           Network.HipChat.Types.Rooms

type TokenAuth = QueryParam "auth_token" String

type HipChatAPI =
       SendMessage
  :<|> GetRoomStatistics

type SendMessage =
    "v2" :> "room" :> Capture "room" Text :> "message"
 :> ReqBody '[JSON] Message
 :> TokenAuth
 :> Post '[JSON] SendMessageResponse


type GetRoomStatistics =
    "v2" :> "room" :> Capture "room" Text :> "statistics"
 :> TokenAuth
 :> Get '[JSON] RoomStatistics
