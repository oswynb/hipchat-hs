{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Network.Hipchat.Types.API where

import           Data.Text                           (Text)

import           Servant.API

import           Network.Hipchat.Types.Common
import           Network.Hipchat.Types.Rooms
import           Network.Hipchat.Types.TokenRequest
import           Network.Hipchat.Types.TokenResponse
import           Network.Hipchat.Types.User

type HipchatAPI =
       SendMessage
  :<|> CreateRoom
  :<|> GetRoomStatistics
  :<|> GenerateToken
  :<|> ViewUser
  :<|> CreateWebhook

type SendMessage = TokenAuth (
    "v2" :> "room"
 :> Capture "room" Text
 :> "message"
 :> ReqBody '[JSON] Message
 :> Post '[JSON] SendMessageResponse)

type GetRoomStatistics = TokenAuth (
    "v2" :> "room"
 :> Capture "room" Text
 :> "statistics"
 :> Get '[JSON] RoomStatistics)

type GenerateToken = TokenAuth (
    "v2" :> "oauth" :> "token"
  :> ReqBody '[JSON] TokenRequest
  :> Post '[JSON] TokenResponse)

type ViewUser = TokenAuth (
    "v2" :> "user"
 :> Capture "id_or_email" Text
 :> Get '[JSON] User)

--------------------------------------------------------------------------------
--
-- Rooms

type CreateRoom = TokenAuth (
    "v2" :> "room"
 :> ReqBody '[JSON] CreateRoomRequest
 :> Post '[JSON] CreateRoomResponse)

type CreateWebhook = TokenAuth (
    "v2" :> "room"
 :> Capture "room_id_or_name" IdOrName
 :> "extension"
 :> "webhook"
 :> Capture "key" WebhookKey
 :> ReqBody '[JSON] CreateWebhookRequest
 :> Put '[JSON] CreateWebhookResponse)
