{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module HipChat.Types.API where

import           Data.Text                              (Text)

import           Servant.API

import           HipChat.Types.Auth
import           HipChat.Types.Common
import           HipChat.Types.Extensions
import           HipChat.Types.RoomAddonUIUpdateRequest
import           HipChat.Types.Rooms
import           HipChat.Types.User

type HipChatAPI = TokenAuth(
       SendMessage
  :<|> SendNotification
  :<|> CreateRoom
  :<|> GetRoomStatistics
  :<|> ViewUser
  :<|> CreateWebhook
  :<|> GetAllMembers
  :<|> RoomAddonUIUpdate
  :<|> GenerateToken)

type GenerateToken =
    "v2" :> "oauth" :> "token"
  :> ReqBody '[FormUrlEncoded] TokenRequest
  :> BasicAuth "oauth" Int
  :> Post '[JSON] TokenResponse

type ViewUser =
    "v2" :> "user"
 :> Capture "id_or_email" Text
 :> Get '[JSON] User

--------------------------------------------------------------------------------
--
-- Rooms

type CreateRoom =
    "v2" :> "room"
 :> ReqBody '[JSON] CreateRoomRequest
 :> Post '[JSON] CreateRoomResponse

type CreateWebhook =
    "v2" :> "room"
 :> Capture "room_id_or_name" IdOrName
 :> "extension"
 :> "webhook"
 :> Capture "key" Text
 :> ReqBody '[JSON] Webhook
 :> Put '[JSON] CreateWebhookResponse

type SendMessage =
    "v2" :> "room"
 :> Capture "room" Text
 :> "message"
 :> ReqBody '[JSON] Message
 :> Post '[JSON] SendMessageResponse

type SendNotification =
    "v2" :> "room"
 :> Capture "room" Text
 :> "notification"
 :> ReqBody '[JSON] SendNotificationRequest
 :> PostNoContent '[JSON] NoContent

type GetRoomStatistics =
    "v2" :> "room"
 :> Capture "room" Text
 :> "statistics"
 :> Get '[JSON] RoomStatistics

type GetAllMembers =
    "v2" :> "room"
  :> Capture "room_id_or_name" IdOrName
  :> "member"
  :> QueryParam "start-index" Int
  :> QueryParam "max-results" Int
  :> Get '[JSON] GetAllMembersResponse

type GetAllRooms =
    "v2" :> "room"
  :> QueryParam "start-index" Int
  :> QueryParam "max-results" Int
  :> QueryParam "include-private" Bool
  :> QueryParam "include-archived" Bool
  :> Get '[JSON] GetAllRoomsResponse

--------------------------------------------------------------------------------

-- Addons

type RoomAddonUIUpdate =
    "v2" :> "addon" :> "ui" :> "room"
  :> Capture "room_id" Int
  :> ReqBody '[JSON] RoomAddonUIUpdateRequest
  :> PostNoContent '[JSON] NoContent
