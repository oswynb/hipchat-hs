{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}

module Network.Hipchat.Client where

import           Network.Hipchat.Types.API

import           Control.Monad.Trans.Either
import           Data.Proxy
import           Data.Text                           (Text)
import Data.Aeson
import           Servant.API
import GHC.Generics
import           Servant.Client
import qualified Data.Text as T
import System.IO
import Data.Time.Clock
import Data.Monoid
import Data.Time.Format

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


shame :: Text -> Maybe Token -> IO ()
shame id_or_email token = do
    user <- getShame id_or_email token
    putStrLn $ T.unpack user
    shameResp <- runEitherT $ lookupShame user
    case shameResp of
        Left err -> print $ show err
        Right DirectoryResponse{..} -> do
--            currentTime <- getCurrentTime
--            let seconds = diffUTCTime currentTime (unDirectoryTime drHireDate)
--            let days = floor $ seconds / 60 / 60 / 24 :: Int
   --         let (Manager mName mUsername) = head drManagers
--            let shameMessage = full_name <> " used @_here, " <>
--                    if days < 90 then
--                        " but has only been here for " <> T.pack (show days) <> " days, lets be nice."
--                    else
--                        " and has been here for " <> T.pack (show days) <> " days, SHAME!\n" <>
--                        "Maybe we should tell their manager, " <> mName <> "..."
            -- putStrLn $ T.unpack shameMessage
            --result <- runEitherT $ sendMessage "Build Engineering - Private" (Message shameMessage) token
            result <- runEitherT $ sendMessage token "top secrat" (Message $ drFullName <> ": " <> drTenure)
            case result of
                Left err -> print $ show err
                Right resp -> print resp

getShame :: Text -> Maybe Token -> IO Text
getShame id_or_email token = do
    result <- runEitherT $ viewUser token id_or_email
    case result of
        Left err -> return "" -- $ fromString $ show err
        Right User{..} -> case userEmail of
            Nothing -> return "no email =("
            Just email -> return $ head (T.splitOn "@" email)

type Directory = "api" :> "staffData" :> Capture "staff_name" Text :> Get '[JSON] DirectoryResponse

directory :: Proxy Directory
directory = Proxy

lookupShame = client directory host
  where
    host = BaseUrl Https "directory.atlassian.io" 443
data Manager = Manager Text Text deriving (Show)

instance FromJSON Manager where
    parseJSON (Object v) = Manager <$> v .: "full_name" <*> v .: "username"

data DirectoryResponse = DirectoryResponse
  { drFullName  :: Text
  --, drHireDate  :: DirectoryTime
  , drTenure     :: Text
  , drManagers   :: [Manager]
--  , drBlogPosts :: [()]
  } deriving (Generic, Show)

instance FromJSON DirectoryResponse where
  parseJSON = genericHipchatParseJSON 2

newtype DirectoryTime = DirectoryTime
  { unDirectoryTime :: UTCTime
  } deriving Show

instance FromJSON DirectoryTime where
    parseJSON (String x) = do
        utcTime <- parseTimeM False defaultTimeLocale "%F %X" (T.unpack x)
        return $ DirectoryTime utcTime