{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HipChat.Types.Auth where

import Control.Monad
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Monoid
import           Data.String
import           Data.Text         (Text)
import           Data.Time
import qualified Data.Text            as T
import           GHC.Generics

--------------------------------------------------------------------------------

-- Scopes

data APIScope
  = AdminGroup
  | AdminRoom
  | ManageRooms
  | SendMessage
  | SendNotification
  | ViewGroup
  | ViewMessages
  deriving Eq

instance Show APIScope where
  show = T.unpack . (^. re apiScope)

apiScope :: Prism' Text APIScope
apiScope = prism' display parse
  where
    display = \case
      AdminGroup       -> "admin_group"
      AdminRoom        -> "admin_room"
      ManageRooms      -> "manage_rooms"
      SendMessage      -> "send_message"
      SendNotification -> "send_notification"
      ViewGroup        -> "view_group"
      ViewMessages     -> "view_messages"
    parse = \case
      "admin_group"       -> return AdminGroup
      "admin_room"        -> return AdminRoom
      "manage_rooms"      -> return ManageRooms
      "send_message"      -> return SendMessage
      "send_notification" -> return SendNotification
      "view_group"        -> return ViewGroup
      "view_messages"     -> return ViewMessages
      s                   -> fail $ "unexpected API scope " <> T.unpack s

newtype ScopeList = ScopeList [APIScope]
  deriving (Show, Eq)

instance ToJSON ScopeList where
  toJSON (ScopeList ts) = String (T.intercalate " " $ map (^. re apiScope) ts)

instance FromJSON ScopeList where
  parseJSON = withText "ScopeList" $ \s -> do
    let rawScopes = T.splitOn " " s
    scopes <- forM rawScopes $ \scope -> case scope ^? apiScope of
      Nothing -> fail "invalid scope"
      Just scope' -> return scope'
    return $ ScopeList scopes

--------------------------------------------------------------------------------

-- HipChat API Consumer

data APIConsumer = APIConsumer
  { apiAvatar   :: Maybe Text
  , apiFromName :: Maybe Text
  , apiScopes   :: ScopeList
  } deriving (Generic, Show, Eq)

defaultAPIConsumer :: APIConsumer
defaultAPIConsumer = APIConsumer Nothing Nothing (ScopeList [SendNotification])

instance ToJSON APIConsumer where
  toJSON = genericToJSON (aesonPrefix camelCase){omitNothingFields = True}

instance FromJSON APIConsumer where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

clientCredentialsRequest :: Text -> Text -> TokenRequest
clientCredentialsRequest user pass =
  TokenRequest (Just user) ClientCredentials Nothing Nothing Nothing Nothing (Just $ ScopeList [AdminRoom]) (Just pass) Nothing Nothing


data AccessToken = AccessToken
  { accessTokenAccessToken :: Text
  , accessTokenExpires     :: UTCTime
  , accessTokenRefresh     :: Text
  } deriving (Generic, Show, Eq)

data GrantType = AuthorizationCode
               | RefreshToken
               | Password
               | ClientCredentials
               | Personal
               | RoomNotification

instance Show GrantType where
  show = show . (^. re grantType)

grantType :: Prism' Text GrantType
grantType = prism' display parse
  where
    display AuthorizationCode = "authorization_code"
    display RefreshToken      = "refresh_token"
    display Password          = "password"
    display ClientCredentials = "client_credentials"
    display Personal          = "personal"
    display RoomNotification  = "room_notification"

    parse "authorization_code" = Just AuthorizationCode
    parse "refresh_token"      = Just RefreshToken
    parse "password"           = Just Password
    parse "client_credentials" = Just ClientCredentials
    parse "personal"           = Just Personal
    parse "room_notification"  = Just RoomNotification
    parse _                    = Nothing

instance ToJSON GrantType where
  toJSON = toJSON . (^. re grantType)

data TokenRequest = TokenRequest
  { trUsername     :: Maybe Text
  , trGrantType    :: GrantType
  , trUserId       :: Maybe Text
  , trCode         :: Maybe Text
  , trClientName   :: Maybe Text
  , trRedirectUri  :: Maybe Text
  , trScope        :: Maybe ScopeList
  , trPassword     :: Maybe Text
  , trGroupId      :: Maybe Text
  , trRefreshToken :: Maybe Text
  } deriving (Generic, Show)

instance ToJSON TokenRequest where
  toJSON = genericToJSON $ aesonPrefix snakeCase

tokenRequest :: GrantType -> TokenRequest
tokenRequest gt = TokenRequest Nothing gt Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data TokenResponse = TokenResponse
  { accessToken  :: Text
  , expiresIn    :: Int
  , groupName    :: Text
  , tokenType    :: Text
  , scope        :: ScopeList
  , groupId      :: Int
  , refreshToken :: Int
  } deriving (Generic, Show)

instance FromJSON TokenResponse where
  parseJSON = genericParseJSON $ aesonDrop 0 snakeCase
