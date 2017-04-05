{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module HipChat.Types.Auth where

import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Time
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Web.FormUrlEncoded
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

instance ToJSON APIScope where
  toJSON = String . (^. re apiScope)

instance FromJSON APIScope where
  parseJSON = withText "Scope" $ \t -> case t ^? apiScope of
    Nothing -> fail (T.unpack t)
    Just s  -> return s

apiScope :: Prism' Text APIScope
apiScope = prism' enc dec
  where
    enc = \case
      AdminGroup       -> "admin_group"
      AdminRoom        -> "admin_room"
      ManageRooms      -> "manage_rooms"
      SendMessage      -> "send_message"
      SendNotification -> "send_notification"
      ViewGroup        -> "view_group"
      ViewMessages     -> "view_messages"
    dec = \case
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
  , apiScopes   :: [APIScope]
  } deriving (Generic, Show, Eq)

defaultAPIConsumer :: APIConsumer
defaultAPIConsumer = APIConsumer Nothing Nothing [SendNotification]

adminConsumer :: APIConsumer
adminConsumer = APIConsumer Nothing Nothing [AdminRoom]

instance ToJSON APIConsumer where
  toJSON = genericToJSON (aesonPrefix camelCase){omitNothingFields = True}

instance FromJSON APIConsumer where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

clientCredentialsRequest :: Maybe Text -> TokenRequest
clientCredentialsRequest user =
  TokenRequest user ClientCredentials Nothing Nothing Nothing Nothing (Just $ ScopeList [SendNotification]) Nothing Nothing Nothing

data AccessToken = AccessToken
  { accessTokenAccessToken :: Text
  , accessTokenExpires     :: UTCTime
  } deriving (Generic, Show, Eq)

instance FromRow AccessToken where
  fromRow = AccessToken <$> field <*> field

instance ToRow AccessToken where
  toRow (AccessToken a b) = toRow (a, b)

data GrantType = AuthorizationCode
               | RefreshToken
               | Password
               | ClientCredentials
               | Personal
               | RoomNotification

instance Show GrantType where
  show = show . (^. re grantType)

grantType :: Prism' Text GrantType
grantType = prism' enc dec
  where
    enc = \case
      AuthorizationCode -> "authorization_code"
      RefreshToken      -> "refresh_token"
      Password          -> "password"
      ClientCredentials -> "client_credentials"
      Personal          -> "personal"
      RoomNotification  -> "room_notification"
    dec = \case
      "authorization_code" -> Just AuthorizationCode
      "refresh_token"      -> Just RefreshToken
      "password"           -> Just Password
      "client_credentials" -> Just ClientCredentials
      "personal"           -> Just Personal
      "room_notification"  -> Just RoomNotification
      _                    -> Nothing

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
  toJSON = genericToJSON $ (aesonPrefix snakeCase){omitNothingFields=True}

instance ToForm TokenRequest where
  toForm tr =
    let (Object obj) = toJSON tr
    in Form $ fmap (\(String t) -> [t]) obj

tokenRequest :: GrantType -> TokenRequest
tokenRequest gt = TokenRequest Nothing gt Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data TokenResponse = TokenResponse
  { trsAccessToken  :: Text
  , trsExpiresIn    :: Int
  , trsGroupName    :: Text
  , trsTokenType    :: Text
  , trsScope        :: ScopeList
  , trsGroupId      :: Int
  , trsRefreshToken :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON TokenResponse where
  parseJSON = genericParseJSON $ aesonPrefix snakeCase
