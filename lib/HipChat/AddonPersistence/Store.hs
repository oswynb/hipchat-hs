{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module HipChat.AddonPersistence.Store
  ( Postgres
  , HipChatStore(..)
  , Conn(..)
  , Conf(..)
  ) where

import           Control.Monad
import           Data.List
import           Data.Pool
import           Data.String
import           Data.Text                  (Text)
import           Database.PostgreSQL.Simple

import           HipChat.Types.Auth
import           HipChat.Types.Extensions

data Postgres

safeHead :: [a] -> Maybe a
safeHead = fmap fst . uncons

class Monad m => HipChatStore t m where
  data Conn t :: *
  data Conf t :: *

  -- | Standard database/store setup
  initConnection                   :: Conf t -> m (Conn t)
  destroyConnection                :: Conn t -> m ()
  initStore                        :: Conn t -> m ()

  -- | Hipchat specific
  addInstallation                  :: Conn t -> Registration -> m ()
  removeInstallationById           :: Conn t -> Text -> m ()
  getInstallationById              :: Conn t -> Text -> m (Maybe Registration)
  getInstallationByRoomAndGroup    :: Conn t -> Maybe Int -> Int -> m (Maybe Registration)
  getAllInstallations              :: Conn t -> m [Registration]
  storeToken                       :: Conn t -> Text -> AccessToken -> m ()
  getToken                         :: Conn t -> Text -> m (Maybe AccessToken)

instance HipChatStore Postgres IO where
  data Conn Postgres = PGConn (Pool Connection)
  data Conf Postgres = PGConf ConnectInfo

   -- | 1 stripe, 5 second unused connection lifetime, 4 max connections to DB
  initConnection (PGConf connInfo) = PGConn <$> createPool (connect connInfo) close 1 5 4
  destroyConnection (PGConn pool) = destroyAllResources pool
  initStore (PGConn pool) = withResource pool $ \conn -> void $ execute_ conn hipchatInitSql
  addInstallation (PGConn pool) reg@Registration{..} = withResource pool $ \conn -> case registrationRoomId of
    -- | Global installation
    Nothing -> do
      installation <- getInstallationById (PGConn pool) registrationOauthId
      void $ case installation of
        Nothing ->
          execute conn "INSERT INTO installations VALUES (?, ?, ?, ?, ?)" reg
        Just _ -> execute conn "UPDATE installations SET capabilities_url=?,room_id=?,group_id=?,oauth_secret=? WHERE oauth_id = ?"
          (registrationCapabilitiesUrl, registrationRoomId, registrationGroupId, registrationOauthSecret, registrationOauthId)
    -- | Room installation
    Just _ -> do
      installation <- getInstallationByRoomAndGroup (PGConn pool) registrationRoomId registrationGroupId
      void $ case installation of
        Nothing ->
          execute conn "INSERT INTO installations VALUES (?, ?, ?, ?, ?)" reg
        Just Registration{registrationOauthId=oldId} ->
          execute conn "UPDATE installations SET oauth_id=?,capabilities_url=?,room_id=?,group_id=?,oauth_secret=? WHERE oauth_id = ?"
            (reg :. Only oldId)
  removeInstallationById (PGConn pool) oauth_id       = withResource pool $ \conn -> void $ execute conn
    "DELETE FROM installations WHERE oauth_id = ?" (Only oauth_id)
  getInstallationById (PGConn pool) oauth_id       = withResource pool $ \conn -> safeHead <$> query conn
    "SELECT * FROM installations WHERE oauth_id = ?" (Only oauth_id)
  getInstallationByRoomAndGroup (PGConn pool) room_id group_id = withResource pool $ \conn -> safeHead <$> query conn
    "SELECT * FROM installations WHERE room_id = ? AND group_id = ?" (room_id, group_id)
  getAllInstallations (PGConn pool)                = withResource pool $ \conn -> query_ conn "SELECT * FROM installations"
  storeToken          (PGConn pool) oauth_id token = withResource pool $ \conn -> void $ execute conn
    "INSERT INTO oauth_tokens VALUES (?, ?, ?) ON CONFLICT (oauth_id) DO UPDATE SET token=?, expires=?" (Only oauth_id :. token :. token)
  getToken            (PGConn pool) oauth_id       = withResource pool $ \conn -> safeHead <$> query conn
    "SELECT token, expires FROM oauth_tokens WHERE oauth_id = ?" (Only oauth_id)

hipchatInitSql :: Query
hipchatInitSql = fromString $ unlines
  [ "CREATE TABLE IF NOT EXISTS installations ("
  , "oauth_id         text NOT NULL,"
  , "capabilities_url text NOT NULL,"
  , "room_id          int          ,"
  , "group_id         int  NOT NULL,"
  , "oauth_secret     text NOT NULL,"
  , "PRIMARY KEY(oauth_id)"
  , ");"
  , ""
  , "CREATE TABLE IF NOT EXISTS oauth_tokens ("
  , "oauth_id text        NOT NULL references installations(oauth_id) ON DELETE CASCADE,"
  , "token    text        NOT NULL,"
  , "expires  timestamptz NOT NULL,"
  , "PRIMARY KEY(oauth_id)"
  , ");"
  , ""
  ]
