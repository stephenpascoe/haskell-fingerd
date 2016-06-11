{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Database ( createUsers
                    , allUsersQuery
                    , getUserQuery
                    , getUser
                    , createDatabase
                    , connectDb
                    , User(..)
                    ) where

import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Text.RawString.QQ
import Data.Typeable

import Database.SQLite.Simple.Types
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite


data User = User { userId :: Integer
                 , username :: Text
                 , shell :: Text
                 , homeDirectory :: Text
                 , realName :: Text
                 , phone :: Text
                 } deriving (Eq, Show)

{-
-- Exceptions
-}

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData


createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT UNIQUE,
  shell TEXT,
  homeDirectory  TEXT,
  realName TEXT,
  phone TEXT
)
|]

insertUserQuery :: Query
insertUserQuery = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsersQuery :: Query
allUsersQuery = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * FROM users WHERE username = ?"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData


connectDb :: IO Connection
connectDb = open "finger.db"

createDatabase :: IO ()
createDatabase = do
  conn <- connectDb
  execute_ conn createUsers
  addUser conn me
  rows <- query_ conn allUsersQuery
  mapM_ print (rows :: [User])
  SQLite.close conn
  where me = User { userId = 0
                  , username = "stampy"
                  , shell = "/bin/zsh"
                  , homeDirectory = "/home/bob"
                  , realName = "Stampy Longnose"
                  , phone = "555-123-4567"
                  }

addUser :: Connection -> User -> IO ()
addUser conn user = do
  execute conn insertUserQuery $ newUserRow user

instance FromRow User where
  fromRow = User <$> field <*> field <*> field
                 <*> field <*> field <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

type UserRow = (Null, Text, Text, Text, Text, Text)

newUserRow :: User -> UserRow
newUserRow User{..} = (Null, username, shell, homeDirectory, realName, phone)
