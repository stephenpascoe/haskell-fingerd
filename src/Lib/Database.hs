{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Lib.Database ( createUsers
                    , insertUser
                    , allUsers
                    , getUserQuery
                    , getUser
                    , createDatabase
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

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery = "SELECT * FROM users WHERE username = ?"


getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where meRow :: UserRow
        meRow = (Null, "bob", "/bin/zsh", "/home/bob", "Bob Dole", "555-123-4567")

instance FromRow User where
  fromRow = User <$> field <*> field <*> field
                 <*> field <*> field <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

type UserRow = (Null, Text, Text, Text, Text, Text)
