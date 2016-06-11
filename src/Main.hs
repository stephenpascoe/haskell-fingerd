{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ
import Lib.Database

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsersQuery
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat [
  "Login: ", e username, "\t\t\t\t",
  "Name: ", e realName, "\n",
  "Directory: ", e homeDir, "\t\t\t",
  "Shell: ", e shell, "\n"
  ]
  where e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do putStrLn ("Couldn't find matching user for username: " ++
                            (show username))
                  return ()
    Just user -> sendAll soc (formatUser user)


handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name -> returnUser dbConn soc (decodeUtf8 name)


handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Get connection, handling query"
  handleQuery dbConn soc
  sClose soc



main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                           Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
          Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- connectDb
  handleQueries conn sock
  SQLite.close conn
  sClose sock
