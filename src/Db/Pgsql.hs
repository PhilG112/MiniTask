{-# LANGUAGE DeriveAnyClass #-}

module Db.Pgsql (getTest, insertTest) where

import Api.Apis (RegisterRequest)
import Data.Int (Int64)
import Database.PostgreSQL.Simple
    ( Only (Only),
      connectPostgreSQL,
      execute,
      query_,
    )

getTest :: IO Int
getTest = do
    conn <- connectPostgreSQL "connString"
    [Only i] <- query_ conn "select 2 + 2"
    return i

insertTest :: RegisterRequest -> IO Int64
insertTest req = do
    conn <- connectPostgreSQL "connString"
    numRows <-
        execute conn "INSERT INTO Users (email, username, password) VALUES (?,?,?)" req
    return numRows