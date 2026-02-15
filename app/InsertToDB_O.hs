{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit        (ExitCode(..), die)
import System.Process
import qualified Data.ByteString as BS
import System.IO

------------------------------------------------------------
-- PostgreSQL: import COPY BINARY file via \copy
------------------------------------------------------------

importPgBinViaPsql
  :: FilePath  -- ^ bin file path
  -> String    -- ^ database name
  -> String    -- ^ table name
  -> IO ()
importPgBinViaPsql binPath dbName tableName = do
  let binPathSql = map (\c -> if c == '\\' then '/' else c) binPath
      copyCmd    = "\\copy " ++ tableName
                ++ " FROM '" ++ binPathSql ++ "' BINARY"

  putStrLn $ "Running: psql -U postgres -d " ++ dbName ++ " -c \"" ++ copyCmd ++ "\""

  exitCode <- rawSystem "psql"
    [ "-U", "postgres"
    , "-d", dbName
    , "-c", copyCmd
    ]

  case exitCode of
    ExitSuccess ->
      putStrLn "PostgreSQL import (bin via \\copy) finished successfully."
    ExitFailure c ->
      die $ "psql \\copy failed with exit code " ++ show c

------------------------------------------------------------
-- MongoDB: import BSON file via mongorestore
------------------------------------------------------------

importMongoBsonViaMongoRestore
  :: FilePath  -- ^ bson file path (e.g. users_bson_synced.bson)
  -> String    -- ^ database name (e.g. "demo")
  -> String    -- ^ collection name (e.g. "dcoll")
  -> IO ()
importMongoBsonViaMongoRestore bsonPath dbName collName = do
  putStrLn $ "Running: mongorestore -d " ++ dbName ++ " -c " ++ collName ++ " " ++ bsonPath
  exitCode <- rawSystem "mongorestore"
    [ "-d", dbName
    , "-c", collName
    , bsonPath
    ]
  case exitCode of
    ExitSuccess   ->
      putStrLn "MongoDB import (BSON via mongorestore) finished successfully."
    ExitFailure c ->
      die $ "mongorestore failed with exit code " ++ show c

------------------------------------------------------------
-- CLI
------------------------------------------------------------

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn ""
  putStrLn "  Import PostgreSQL bin (COPY BINARY format):"
  putStrLn "    insert_to_db import-pg-bin <binFile> <dbName> <tableName>"
  putStrLn ""
  putStrLn "  Import MongoDB BSON via mongorestore:"
  putStrLn "    insert_to_db import-mongo-bson <bsonFile> <dbName> <collection>"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  insert_to_db import-pg-bin users_bin_synced.bin postgres users"
  putStrLn "  insert_to_db import-mongo-bson users_bson_synced.bson demo dcoll"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["import-pg-bin", binPath, dbName, tableName] ->
      importPgBinViaPsql binPath dbName tableName

    ["import-mongo-bson", bsonPath, dbName, collName] ->
      importMongoBsonViaMongoRestore bsonPath dbName collName

    _ -> usage