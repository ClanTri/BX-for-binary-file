{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment (getArgs)
import System.Exit        (ExitCode(..), die)
import System.Process     (rawSystem)

------------------------------------------------------------
-- Helpers
------------------------------------------------------------

runOrDie :: String -> [String] -> String -> IO ()
runOrDie cmd args okMsg = do
  exitCode <- rawSystem cmd args
  case exitCode of
    ExitSuccess   -> putStrLn okMsg
    ExitFailure c -> die $ cmd ++ " failed with exit code " ++ show c

fixWinPathForPsql :: FilePath -> FilePath
fixWinPathForPsql = map (\c -> if c == '\\' then '/' else c)

------------------------------------------------------------
-- PostgreSQL: import COPY BINARY file via \copy
------------------------------------------------------------

importPgBinViaPsql :: FilePath -> String -> String -> IO ()
importPgBinViaPsql binPath dbName tableName = do
  let binPathSql = fixWinPathForPsql binPath
      copyCmd    = "\\copy " ++ tableName
                ++ " FROM '" ++ binPathSql ++ "' BINARY"
  putStrLn $ "Running: psql -U postgres -d " ++ dbName ++ " -c \"" ++ copyCmd ++ "\""
  runOrDie "psql" ["-U","postgres","-d",dbName,"-c",copyCmd]
    "PostgreSQL import (bin via \\copy) finished successfully."

------------------------------------------------------------
-- PostgreSQL: import XML via psql + user-provided SQL script
-- Same idea as JSON: mapping/parsing logic lives in SQL file.
------------------------------------------------------------

importPgXmlViaSql :: FilePath -> FilePath -> String -> IO ()
importPgXmlViaSql xmlPath sqlPath dbName = do
  let xmlPathSql = fixWinPathForPsql xmlPath
      sqlPathSql = fixWinPathForPsql sqlPath
      args =
        [ "-U","postgres"
        , "-d",dbName
        , "-v","ON_ERROR_STOP=1"
        , "-v", "xmlfile=" ++ xmlPathSql
        , "-f", sqlPathSql
        ]
  putStrLn $ "Running: psql -U postgres -d " ++ dbName ++ " -v xmlfile=" ++ xmlPathSql ++ " -f " ++ sqlPathSql
  runOrDie "psql" args "PostgreSQL import (XML via SQL script) finished successfully."

------------------------------------------------------------
-- MongoDB: import BSON file via mongorestore
------------------------------------------------------------

importMongoBsonViaMongoRestore
  :: FilePath
  -> String
  -> String
  -> IO ()
importMongoBsonViaMongoRestore bsonPath dbName collName = do
  putStrLn $ "Running: mongorestore --drop -d " ++ dbName ++ " -c " ++ collName ++ " " ++ bsonPath
  runOrDie "mongorestore"
    [ "--drop"
    , "-d", dbName
    , "-c", collName
    , bsonPath
    ]
    "MongoDB import (BSON via mongorestore) finished successfully."


------------------------------------------------------------
-- MongoDB: import JSON via mongoimport
--
-- Assumes the JSON file is either:
--   - one JSON document per line (newline-delimited JSON), OR
--   - a JSON array (use --jsonArray)
------------------------------------------------------------

importMongoJsonViaMongoImport :: FilePath -> String -> String -> Bool -> IO ()
importMongoJsonViaMongoImport jsonPath dbName collName isJsonArray = do
  let baseArgs =
        [ "-d", dbName
        , "-c", collName
        , "--file", jsonPath
        , "--drop"
        ]
      args = if isJsonArray then baseArgs ++ ["--jsonArray"] else baseArgs
  putStrLn $ "Running: mongoimport -d " ++ dbName ++ " -c " ++ collName ++ " --file " ++ jsonPath
           ++ (if isJsonArray then " --jsonArray" else "")
           ++ " --drop"
  runOrDie "mongoimport" args "MongoDB import (JSON via mongoimport) finished successfully."

------------------------------------------------------------
-- CLI
------------------------------------------------------------

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn ""
  putStrLn "PostgreSQL:"
  putStrLn "  insert_to_db import-pg-bin  <binFile>  <dbName> <tableName>"
  putStrLn "  insert_to_db import-pg-json <jsonFile> <sqlFile> <dbName>"
  putStrLn "  insert_to_db import-pg-xml  <xmlFile>  <sqlFile> <dbName>"
  putStrLn ""
  putStrLn "MongoDB:"
  putStrLn "  insert_to_db import-mongo-bson <bsonFile> <dbName> <collection>   (uses --drop)"
  putStrLn "  insert_to_db import-mongo-json <jsonFile> <dbName> <collection> [--jsonArray]"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  insert_to_db import-pg-bin users_bin_synced.bin postgres users"
  putStrLn "  insert_to_db import-mongo-bson users_bson_synced.bson test users"
  putStrLn "  insert_to_db import-mongo-json users_json_synced.json test users --jsonArray"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["import-pg-bin", binPath, dbName, tableName] ->
      importPgBinViaPsql binPath dbName tableName

    ["import-pg-xml", xmlPath, sqlPath, dbName] ->
      importPgXmlViaSql xmlPath sqlPath dbName

    ["import-mongo-bson", bsonPath, dbName, collName] ->
      importMongoBsonViaMongoRestore bsonPath dbName collName

    ["import-mongo-json", jsonPath, dbName, collName] ->
      importMongoJsonViaMongoImport jsonPath dbName collName False

    ["import-mongo-json", jsonPath, dbName, collName, "--jsonArray"] ->
      importMongoJsonViaMongoImport jsonPath dbName collName True

    _ -> usage
