{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Criterion.Types (Config(..))
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Control.Monad (void)

--------------------------------------------------------------------------------
-- Helpers: run external commands, capture output, fail with stderr
--------------------------------------------------------------------------------

runCmd :: FilePath -> [String] -> IO ()
runCmd cmd args = do
  (ec, _out, err) <- readProcessWithExitCode cmd args ""
  case ec of
    ExitSuccess   -> pure ()
    ExitFailure c -> error $
      "Command failed: " ++ show cmd ++ " " ++ unwords args
      ++ "\nexit code=" ++ show c ++ "\n" ++ err

--------------------------------------------------------------------------------
-- PostgreSQL: truncate (clear) + \copy binary
--------------------------------------------------------------------------------

-- TRUNCATE is fast and resets table; RESTART IDENTITY resets SERIAL/IDENTITY.
-- CASCADE helps if there are FK dependencies (optional but often handy).
pgTruncateTable :: String -> String -> IO ()
pgTruncateTable dbName tableName = do
  let sql = "TRUNCATE TABLE " ++ tableName ++ " RESTART IDENTITY CASCADE;"
  runCmd "psql" ["-U","postgres","-d",dbName,"-c",sql]

pgCopyBinary :: FilePath -> String -> String -> IO ()
pgCopyBinary binPath dbName tableName = do
  let binPathSql = map (\c -> if c == '\\' then '/' else c) binPath
      copyCmd    = "\\copy " ++ tableName ++ " FROM '" ++ binPathSql ++ "' BINARY"
  runCmd "psql" ["-U","postgres","-d",dbName,"-c",copyCmd]

pgClearAndImport :: FilePath -> String -> String -> IO ()
pgClearAndImport binPath dbName tableName = do
  pgTruncateTable dbName tableName
  pgCopyBinary    binPath dbName tableName

--------------------------------------------------------------------------------
-- MongoDB: clear collection + mongorestore
--------------------------------------------------------------------------------

-- Clear collection via mongosh (preferred over legacy mongo shell).
-- If your environment uses `mongo` instead of `mongosh`, tell me; I’ll switch.
mongoDropCollection :: String -> String -> IO ()
mongoDropCollection dbName collName = do
  -- Drop the collection; if it doesn't exist, drop() returns false; that's okay.
  let js = "db.getSiblingDB('" ++ dbName ++ "').getCollection('" ++ collName ++ "').drop()"
  runCmd "mongosh" [dbName, "--quiet", "--eval", js]

mongoRestoreBson :: FilePath -> String -> String -> IO ()
mongoRestoreBson bsonPath dbName collName = do
  -- `--drop` will drop the collection before restoring.
  -- If your BSON is a single collection dump, this is usually what you want.
  runCmd "mongorestore" ["--drop","-d",dbName,"-c",collName,bsonPath]

mongoClearAndImport :: FilePath -> String -> String -> IO ()
mongoClearAndImport bsonPath dbName collName = do
  -- 方案A：显式 drop（依赖 mongosh）
  -- mongoDropCollection dbName collName
  -- runCmd "mongorestore" ["-d",dbName,"-c",collName,bsonPath]

  -- 方案B：只用 mongorestore --drop（更简单，通常足够）
  mongoRestoreBson bsonPath dbName collName

--------------------------------------------------------------------------------
-- CLI:
--   stack bench --ba "<pgBin> <pgDb> <pgTable> <mongoBson> <mongoDb> <mongoColl> [criterion args...]"
--------------------------------------------------------------------------------

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  bench-insert-to-db <pgBin> <pgDb> <pgTable> <mongoBson> <mongoDb> <mongoColl> [criterion-args...]"
  putStrLn ""
  putStrLn "Example:"
  putStrLn "  stack bench --ba \"users_bin_synced.bin postgres users users_bson_synced.bson test users --output report_insert.html\""
  putStrLn ""
  putStrLn "Notes:"
  putStrLn "  PostgreSQL uses: TRUNCATE TABLE <table> RESTART IDENTITY CASCADE; then \\copy ... BINARY"
  putStrLn "  MongoDB uses:    mongorestore --drop ..."

main :: IO ()
main = do
  args <- getArgs
  case args of
    (pgBin:pgDb:pgTable:mBson:mDb:mColl:restCriterionArgs) ->
      withArgs restCriterionArgs $ do
        let cfg = defaultConfig
              { timeLimit = 60.0
              , resamples = 1000
              }

        defaultMainWith cfg
          [ bgroup "insert-to-db(clear+import)"
              [ bench "postgres/truncate+copy-binary" $
                  whnfIO (pgClearAndImport pgBin pgDb pgTable)

              , bench "mongodb/drop+mongorestore-bson" $
                  whnfIO (mongoClearAndImport mBson mDb mColl)
              ]
          ]
    _ -> usage