{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MongoDB
import Control.Monad.Trans (liftIO)
import Control.Exception (try, IOException)

main :: IO ()
main = do
    result <- tryConnectMongoDB
    case result of
        Left err -> putStrLn $ "Failed to connect: " ++ show err
        Right _  -> putStrLn "Successfully connected to MongoDB"

tryConnectMongoDB :: IO (Either IOException Pipe)
tryConnectMongoDB = try $ connect (host "127.0.0.1")
