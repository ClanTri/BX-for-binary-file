{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MongoDB
import Control.Monad (forM_)
import Control.Exception (bracket)

main :: IO ()
main = bracket
  (connect (host "127.0.0.1"))  -- acquire
  close                         -- release
  (\pipe -> do
      docs <- access pipe master "test" findAllUsers
      forM_ docs printUser
  )

findAllUsers :: Action IO [Document]
findAllUsers = do
  cursor <- find (select [] "users")
  docs <- rest cursor
  closeCursor cursor
  return docs

printUser :: Document -> IO ()
printUser doc = do
  let uid    = at "_id" doc :: Int
      name   = at "name" doc :: String
      age    = at "age" doc :: Int
      gender = at "gender" doc :: String
  putStrLn $ "ID: " ++ show uid ++
             ", Name: " ++ name ++
             ", Age: " ++ show age ++
             ", Gender: " ++ gender