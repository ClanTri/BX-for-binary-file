{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (replicateM)
import           Data.Int               (Int32)
import qualified Data.Text              as T
import           Database.MongoDB
import           System.Environment     (getArgs)
import           System.Random          (randomRIO)
import           Text.Read              (readMaybe)

-- split into batches
chunk :: Int -> [a] -> [[a]]
chunk k xs
  | k <= 0    = error "chunk size must be positive"
  | null xs   = []
  | otherwise = take k xs : chunk k (drop k xs)

-- a pool of names
namePool :: [T.Text]
namePool =
  [ "Alice","Bob","Carol","Dave","Eve","Frank","Grace","Heidi","Ivan","Judy"
  , "Mallory","Niaj","Olivia","Peggy","Rupert","Sybil","Trent","Victor","Wendy","Yvonne"
  , "Haruto","Yui","Ren","Aoi","Sota","Hina","Itsuki","Rio","Takumi","Sakura"
  , "Wei","Ming","Hua","Li Hua","Zhang Wei","Wang Fang","Chen Jie","Liu Yang","Zhao Min","Sun Lei"
  ]

randName :: IO T.Text
randName = do
  i <- randomRIO (0, length namePool - 1)
  pure (namePool !! i)

randAge :: IO Int32
randAge = fromIntegral <$> randomRIO (16, 55 :: Int)

-- build one document
mkDoc :: Int32 -> T.Text -> IO Document
mkDoc newId nm = do
  a <- randAge
  nm <- randName
  pure [ "_id" =: newId
       , "name" =: nm
       , "age"  =: a
       ]

-- find current max _id
currentMaxId :: Pipe -> Database -> Collection -> IO Int32
currentMaxId pipe db coll = access pipe master db $ do
  cur <- find (select [] coll){ sort = ["_id" =: (-1 :: Int32)], limit = 1 }
  docs <- rest cur
  pure $ case docs of
    (d:_) -> maybe 0 id (Database.MongoDB.lookup "_id" d :: Maybe Int32)
    _     -> 0

-- insert N docs
insertN :: Pipe -> Database -> Collection -> Int -> IO Int
insertN pipe db coll n = do
  startId <- currentMaxId pipe db coll
  let ids   = [startId + 1 .. startId + fromIntegral n]
      names = take n (cycle namePool)
  docs <- mapM (uncurry mkDoc) (zip ids names)
  access pipe master db $ do
    let batches = chunk 1000 docs
    counts <- mapM (fmap length . insertMany coll) batches
    pure (sum counts)

main :: IO ()
main = do
  args <- getArgs
  let n = case args of
            (x:_) -> maybe 100 id (readMaybe x)  -- 默认 100
            _     -> 100

  pipe <- connect (host "127.0.0.1")
  let db   = "test"  :: Database
      coll = "users" :: Collection

  inserted <- insertN pipe db coll n

  -- print total
  total <- access pipe master db $ count (select [] coll)

  -- print latest 5 entries
  newest <- access pipe master db $ do
    cur <- find (select [] coll){ sort = ["_id" =: (-1 :: Int32)], limit = 5 }
    rest cur

  close pipe

  putStrLn $ "Inserted " ++ show inserted ++ " docs into " ++ T.unpack coll
  putStrLn $ "Now total documents in " ++ T.unpack coll ++ " = " ++ show total
  putStrLn "Newest 5 documents:"
  mapM_ print newest
