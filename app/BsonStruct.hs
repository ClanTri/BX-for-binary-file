{-# LANGUAGE DeriveGeneric #-}
module BsonStruct (BsonHeader(..), BsonUserEntry(..), BsonFileStruct(..)) where

import qualified Data.ByteString as BS
import Data.Word


data BsonHeader = BsonHeader {
  bsonHeaderBytes :: BS.ByteString  
} deriving (Show, Eq)

-- BSON users
data BsonUserEntry = BsonUserEntry {
  bsonId   :: Word32,          -- _id
  bsonAge  :: Word32,          -- age
  bsonName :: BS.ByteString,   -- name
  bsonRaw  :: BS.ByteString    -- suf(reserve)
} deriving (Show, Eq)

-- BSON file structure
data BsonFileStruct = BsonFileStruct {
  bsonHeader  :: BsonHeader,         -- pre
  bsonEntries :: [BsonUserEntry],    -- entries
  bsonFooter  :: BS.ByteString       -- suf
} deriving (Show, Eq)
