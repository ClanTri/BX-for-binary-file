{-# LANGUAGE DeriveGeneric #-}

module PgBinaryStruct where

import qualified Data.ByteString as BS
import Data.Word

-- File Header (PGCOPY\n...)
data PgHeader = PgHeader {
  pgSignature :: BS.ByteString  -- 11 bytes
, pgFlags     :: Word32         -- 4 bytes
, pgExtLen    :: Word32         -- 4 bytes (usually 0)
} deriving (Show, Eq)

-- User record
data PgUserEntry = PgUserEntry {
  userId     :: Word32,         -- 4 bytes
  userName   :: BS.ByteString,  -- variable length
  userGender :: BS.ByteString,  -- variable length
  userAge    :: Word32          -- 4 bytes
} deriving (Show, Eq)

-- Full structure
data PgBinaryFile = PgBinaryFile {
  pgHeader  :: PgHeader,
  pgRecords :: [PgUserEntry]
} deriving (Show, Eq)