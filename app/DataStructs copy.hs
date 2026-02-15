{-# LANGUAGE DeriveGeneric #-}

module DataStructs
  ( PgUserEntry(..)
  , BsonUserEntry(..)
  , UnifiedStruct(..)
  ) where

import qualified Data.ByteString as BS
import           Data.Word
import           GHC.Generics (Generic)

-- Binary (bin) record: no per-record field-count; each field is [4B BE length + payload]
data PgUserEntry = PgUserEntry
  { userId     :: Word32
  , userName   :: BS.ByteString
  , userGender :: BS.ByteString
  , userAge    :: Word32
  } deriving (Show, Eq, Generic)

-- BSON record: {_id :: Int32, name :: String, age :: Int32}
data BsonUserEntry = BsonUserEntry
  { bsonId   :: Word32
  , bsonAge  :: Word32
  , bsonName :: BS.ByteString
  } deriving (Show, Eq, Generic)

-- Unified view used by BiGUL (we only sync 'age')
data UnifiedStruct = UnifiedStruct
  { uid  :: Word32
  , age  :: Word32
  , name :: BS.ByteString
  } deriving (Show, Eq, Generic)
