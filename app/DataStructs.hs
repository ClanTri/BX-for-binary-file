{-# LANGUAGE DeriveGeneric #-}

module DataStructs where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.Word

data PgUserEntry = PgUserEntry
  { userId     :: Word32
  , userName   :: BS.ByteString
  , userGender :: BS.ByteString
  , userAge    :: Word32
  } deriving (Show, Eq, Generic)

data BsonUserEntry = BsonUserEntry
  { bsonId   :: Word32
  , bsonAge  :: Word32
  , bsonName :: BS.ByteString
  } deriving (Show, Eq, Generic)

data UnifiedStruct = UnifiedStruct
  { uid  :: Word32
  , age  :: Word32
  , name :: BS.ByteString
  } deriving (Show, Eq, Generic)

instance NFData PgUserEntry
instance NFData BsonUserEntry
instance NFData UnifiedStruct
