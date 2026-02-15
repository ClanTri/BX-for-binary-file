{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module DataStructs_txt
  ( PgUserEntry(..)
  , BsonUserEntry(..)
  , UnifiedStruct(..)
  ) where

import           GHC.Generics (Generic, Rep)    
import           Generics.BiGUL.TH (deriveBiGULGeneric)
import Control.DeepSeq (NFData)

data PgUserEntry = PgUserEntry
  { userId     :: Int
  , userName   :: String
  , userGender :: String
  , userAge    :: Int
  } deriving (Show, Eq, Generic)

data BsonUserEntry = BsonUserEntry
  { bsonId   :: Int
  , bsonName :: String
  , bsonAge  :: Int
  } deriving (Show, Eq, Generic)

data UnifiedStruct = UnifiedStruct
  { uid  :: Int
  , age  :: Int
  , name :: String
  } deriving (Show, Eq, Generic)

instance NFData PgUserEntry   
instance NFData BsonUserEntry 
instance NFData UnifiedStruct 
