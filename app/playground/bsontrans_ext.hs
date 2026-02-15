{-# LANGUAGE DeriveGeneric #-}

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import Control.Monad (replicateM)
import GHC.Generics (Generic)

data User = User
  { userId :: Int
  , userName :: String
  } deriving (Show)

putUser :: User -> Put
putUser (User uid name) = do
  putWord32be (fromIntegral uid)
  putWord8 (fromIntegral $ length name)
  mapM_ (putWord8 . fromIntegral . fromEnum) name

getUser :: Get User
getUser = do
  uid <- getWord32be
  len <- getWord8
  chars <- replicateM (fromIntegral len) getWord8
  return $ User (fromIntegral uid) (map (toEnum . fromIntegral) chars)

main :: IO ()
main = do
  let user = User 42 "Bob"
  let bs = runPut (putUser user)
  BL.writeFile "user_custom2.dat" bs

  bs2 <- BL.readFile "user_custom2.dat"
  let user2 = runGet getUser bs2
  print user2