{-# LANGUAGE DeriveGeneric #-}

import Data.Binary
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)

-- define User
data User = User
  { userId :: Int
  , userName :: String
  } deriving (Show, Generic)

instance Binary User

-- encode to ByteString
saveUser :: FilePath -> User -> IO ()
saveUser path user = BL.writeFile path (encode user)

-- decode to User
loadUser :: FilePath -> IO User
loadUser path = do
  content <- BL.readFile path
  return (decode content)

-- main
main :: IO ()
main = do
  let user = User 101 "Alice"
  saveUser "user.dat" user
  putStrLn "User saved to user.dat."

  user2 <- loadUser "user.dat"
  putStrLn $ "Loaded user: " ++ show user2