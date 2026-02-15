{-# LANGUAGE OverloadedStrings #-}

import Database.MongoDB
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)

main :: IO ()
main = do
  pipe <- connect (host "127.0.0.1")
  e <- access pipe master "test" $ insert "people"
         [ "name" =: ("Alice" :: Text)
         , "age"  =: (30 :: Int)
         ]
  print e
  close pipe