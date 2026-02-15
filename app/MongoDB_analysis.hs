{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Word
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
import Numeric (showHex)

-- find name and age from bitstreams
extractNameAge :: BS.ByteString -> Maybe (String, Int)
extractNameAge bs =
  let len = fromIntegral $ runGet getWord32le (BL.fromStrict (BS.take 4 bs))
      doc = BS.take len bs
      rest = BS.drop 4 doc
  in
    let name = extractStringField "name" rest
        age  = extractInt32Field "age" rest
    in case (name, age) of
         (Just n, Just a) -> Just (n, a)
         _                -> Nothing

-- 
extractStringField :: String -> BS.ByteString -> Maybe String
extractStringField fieldName bs
  | BS.null bs = Nothing
  | BS.head bs == 0 = Nothing
  | otherwise =
      let typ = BS.head bs
          rest1 = BS.tail bs
          (key, rest2) = BS.break (== 0) rest1
          rest3 = BS.drop 1 rest2
          keyStr = map (toEnum . fromEnum) (BS.unpack key)
      in if typ == 0x02 && keyStr == fieldName
         then let strlen = runGet getWord32le (BL.fromStrict (BS.take 4 rest3))
                  nameBytes = BS.take (fromIntegral strlen) (BS.drop 4 rest3)
                  name = map (toEnum . fromEnum) (BS.unpack (BS.take (fromIntegral strlen - 1) nameBytes))
              in Just name
         else extractStringField fieldName (skipValue typ rest3)

extractInt32Field :: String -> BS.ByteString -> Maybe Int
extractInt32Field fieldName bs
  | BS.null bs = Nothing
  | BS.head bs == 0 = Nothing
  | otherwise =
      let typ = BS.head bs
          rest1 = BS.tail bs
          (key, rest2) = BS.break (== 0) rest1
          rest3 = BS.drop 1 rest2
          keyStr = map (toEnum . fromEnum) (BS.unpack key)
      in if typ == 0x10 && keyStr == fieldName
         then let age = runGet getWord32le (BL.fromStrict (BS.take 4 rest3))
              in Just (fromIntegral age)
         else extractInt32Field fieldName (skipValue typ rest3)

skipValue 0x02 bs = let strlen = runGet getWord32le (BL.fromStrict (BS.take 4 bs)) in BS.drop (4 + fromIntegral strlen) bs
skipValue 0x10 bs = BS.drop 4 bs
skipValue _ bs = BS.drop 1 bs

main :: IO ()
main = do
  BL.writeFile "out_custom.bin" BL.empty  -- empty file
  raw <- BL.readFile "users.bson"
  let bs = BL.toStrict raw
  print (BS.take 32 bs)  -- print bson file
  processAll bs
  out <- BL.readFile "out_custom.bin"
  putStrLn "==== out_custom.bin (DEC):"
  print (BL.unpack out)
  putStrLn "==== out_custom.bin (HEX):"
  print (map (\x -> let s = showHex x "" in if length s == 1 then '0':s else s) (BL.unpack out))


processAll :: BS.ByteString -> IO ()
processAll bs
  | BS.null bs = return ()
  | otherwise =
      let len = fromIntegral $ runGet getWord32le (BL.fromStrict (BS.take 4 bs))
          doc = BS.take len bs
          rest = BS.drop len bs
      in
        case extractNameAge doc of
          Just (name, age) -> do
            putStrLn $ "name=" ++ name ++ ", age=" ++ show age
            BL.appendFile "out_custom.bin" (runPut $ putWord8 (fromIntegral $ length name) >> mapM_ (putWord8 . fromIntegral . fromEnum) name >> putWord32be (fromIntegral age))
            processAll rest
          Nothing -> processAll rest
