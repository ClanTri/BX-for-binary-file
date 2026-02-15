{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Word
import Numeric (showHex)

-- 你的结构体
data BsonUserEntry = BsonUserEntry
  { bsonId   :: Word32
  , bsonAge  :: Word32
  , bsonName :: BS.ByteString
  } deriving (Show, Eq)

bsToWord32LE :: BS.ByteString -> Word32
bsToWord32LE bs = BS.foldr (\b acc -> acc * 256 + fromIntegral b) 0 bs

main :: IO ()
main = do
  bs <- BS.readFile "app/users.bson"
  let entries = parseBsonArray bs
  putStrLn "Parsed BSON entries:"
  mapM_ print entries

-- 跳过顶层的 array/doc，只剥离 array 部分
parseBsonArray :: BS.ByteString -> [BsonUserEntry]
parseBsonArray bs =
  let afterLen = BS.drop 4 bs
      _type = BS.head afterLen      -- 0x04
      rest1 = BS.tail afterLen
      (_, rest2) = BS.break (== 0) rest1
      rest3 = BS.drop 1 rest2
      arrayLen = bsToWord32LE (BS.take 4 rest3)
      arrayBody = BS.take (fromIntegral arrayLen - 5) (BS.drop 4 rest3)
  in parseArrayDocs arrayBody

parseArrayDocs :: BS.ByteString -> [BsonUserEntry]
parseArrayDocs bs
  | BS.null bs = []
  | BS.head bs == 0x00 = []
  | otherwise =
      let typ = BS.head bs
          rest1 = BS.tail bs
          (_, rest2) = BS.break (== 0) rest1 -- 跳过"0"/"1"/"2"...
          rest3 = BS.drop 1 rest2
          docLen = bsToWord32LE (BS.take 4 rest3)
          doc = BS.take (fromIntegral docLen) rest3
          rest4 = BS.drop (fromIntegral docLen) rest3
          entry = parseBsonUserEntry doc
      in entry : parseArrayDocs rest4

-- 解析单个 entry 的 BSON 文档
parseBsonUserEntry :: BS.ByteString -> BsonUserEntry
parseBsonUserEntry bs =
  let body = BS.drop 4 bs
      (idVal, rest1)   = parseFieldInt32 body "_id"
      (nameVal, rest2) = parseFieldString rest1 "name"
      (ageVal, _)      = parseFieldInt32 rest2 "age"
  in BsonUserEntry idVal ageVal nameVal

-- 解析一个int32字段
parseFieldInt32 :: BS.ByteString -> String -> (Word32, BS.ByteString)
parseFieldInt32 bs expectName =
  let typ = BS.head bs
      rest1 = BS.tail bs
      (fname, rest2) = BS.break (== 0) rest1
      fieldName = BC.unpack fname
      rest3 = BS.drop 1 rest2
      val = bsToWord32LE (BS.take 4 rest3)
      rest4 = BS.drop 4 rest3
  in if fieldName == expectName then (val, rest4)
     else error $ "Expected field: " ++ expectName ++ ", got: " ++ fieldName

-- 解析一个string字段
parseFieldString :: BS.ByteString -> String -> (BS.ByteString, BS.ByteString)
parseFieldString bs expectName =
  let typ = BS.head bs
      rest1 = BS.tail bs
      (fname, rest2) = BS.break (== 0) rest1
      fieldName = BC.unpack fname
      rest3 = BS.drop 1 rest2
      strLen = bsToWord32LE (BS.take 4 rest3)
      str = BS.take (fromIntegral strLen - 1) (BS.drop 4 rest3)  -- -1 去掉末尾0
      rest4 = BS.drop (4 + fromIntegral strLen) rest3
  in if fieldName == expectName then (str, rest4)
     else error $ "Expected field: " ++ expectName ++ ", got: " ++ fieldName
