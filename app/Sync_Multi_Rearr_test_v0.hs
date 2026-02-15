{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Word
import Numeric (showHex)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Bits
import GHC.Generics 
import Generics.BiGUL
import Generics.BiGUL.TH

-----------------------------
-- bin 
-----------------------------
data PgUserEntry = PgUserEntry
  { userId     :: Word32
  , userName   :: BS.ByteString
  , userGender :: BS.ByteString
  , userAge    :: Word32
  } deriving (Show, Eq, Generic)

-----------------------------
-- bson 
-----------------------------
data BsonUserEntry = BsonUserEntry
  { bsonId   :: Word32
  , bsonAge  :: Word32
  , bsonName :: BS.ByteString
  } deriving (Show, Eq, Generic)

-----------------------------
-- UnifiedStruct
-----------------------------
data UnifiedStruct = UnifiedStruct
  { uid   :: Word32
  , age   :: Word32
  , name  :: BS.ByteString
  } deriving (Show, Eq, Generic)

-----------------------------
-- BiGUL lens：only age 
-----------------------------
-- BiGUL lens for age
ageLens :: BiGUL UnifiedStruct Word32
ageLens = $(rearrS [| \u -> age u |]) Replace


-- 
syncAge :: UnifiedStruct -> UnifiedStruct -> UnifiedStruct
syncAge s v = case put ageLens s (get ageLens v) of
  Just s' -> s'
  Nothing -> s

-----------------------------
-- bin file analysis and writting
-----------------------------
bsToWord32LE :: BS.ByteString -> Word32
bsToWord32LE bs = BS.foldr (\b acc -> acc * 256 + fromIntegral b) 0 bs

bsToWord32BE :: BS.ByteString -> Word32
bsToWord32BE bs = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 bs

encodeWord32LE :: Word32 -> BS.ByteString
encodeWord32LE w = BS.pack [fromIntegral w, fromIntegral (w `shiftR` 8), fromIntegral (w `shiftR` 16), fromIntegral (w `shiftR` 24)]

encodeWord32BE :: Word32 -> BS.ByteString
encodeWord32BE w = BS.pack [fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16), fromIntegral (w `shiftR` 8), fromIntegral w]

parseBinFile :: BS.ByteString -> [PgUserEntry]
parseBinFile bs =
  let skipHeader = 19
      go rest
        | BS.length rest < 2 = []
        | BS.take 2 rest == BS.pack [0xFF, 0xFF] = []
        | otherwise =
            let (entry, remain) = parsePgUserEntry rest
            in entry : go remain
  in go (BS.drop skipHeader bs)

parsePgUserEntry :: BS.ByteString -> (PgUserEntry, BS.ByteString)
parsePgUserEntry bs =
  let (uid, r1)    = parsePgIntField bs
      (name, r2)   = parsePgTextField r1
      (gender, r3) = parsePgTextField r2
      (age, r4)    = parsePgIntField r3
  in (PgUserEntry uid name gender age, r4)

parsePgTextField :: BS.ByteString -> (BS.ByteString, BS.ByteString)
parsePgTextField bs =
  let len = bsToWord32BE (BS.take 4 bs)
      content = BS.take (fromIntegral len) (BS.drop 4 bs)
      rest = BS.drop (4 + fromIntegral len) bs
  in (content, rest)

parsePgIntField :: BS.ByteString -> (Word32, BS.ByteString)
parsePgIntField bs =
  let len = bsToWord32BE (BS.take 4 bs) -- must == 4
      val = bsToWord32BE (BS.take 4 (BS.drop 4 bs))
      rest = BS.drop (4 + 4) bs
  in (val, rest)

assembleBinFile :: [PgUserEntry] -> BS.ByteString
assembleBinFile entries =
  let header = BS.pack ([80,71,67,79,80,89,10,255,13,10,0,0,0,0,0,0,0,0,0] :: [Word8])
      recs = BS.concat (map assemblePgUserEntry entries)
      eof = BS.pack [0xFF, 0xFF]
  in BS.concat [header, recs, eof]

assemblePgUserEntry :: PgUserEntry -> BS.ByteString
assemblePgUserEntry (PgUserEntry uid name gender age) =
  let uidF    = encodeWord32BE 4 `BS.append` encodeWord32BE uid
      nameF   = encodeWord32BE (fromIntegral $ BS.length name) `BS.append` name
      genderF = encodeWord32BE (fromIntegral $ BS.length gender) `BS.append` gender
      ageF    = encodeWord32BE 4 `BS.append` encodeWord32BE age
  in BS.concat [uidF, nameF, genderF, ageF]

-----------------------------
-- bson file analysis and writting
-----------------------------
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
          (_, rest2) = BS.break (== 0) rest1
          rest3 = BS.drop 1 rest2
          docLen = bsToWord32LE (BS.take 4 rest3)
          doc = BS.take (fromIntegral docLen) rest3
          rest4 = BS.drop (fromIntegral docLen) rest3
          entry = parseBsonUserEntry doc
      in entry : parseArrayDocs rest4

parseBsonUserEntry :: BS.ByteString -> BsonUserEntry
parseBsonUserEntry bs =
  let body = BS.drop 4 bs
      (idVal, rest1)   = parseFieldInt32 body "_id"
      (nameVal, rest2) = parseFieldString rest1 "name"
      (ageVal, _)      = parseFieldInt32 rest2 "age"
  in BsonUserEntry idVal ageVal nameVal

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

parseFieldString :: BS.ByteString -> String -> (BS.ByteString, BS.ByteString)
parseFieldString bs expectName =
  let typ = BS.head bs
      rest1 = BS.tail bs
      (fname, rest2) = BS.break (== 0) rest1
      fieldName = BC.unpack fname
      rest3 = BS.drop 1 rest2
      strLen = bsToWord32LE (BS.take 4 rest3)
      str = BS.take (fromIntegral strLen - 1) (BS.drop 4 rest3)
      rest4 = BS.drop (4 + fromIntegral strLen) rest3
  in if fieldName == expectName then (str, rest4)
     else error $ "Expected field: " ++ expectName ++ ", got: " ++ fieldName

assembleBsonFile :: [BsonUserEntry] -> BS.ByteString
assembleBsonFile entries =
  let arrBody = BS.concat (zipWith assembleBsonArrayItem [0..] entries) `BS.snoc` 0x00
      arrLen  = encodeWord32LE (fromIntegral $ BS.length arrBody + 4 + 1)
      arrayDoc = BS.concat [arrLen, arrBody, BS.singleton 0x00]
      dataField = BS.concat [BS.singleton 0x04, "data", BS.singleton 0x00, encodeWord32LE (fromIntegral $ BS.length arrayDoc), arrayDoc]
      docLen = encodeWord32LE (fromIntegral $ BS.length dataField + 4 + 1)
  in BS.concat [docLen, dataField, BS.singleton 0x00]

assembleBsonArrayItem :: Int -> BsonUserEntry -> BS.ByteString
assembleBsonArrayItem idx (BsonUserEntry bid bage bname) =
  let docBody = BS.concat [ assembleBsonInt32 0x10 "_id" bid
                          , assembleBsonString 0x02 "name" bname
                          , assembleBsonInt32 0x10 "age" bage
                          ] `BS.snoc` 0x00
      docLen = encodeWord32LE (fromIntegral $ BS.length docBody + 4)
      key = BC.pack (show idx)
  in BS.concat [BS.singleton 0x03, key, BS.singleton 0x00, docLen, docBody]

assembleBsonInt32 :: Word8 -> String -> Word32 -> BS.ByteString
assembleBsonInt32 typ fname val =
  BS.concat [BS.singleton typ, BC.pack fageLens = $(rearrS [| \u -> age u |])name, BS.singleton 0x00, encodeWord32LE val]

assembleBsonString :: Word8 -> String -> BS.ByteString -> BS.ByteString
assembleBsonString typ fname val =
  let slen = encodeWord32LE (fromIntegral $ BS.length val + 1)
  in BS.concat [BS.singleton typ, BC.pack fname, BS.singleton 0x00, slen, val, BS.singleton 0x00]

-----------------------------
-- 
-----------------------------
toUnifiedFromPg :: PgUserEntry -> UnifiedStruct
toUnifiedFromPg (PgUserEntry uid name _gender age) = UnifiedStruct uid age name

toUnifiedFromBson :: BsonUserEntry -> UnifiedStruct
toUnifiedFromBson (BsonUserEntry uid age name) = UnifiedStruct uid age name

fromUnifiedToPg :: PgUserEntry -> UnifiedStruct -> PgUserEntry
fromUnifiedToPg orig unified = orig { userAge = age unified }

fromUnifiedToBson :: BsonUserEntry -> UnifiedStruct -> BsonUserEntry
fromUnifiedToBson orig unified = orig { bsonAge = age unified }

-----------------------------
-- main process
-----------------------------
main :: IO ()
main = do
  putStrLn "input: bin2bson or bson2bin"
  direction <- getLine

  binRaw <- BS.readFile "app/users.bin"
  bsonRaw <- BS.readFile "app/users.bson"
  let binEntries  = parseBinFile binRaw
      bsonEntries = parseBsonArray bsonRaw

      binMap  = Map.fromList [(userId e, e) | e <- binEntries]
      bsonMap = Map.fromList [(bsonId e, e) | e <- bsonEntries]

      commonIds = Set.toList $ Map.keysSet binMap `Set.intersection` Map.keysSet bsonMap

  case direction of
    "bin2bson" -> do
      let updatedBson = [ let srcUni = toUnifiedFromPg (binMap Map.! i)
                              origB = bsonMap Map.! i
                              origUni = toUnifiedFromBson origB
                              syncedUni = syncAge origUni srcUni
                          in fromUnifiedToBson origB syncedUni
                        | i <- commonIds
                        ]
      BS.writeFile "users_bson_synced.bson" (assembleBsonFile updatedBson)
      putStrLn "Written users_bson_synced.bson"
    "bson2bin" -> do
      let updatedBin = [ let srcUni = toUnifiedFromBson (bsonMap Map.! i)
                  
                             origBin = binMap Map.! i
                             origUni = toUnifiedFromPg origBin
                             syncedUni = syncAge origUni srcUni
                         in fromUnifiedToPg origBin syncedUni
                       | i <- commonIds
                       ]
      BS.writeFile "users_bin_synced.bin" (assembleBinFile updatedBin)
      putStrLn "Written users_bin_synced.bin"
    _ -> putStrLn "Unknown direction, use bin2bson or bson2bin"
