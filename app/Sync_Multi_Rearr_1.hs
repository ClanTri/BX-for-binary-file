{-# LANGUAGE TemplateHaskell #-}

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import FileStructDef_bson
import FileStructDef_bin
import UnifiedStructDef

-- Convert PgUserEntry to FileStruct
pgToUnified :: PgUserEntry -> FileStruct
pgToUnified (PgUserEntry uid name gender age) =
  let pre  = BS.concat [encodeWord32BE uid, encodeWithLength name, encodeWithLength gender]
      suf  = BS.empty
  in UnifiedStruct pre age suf

-- Convert BsonUserEntry to FileStruct
bsonToUnified :: BsonUserEntry -> FileStruct
bsonToUnified (BsonUserEntry uid age name raw) =
  let pre  = BS.concat [encodeWord32LE uid, encodeWithLength name]
      suf  = raw
  in UnifiedStruct pre age suf

-- Convert FileStruct back to PgUserEntry
unifiedToPg :: FileStruct -> PgUserEntry
unifiedToPg (UnifiedStruct pre age _) =
  let (uidBs, rest1) = BS.splitAt 4 pre
      (nameLenBs, rest2) = BS.splitAt 4 rest1
      nameLen = bsToWord32BE nameLenBs
      (name, rest3) = BS.splitAt (fromIntegral nameLen) rest2
      (genderLenBs, genderData) = BS.splitAt 4 rest3
      genderLen = bsToWord32BE genderLenBs
      gender = BS.take (fromIntegral genderLen) genderData
      uid = bsToWord32BE uidBs
  in PgUserEntry uid name gender age

-- Convert FileStruct back to BsonUserEntry
unifiedToBson :: FileStruct -> BsonUserEntry
unifiedToBson (UnifiedStruct pre age suf) =
  let (uidBs, rest1) = BS.splitAt 4 pre
      (nameLenBs, nameData) = BS.splitAt 4 rest1
      nameLen = bsToWord32LE nameLenBs
      name = BS.take (fromIntegral nameLen) nameData
      uid = bsToWord32LE uidBs
  in BsonUserEntry uid age name suf

-- Utility functions
encodeWord32BE :: Word32 -> BS.ByteString
encodeWord32BE w = BS.pack [ fromIntegral (w `shiftR` 24)
                           , fromIntegral (w `shiftR` 16)
                           , fromIntegral (w `shiftR` 8)
                           , fromIntegral w ]

encodeWord32LE :: Word32 -> BS.ByteString
encodeWord32LE w = BS.pack [ fromIntegral w
                           , fromIntegral (w `shiftR` 8)
                           , fromIntegral (w `shiftR` 16)
                           , fromIntegral (w `shiftR` 24) ]

encodeWithLength :: BS.ByteString -> BS.ByteString
encodeWithLength bs = encodeWord32BE (fromIntegral $ BS.length bs) `BS.append` bs

bsToWord32BE :: BS.ByteString -> Word32
bsToWord32BE bs = foldl (\acc b -> acc * 256 + fromIntegral b) 0 (BS.unpack bs)

bsToWord32LE :: BS.ByteString -> Word32
bsToWord32LE bs = foldr (\b acc -> acc * 256 + fromIntegral b) 0 (BS.unpack bs)

-- BiGUL lens for age
ageLens :: BiGUL FileStruct Word32
ageLens = $(rearrS [| \(UnifiedStruct _ a _) -> a |]) Replace

-- Synchronize one pair
syncAge :: FileStruct -> FileStruct -> FileStruct
syncAge s v = case put ageLens s (get ageLens v) of
  Just s' -> s'
  Nothing -> s

main :: IO ()
main = do
  putStrLn "input: bin2bson or bson2bin"
  direction <- getLine

  -- read and decode all entries
  binRaw <- BS.readFile "users.bin"
  bsonRaw <- BS.readFile "users.bson"

  let binEntries  = [PgUserEntry 1 "Alice" "Female" 25, PgUserEntry 2 "Mike" "Male" 23] -- mockup
      bsonEntries = [BsonUserEntry 1 25 "Alice" BS.empty, BsonUserEntry 2 23 "Mike" BS.empty] -- mockup

      binMap = [(userId e, pgToUnified e) | e <- binEntries]
      bsonMap = [(bsonId e, bsonToUnified e) | e <- bsonEntries]

      commonIds = [i | (i, _) <- binMap, i `elem` map fst bsonMap]
      lookup' m i = case lookup i m of Just x -> x; Nothing -> error "missing"

      updated = [ case direction of
                    "bin2bson" -> (i, syncAge (lookup' bsonMap i) (lookup' binMap i))
                    "bson2bin" -> (i, syncAge (lookup' binMap i) (lookup' bsonMap i))
                    _ -> error "Unknown direction"
                | i <- commonIds ]

      binUpdated  = [ unifiedToPg u | (i, u) <- updated ]
      bsonUpdated = [ unifiedToBson u | (i, u) <- updated ]

  -- mockup output
  print binUpdated
  print bsonUpdated
