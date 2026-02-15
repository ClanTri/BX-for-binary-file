{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Sync_Multi_Rearr_test
  ( 
    parseBinFile, parseBsonArray
  , assembleBinFile, assembleBsonFile
  , toUnifiedFromPg, toUnifiedFromBson
  , updatePgFromUnified, updateBsonFromUnified
  , syncAge
  , runCLI
  ) where


import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BC
import           Data.Bits              (shiftR)
import           Data.Word
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set

-- BiGUL
import           Generics.BiGUL
import           Generics.BiGUL.TH
import           Generics.BiGUL.Interpreter

-- Our data types in a separate module (so TH can reify them)
import           DataStructs

--------------------------------------------------------------------------------
-- bin side (NO per-record field-count; each field = 4B big-endian length + payload)
--------------------------------------------------------------------------------

parseBinFile :: BS.ByteString -> [PgUserEntry]
parseBinFile bs =
  let rest = BS.drop 19 bs
  in go rest
  where
    go s
      | BS.length s < 2                    = []
      | BS.take 2 s == BS.pack [0xFF,0xFF] = []   -- EOF marker
      | otherwise =
          let (e, s') = parsePgUserEntry s
          in e : go s'

parsePgUserEntry :: BS.ByteString -> (PgUserEntry, BS.ByteString)
parsePgUserEntry bs =
  let fcnt = bsToWord16BE (BS.take 2 bs)
      r0   = BS.drop 2 bs
      (uid,    r1) = parsePgIntField  r0
      (name,   r2) = parsePgTextField r1
      (gender, r3) = parsePgTextField r2
      (age,    r4) = parsePgIntField  r3
  in if fcnt /= 4
       then error $ "Unexpected field-count: " ++ show fcnt
       else (PgUserEntry uid name gender age, r4)

-- 文本字段: 4B BE长度 + N字节
parsePgTextField :: BS.ByteString -> (BS.ByteString, BS.ByteString)
parsePgTextField s =
  let len = bsToWord32BE (BS.take 4 s)
      v   = BS.take (fromIntegral len) (BS.drop 4 s)
      r   = BS.drop (4 + fromIntegral len) s
  in (v, r)

-- 整数字段: 4B BE长度(=4) + 4B BE整数
parsePgIntField :: BS.ByteString -> (Word32, BS.ByteString)
parsePgIntField s =
  let _len = bsToWord32BE (BS.take 4 s)   -- 应为4
      v    = bsToWord32BE (BS.take 4 (BS.drop 4 s))
      r    = BS.drop 8 s
  in (v, r)

-- 组装单条：写回 2B field-count(=4) + 四个字段(每个都有自己的4B长度前缀)
assemblePgUserEntry :: PgUserEntry -> BS.ByteString
assemblePgUserEntry (PgUserEntry uid name gender age) =
  let fcnt   = encodeWord16BE 4
      uidF   = encodeWord32BE 4 `BS.append` encodeWord32BE uid
      nameF  = encodeWord32BE (fromIntegral $ BS.length name)  `BS.append` name
      gendF  = encodeWord32BE (fromIntegral $ BS.length gender) `BS.append` gender
      ageF   = encodeWord32BE 4 `BS.append` encodeWord32BE age
  in BS.concat [fcnt, uidF, nameF, gendF, ageF]

-- 多条记录：头(19B) + 记录… + EOF(FF FF)
assembleBinFile :: [PgUserEntry] -> BS.ByteString
assembleBinFile es =
  let header = BS.pack ([80,71,67,79,80,89,10,255,13,10,0,0,0,0,0,0,0,0,0] :: [Word8])
      body   = BS.concat (map assemblePgUserEntry es)
      eof    = BS.pack [0xFF,0xFF]
  in BS.concat [header, body, eof]

--------------------------------------------------------------------------------
-- BSON side (ONLY support BSON stream: doc1 || doc2 || doc3 ...)
-- Each top-level BSON document is one user: { "_id": int32, "name": string, "age": int32 }
--------------------------------------------------------------------------------

parseBsonArray :: BS.ByteString -> [BsonUserEntry]
parseBsonArray = parseBsonStream

parseBsonStream :: BS.ByteString -> [BsonUserEntry]
parseBsonStream = go
  where
    go s
      | BS.null s         = []
      | BS.length s < 4   = error "Corrupt BSON stream: trailing bytes < 4"
      | otherwise =
          let dlenW = bsToWord32LE (BS.take 4 s)
              dlen  = fromIntegral dlenW :: Int
          in if dlenW < 5
                then error $ "Corrupt BSON stream: invalid doc length " ++ show dlenW
             else if BS.length s < dlen
                then error $ "Corrupt BSON stream: need " ++ show dlen ++ " bytes, only have " ++ show (BS.length s)
             else
               let doc  = BS.take dlen s
                   rest = BS.drop dlen s
               in parseBsonUserEntry doc : go rest

parseBsonUserEntry :: BS.ByteString -> BsonUserEntry
parseBsonUserEntry doc =
  let body           = BS.take (BS.length doc - 5) (BS.drop 4 doc)
      (idVal, r1)    = parseFieldInt32  body "_id"
      (nmVal, r2)    = parseFieldString r1   "name"
      (ageVal,_r3)   = parseFieldInt32  r2   "age"
  in BsonUserEntry idVal ageVal nmVal

parseFieldInt32 :: BS.ByteString -> String -> (Word32, BS.ByteString)
parseFieldInt32 s expect =
  if BS.length s < 1
    then error "Unexpected end while reading int32 field (need type byte)"
    else
      let typ = BS.head s
      in if typ /= 0x10
           then error $ "Expected int32 (0x10), got " ++ show typ
           else
             let r1 = BS.tail s
                 (nm, r2) = BS.break (== 0) r1
             in if BS.null r2
                  then error "Bad BSON: field name not NUL-terminated"
                  else
                    let fname = BC.unpack nm
                        r3    = BS.drop 1 r2   -- skip the 0x00 terminator
                    in if BS.length r3 < 4
                         then error "Unexpected end while reading int32 value (need 4 bytes)"
                         else
                           let val = bsToWord32LE (BS.take 4 r3)
                               r4  = BS.drop 4 r3
                           in if fname == expect
                                then (val, r4)
                                else error $ "Expected " ++ expect ++ ", got " ++ fname

parseFieldString :: BS.ByteString -> String -> (BS.ByteString, BS.ByteString)
parseFieldString s expect =
  if BS.null s then error "Unexpected end while reading int32 field" else
  let typ     = BS.head s
      _       = if typ /= 0x02 then error "Expected string" else ()
      r1      = BS.tail s
      (nm,r2) = BS.break (==0) r1
      fname   = BC.unpack nm
      r3      = BS.drop 1 r2
      slen    = bsToWord32LE (BS.take 4 r3)
      str     = BS.take (fromIntegral slen - 1) (BS.drop 4 r3)  -- drop trailing 0
      r4      = BS.drop (4 + fromIntegral slen) r3
  in if fname == expect then (str, r4) else error $ "Expected " ++ expect ++ ", got " ++ fname

assembleBsonFile :: [BsonUserEntry] -> BS.ByteString
assembleBsonFile es = BS.concat (map assembleOne es)
  where
    assembleOne (BsonUserEntry bid bage bname) =
      let body =
            BS.concat
              [ assembleBsonInt32  0x10 "_id"  bid
              , assembleBsonString 0x02 "name" bname
              , assembleBsonInt32  0x10 "age"  bage
              ] `BS.snoc` 0x00
          docLen = encodeWord32LE (fromIntegral (BS.length body + 4))
      in BS.concat [docLen, body]

assembleBsonInt32 :: Word8 -> String -> Word32 -> BS.ByteString
assembleBsonInt32 typ fname v =
  BS.concat [BS.singleton typ, BC.pack fname, BS.singleton 0x00, encodeWord32LE v]

assembleBsonString :: Word8 -> String -> BS.ByteString -> BS.ByteString
assembleBsonString typ fname val =
  let slen = encodeWord32LE (fromIntegral (BS.length val) + 1)
  in BS.concat [BS.singleton typ, BC.pack fname, BS.singleton 0x00, slen, val, BS.singleton 0x00]

--------------------------------------------------------------------------------
-- unified view + BiGUL (sync AGE only)
--------------------------------------------------------------------------------

ageLens :: BiGUL UnifiedStruct Word32
ageLens = $(rearrS [| \(UnifiedStruct _ a _) -> a |]) Replace

syncAge :: UnifiedStruct -> UnifiedStruct -> UnifiedStruct
syncAge target source =
  case get ageLens source of
    Nothing -> target
    Just aS ->
      case put ageLens target aS of
        Nothing -> target
        Just t' -> t'

toUnifiedFromPg   :: PgUserEntry   -> UnifiedStruct
toUnifiedFromPg   (PgUserEntry  i n _g a) = UnifiedStruct i a n

toUnifiedFromBson :: BsonUserEntry -> UnifiedStruct
toUnifiedFromBson (BsonUserEntry i a n)   = UnifiedStruct i a n

-- ONLY update age back
updatePgFromUnified   :: PgUserEntry   -> UnifiedStruct -> PgUserEntry
updatePgFromUnified   orig u = orig { userAge = age u }

updateBsonFromUnified :: BsonUserEntry -> UnifiedStruct -> BsonUserEntry
updateBsonFromUnified orig u = orig { bsonAge = age u }

--------------------------------------------------------------------------------
-- endian helpers
--------------------------------------------------------------------------------

bsToWord32BE :: BS.ByteString -> Word32
bsToWord32BE = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

bsToWord32LE :: BS.ByteString -> Word32
bsToWord32LE = BS.foldr  (\b acc -> acc * 256 + fromIntegral b) 0

encodeWord32BE :: Word32 -> BS.ByteString
encodeWord32BE w = BS.pack
  [ fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

encodeWord32LE :: Word32 -> BS.ByteString
encodeWord32LE w = BS.pack
  [ fromIntegral w
  , fromIntegral (w `shiftR` 8)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 24)
  ]

-- 16-bit big-endian helpers
bsToWord16BE :: BS.ByteString -> Word16
bsToWord16BE = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

encodeWord16BE :: Word16 -> BS.ByteString
encodeWord16BE w = BS.pack [ fromIntegral (w `shiftR` 8)
                           , fromIntegral w ]

--------------------------------------------------------------------------------
-- main: match by id, sync age via BiGUL, write back
---------------------------------------------------------------------

runCLI :: IO ()
runCLI = do
  putStrLn "input: bin2bson or bson2bin"
  dir <- getLine

  -- 1) Make sure the file names are correct
  --    NOTE: double-check these two actually exist in the current working directory.
  binRaw  <- BS.readFile "postgres_users.bin"  
  bsonRaw <- BS.readFile "mongo_users.bson"  

---for test--10/20
  -- 2) Parse
  let binEntries  = parseBinFile  binRaw
      bsonEntries = parseBsonArray bsonRaw

  -- 3) Debug dump: entries and ids
  let preview bs = 
        let n = min 16 (BS.length bs)
        in BC.unpack (BS.take n bs) ++ (if BS.length bs > n then "..." else "")

  putStrLn $ "[DEBUG] bin entries:  " ++ show (length binEntries)
  mapM_ (\e -> putStrLn $
        "  bin  -> id=" ++ show (userId e) ++
        ", name(len="   ++ show (BS.length (userName e))   ++ ", preview=\"" ++ preview (userName e)   ++ "\")" ++
        ", gender(len=" ++ show (BS.length (userGender e)) ++ ", preview=\"" ++ preview (userGender e) ++ "\")" ++
        ", age="        ++ show (userAge e)
        ) binEntries

  putStrLn $ "[DEBUG] bson entries: " ++ show (length bsonEntries)
  mapM_ (\e -> putStrLn $ "  bson -> id=" ++ show (bsonId e) ++
                         ", name=" ++ BC.unpack (bsonName e) ++
                         ", age="  ++ show (bsonAge e)) bsonEntries

  let binMap  = Map.fromList [(userId e, e) | e <- binEntries]
      bsonMap = Map.fromList [(bsonId e, e) | e <- bsonEntries]
      ids     = Set.toList $ Map.keysSet binMap `Set.intersection` Map.keysSet bsonMap

  putStrLn $ "[DEBUG] common ids:   " ++ show ids
  if null ids
    then do
      putStrLn "[HINT] No common ids. read this checklist:"
      putStrLn "  - reading the right files? (user.bin vs users.bin, user.bson vs users.bson)"
      putStrLn "  - parsed ids look correct? (should be small positives like 1,2,...)"
      putStrLn "  - For bin: is int value big-endian? "
      putStrLn "  - For bson: is it a BSON stream (doc1||doc2||...)?"
      putStrLn "  - Check first 4 bytes as little-endian doc length."
      putStrLn "  - If top-level key is not exactly \"data\", our parser still proceeds, but confirm length math."
    else pure ()

  -- 4) Proceed if we do have matches
  case dir of
    "bin2bson" -> do
      let updatedBson =
            [ let uniSrc = toUnifiedFromPg   (binMap  Map.! i)
                  uniTgt = toUnifiedFromBson (bsonMap Map.! i)
                  uniOut = syncAge uniTgt uniSrc
              in updateBsonFromUnified (bsonMap Map.! i) uniOut
            | i <- ids
            ]
      BS.writeFile "users_bson_synced.bson" (assembleBsonFile updatedBson)
      putStrLn "Written users_bson_synced.bson"

    "bson2bin" -> do
      let updatedBin =
            [ let uniSrc = toUnifiedFromBson (bsonMap Map.! i)
                  uniTgt = toUnifiedFromPg   (binMap  Map.! i)
                  uniOut = syncAge uniTgt uniSrc
              in updatePgFromUnified (binMap Map.! i) uniOut
            | i <- ids
            ]
      BS.writeFile "users_bin_synced.bin" (assembleBinFile updatedBin)
      putStrLn "Written users_bin_synced.bin"

    _ -> putStrLn "Unknown direction, use: bin2bson or bson2bin"
