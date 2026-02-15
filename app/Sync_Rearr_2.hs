{-# LANGUAGE TemplateHaskell #-}

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import FileStructDef

-- 
parseBin :: BS.ByteString -> BinStruct
parseBin bs =
    let pre  = BS.take 42 bs
        ageB = BS.take 4 $ BS.drop 42 bs
        ageV = bsToWord32BE ageB
        suf  = BS.drop 46 bs
    in BinStruct pre ageV suf

assembleBin :: BinStruct -> BS.ByteString
assembleBin (BinStruct pre age suf) =
    BS.concat [pre, word32ToBsBE age, suf]

parseBson :: BS.ByteString -> BsonStruct
parseBson bs =
    let pre  = BS.take 25 bs
        ageB = BS.take 4 $ BS.drop 25 bs
        ageV = bsToWord32LE ageB
        suf  = BS.drop 29 bs
    in BsonStruct pre ageV suf

assembleBson :: BsonStruct -> BS.ByteString
assembleBson (BsonStruct pre age suf) =
    BS.concat [pre, word32ToBsLE age, suf]

-- 
bsToWord32BE, bsToWord32LE :: BS.ByteString -> Word32
bsToWord32BE bs = foldl (\acc b -> acc * 256 + fromIntegral b) 0 (BS.unpack bs)
bsToWord32LE bs = foldr (\b acc -> acc * 256 + fromIntegral b) 0 (BS.unpack bs)

word32ToBsBE, word32ToBsLE :: Word32 -> BS.ByteString
word32ToBsBE w = BS.pack [ fromIntegral (w `shiftR` 24)
                         , fromIntegral (w `shiftR` 16)
                         , fromIntegral (w `shiftR` 8)
                         , fromIntegral w ]
word32ToBsLE w = BS.pack [ fromIntegral w
                         , fromIntegral (w `shiftR` 8)
                         , fromIntegral (w `shiftR` 16)
                         , fromIntegral (w `shiftR` 24) ]

-- BiGUL：Sync age only 
-- S1--get-->V--put-->S2
ageSync :: BiGUL BinStruct BsonStruct
ageSync =
  $(rearrS [| \(BinStruct p a s) -> (p, a, s) |]) $
  $(rearrV [| \(BsonStruct p a s) -> (p, a, s) |]) $
    (Skip (\(p, _, _) -> p))
      `Prod`
    Replace
      `Prod`
    (Skip (\(_, _, s) -> s))

-- 
ageSyncInv :: BiGUL BsonStruct BinStruct
ageSyncInv =
  $(rearrS [| \(BsonStruct p a s) -> (p, a, s) |]) $
  $(rearrV [| \(BinStruct p a s) -> (p, a, s) |]) $
    (Skip (\(p, _, _) -> p))
      `Prod`
    Replace
      `Prod`
    (Skip (\(_, _, s) -> s))

-- Synchronization
syncAgeField :: String -> BinStruct -> BsonStruct -> IO ()
syncAgeField direction binStruct bsonStruct =
    case direction of
      "bin2bson" ->  -- bin -> bson
        case put ageSync binStruct bsonStruct of
          Just binStructResult -> do
              BS.writeFile "users_bin_synced.bin" (assembleBin binStructResult)
              putStrLn $ "[bin->bson] Synchronized age: " ++ show (binAge binStructResult)
          Nothing -> putStrLn "Synchronization failed."
      "bson2bin" ->  -- bson -> bin
        case put ageSyncInv bsonStruct binStruct of
          Just bsonStructResult -> do
              BS.writeFile "users_bson_synced.bson" (assembleBson bsonStructResult)
              putStrLn $ "[bson->bin] Synchronized age: " ++ show (bsonAge bsonStructResult)
          Nothing -> putStrLn "Synchronization failed."
      _ -> putStrLn "Unknown direction. Use \"bin2bson\" or \"bson2bin\"."

main :: IO ()
main = do
    binRaw  <- BS.readFile "users.bin"
    bsonRaw <- BS.readFile "users.bson"
    let binStruct  = parseBin binRaw
    let bsonStruct = parseBson bsonRaw

    putStrLn "input: bin2bson or bson2bin"
    direction <- getLine
    syncAgeField direction binStruct bsonStruct
