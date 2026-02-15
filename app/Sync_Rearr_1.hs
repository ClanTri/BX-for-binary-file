{-# LANGUAGE TemplateHaskell #-}

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import FileStructDef

-- parse bin file
parseBin :: BS.ByteString -> FileStruct
parseBin bs =
  let pre  = BS.take 42 bs
      ageB = BS.take 4 $ BS.drop 42 bs
      ageV = bsToWord32BE ageB
      suf  = BS.drop 46 bs
  in FileStruct pre ageV suf

-- Reassemble bin
assembleBin :: FileStruct -> BS.ByteString
assembleBin (FileStruct pre age suf) =
  BS.concat [pre, word32ToBsBE age, suf]

-- parse bson file
parseBson :: BS.ByteString -> FileStruct
parseBson bs =
  let pre  = BS.take 25 bs
      ageB = BS.take 4 $ BS.drop 25 bs
      ageV = bsToWord32LE ageB
      suf  = BS.drop 29 bs
  in FileStruct pre ageV suf

-- Reassemble bson
assembleBson :: FileStruct -> BS.ByteString
assembleBson (FileStruct pre age suf) =
  BS.concat [pre, word32ToBsLE age, suf]

-- trans between bigendian and littleendian 
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
-- s1--get-->v--s2--put-->s2’

ageLens :: BiGUL FileStruct Word32
ageLens = $(rearrS [| \(FileStruct _ a _) -> a |]) Replace

-- Synchronization
syncAgeField :: String -> FileStruct -> FileStruct -> IO ()
syncAgeField direction binStruct bsonStruct =
  case direction of
    "bin2bson" -> do
      -- get age from binfile
      let Just ageFromBin = get ageLens binStruct
      -- put age into bsonfile
      let Just bsonStruct' = put ageLens bsonStruct ageFromBin
      BS.writeFile "users_bson_synced.bson" (assembleBson bsonStruct')
      putStrLn $ "[bin->bson] Synchronized age: " ++ show (age bsonStruct')
    "bson2bin" -> do
      -- get age from bsonfile
      let Just ageFromBson = get ageLens bsonStruct
      -- put age into binfile
      let Just binStruct' = put ageLens binStruct ageFromBson
      BS.writeFile "users_bin_synced.bin" (assembleBin binStruct')
      putStrLn $ "[bson->bin] Synchronized age: " ++ show (age binStruct')
    _ -> putStrLn "Unknown direction. Use \"bin2bson\" or \"bson2bin\"."

main :: IO ()
main = do
  binRaw  <- BS.readFile "users.bin"
  bsonRaw <- BS.readFile "users.bson"
  let binStruct  = parseBin binRaw
  let bsonStruct = parseBson bsonRaw

  putStrLn "input：bin2bson or bson2bin"
  direction <- getLine
  syncAgeField direction binStruct bsonStruct