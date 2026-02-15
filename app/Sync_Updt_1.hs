{-# LANGUAGE TemplateHaskell #-}

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter
import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import FileStructDef

-- 解析BIN文件
parseBin :: BS.ByteString -> FileStruct
parseBin bs =
    let pre  = BS.take 42 bs
        ageB = BS.take 4 $ BS.drop 42 bs
        ageV = bsToWord32BE ageB
        suf  = BS.drop 46 bs
    in FileStruct pre ageV suf

-- 组装BIN文件
assembleBin :: FileStruct -> BS.ByteString
assembleBin (FileStruct pre age suf) =
    BS.concat [pre, word32ToBsBE age, suf]

-- 解析BSON文件
parseBson :: BS.ByteString -> FileStruct
parseBson bs =
    let pre  = BS.take 25 bs
        ageB = BS.take 4 $ BS.drop 25 bs
        ageV = bsToWord32LE ageB
        suf  = BS.drop 29 bs
    in FileStruct pre ageV suf

-- 组装BSON文件
assembleBson :: FileStruct -> BS.ByteString
assembleBson (FileStruct pre age suf) =
    BS.concat [pre, word32ToBsLE age, suf]

-- 辅助：大端/小端转换
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

ageSync' :: BiGUL FileStruct FileStruct
ageSync' = $(update [p| FileStruct pre age suf |]
                    [p| FileStruct pre age suf |]
                    [d| pre = Skip prefix
                        age = Replace
                        suf = Skip suffix |])
                        
-- 同步函数
syncAgeField :: String -> FileStruct -> FileStruct -> IO ()
syncAgeField direction binStruct bsonStruct =
    case direction of
      "bin2bson" ->
        case put ageSync' bsonStruct binStruct of
          Just bsonStruct' -> do
              BS.writeFile "users_bson_synced.bson" (assembleBson bsonStruct')
              putStrLn $ "[bin->bson] Synchronized age: " ++ show (age bsonStruct')
          Nothing -> putStrLn "Synchronization failed."
      "bson2bin" ->
        case put ageSync' binStruct bsonStruct of
          Just binStruct' -> do
              BS.writeFile "users_bin_synced.bin" (assembleBin binStruct')
              putStrLn $ "[bson->bin] Synchronized age: " ++ show (age binStruct')
          Nothing -> putStrLn "Synchronization failed."
      _ -> putStrLn "Unknown direction. Use \"bin2bson\" or \"bson2bin\"."

main :: IO ()
main = do
    binRaw  <- BS.readFile "users.bin"
    bsonRaw <- BS.readFile "users.bson"
    let binStruct  = parseBin binRaw
    let bsonStruct = parseBson bsonRaw
    putStrLn "输入同步方向：bin2bson 或 bson2bin"
    direction <- getLine
    syncAgeField direction binStruct bsonStruct