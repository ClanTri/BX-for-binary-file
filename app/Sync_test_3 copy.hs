{-# LANGUAGE TemplateHaskell #-}
import Generics.BiGUL
import Generics.BiGUL.Interpreter
-- import Generics.BiGUL.TH
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BSC
import Data.Word
import Data.Bits
import FileStructDef


-- parse bin file
parseBin :: BS.ByteString -> FileStruct
parseBin bs =
    let pre  = BS.take 30 bs
        ageB = BS.take 4 $ BS.drop 30 bs
        ageV = bsToWord32BE ageB
        suf  = BS.drop 34 bs
    in FileStruct pre ageV suf

-- Reassemble bin
assembleBin :: FileStruct -> BS.ByteString
assembleBin (FileStruct pre age suf) =
    BS.concat [pre, word32ToBsBE age, suf]

-- parse bson file
parseBson :: BS.ByteString -> FileStruct
parseBson bs =
    let pre  = BS.take 26 bs
        ageB = BS.take 4 $ BS.drop 26 bs
        ageV = bsToWord32LE ageB
        suf  = BS.drop 30 bs
    in FileStruct pre ageV suf

-- Reassemble  bson
assembleBson :: FileStruct -> BS.ByteString
assembleBson (FileStruct pre age suf) =
    BS.concat [pre, word32ToBsLE age, suf]

-- trans between bigendian and littleendian 
bsToWord32BE, bsToWord32LE :: BS.ByteString -> Word32
bsToWord32BE bs = foldl (\acc b -> acc * 256 + fromIntegral b) 0 (BS.unpack bs)
bsToWord32LE bs = foldr (\b acc -> acc * 256 + fromIntegral b) 0 (BS.unpack bs)

word32ToBsBE, word32ToBsLE :: Word32 -> BS.ByteString
word32ToBsBE w = BS.pack [fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16),
                          fromIntegral (w `shiftR` 8), fromIntegral w]
word32ToBsLE w = BS.pack [fromIntegral w, fromIntegral (w `shiftR` 8),
                          fromIntegral (w `shiftR` 16), fromIntegral (w `shiftR` 24)]

-- BiGUL：Sync age only
ageSync :: BiGUL Word32 Word32
ageSync = Replace

        
main :: IO ()
main = do
    binRaw <- BS.readFile "users.bin"
    bsonRaw <- BS.readFile "users.bson"
    
    let binStruct  = parseBin binRaw
    let bsonStruct = parseBson bsonRaw
    
    let ageResult = put ageSync (age bsonStruct) (age binStruct)

    case ageResult of
      Right newAge -> do
        let bsonStruct' = bsonStruct { age = newAge }
        BS.writeFile "users_bson_synced.bson" (assembleBson bsonStruct')
        putStrLn $ "Synchronized age only: " ++ show newAge
      Left err -> putStrLn $ "Synchronization failed: " ++ err