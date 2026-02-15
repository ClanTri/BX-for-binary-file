{-# LANGUAGE TemplateHaskell,DeriveGeneric #-}
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Word
import Data.Bits
import Generics.BiGUL
import Generics.BiGUL.TH
import GHC.Generics (Generic)

-- Struction of Data
data FileStruct = FileStruct {
    prefix :: BS.ByteString,   -- File head
    age    :: Word32,          -- Age of Alice
    suffix :: BS.ByteString    -- End
}  deriving (Show, Eq, Generic)

-- Analyze bin file
parseBin :: BS.ByteString -> FileStruct
parseBin bs =
    let pre  = BS.take 30 bs
        ageB = BS.take 4 $ BS.drop 30 bs
        ageV = bsToWord32BE ageB
        suf  = BS.drop 34 bs
    in FileStruct pre ageV suf

-- Recombine bin file
assembleBin :: FileStruct -> BS.ByteString
assembleBin (FileStruct pre age suf) =
    BS.concat [pre, word32ToBsBE age, suf]

-- Analyze bson file 
parseBson :: BS.ByteString -> FileStruct
parseBson bs =
    let pre  = BS.take 26 bs
        ageB = BS.take 4 $ BS.drop 26 bs
        ageV = bsToWord32LE ageB
        suf  = BS.drop 30 bs
    in FileStruct pre ageV suf

-- Recombine bson file
assembleBson :: FileStruct -> BS.ByteString
assembleBson (FileStruct pre age suf) =
    BS.concat [pre, word32ToBsLE age, suf]

-- Trans between BE and LE
bsToWord32BE, bsToWord32LE :: BS.ByteString -> Word32
bsToWord32BE bs = foldl (\acc b -> acc * 256 + fromIntegral b) 0 (BS.unpack bs)
bsToWord32LE bs = foldr (\b acc -> acc * 256 + fromIntegral b) 0 (BS.unpack bs)

word32ToBsBE, word32ToBsLE :: Word32 -> BS.ByteString
word32ToBsBE w = BS.pack [fromIntegral (w `shiftR` 24), fromIntegral (w `shiftR` 16),
                          fromIntegral (w `shiftR` 8), fromIntegral w]
word32ToBsLE w = BS.pack [fromIntegral w, fromIntegral (w `shiftR` 8),
                          fromIntegral (w `shiftR` 16), fromIntegral (w `shiftR` 24)]

-- BiGUL：Sync the data of "Age"
syncAge :: BiGUL FileStruct FileStruct
syncAge = $(update [p| FileStruct pre age suf |] [p| FileStruct pre age suf |]
    [d| pre = Skip
        age = Replace
        suf = Skip |])

main :: IO ()
main = do
    binRaw <- BS.readFile "users.bin"
    bsonRaw <- BS.readFile "users.bson"

    let binStruct = parseBin binRaw
    let bsonStruct = parseBson bsonRaw

    putStrLn $ "Struction of BIN:  " ++ show binStruct
    putStrLn $ "Struction of BSON:  " ++ show bsonStruct

    -- Sync bson by using bin
    let bsonStruct' = put syncAge bsonStruct binStruct

    case bsonStruct' of
      Right s -> do
          BS.writeFile "users_bson_synced.bson" (assembleBson s)
          putStrLn $ "Bson file synchronized" ++ show (age s)
      Left err -> print err

      -- putStrLn $ "Synchronized age only: " ++ show newAge
-- -> putStrLn $ "Synchronization failed: " ++ err