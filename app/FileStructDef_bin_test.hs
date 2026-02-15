{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Word
import PgBinaryStruct
import Debug.Trace (trace)


-- 解析大文件结构
parseBinFile :: BS.ByteString -> PgBinaryFile
parseBinFile bs =
  let (headerBs, rest) = BS.splitAt 19 bs
      header = parsePgHeader headerBs
      entries = parseAllRecords rest
  in PgBinaryFile header entries

-- 解析文件头
parsePgHeader :: BS.ByteString -> PgHeader
parsePgHeader bs =
  let sig = BS.take 11 bs
      flags = bsToWord32BE (BS.take 4 (BS.drop 11 bs))
      extlen = bsToWord32BE (BS.drop 15 bs)
  in PgHeader sig flags extlen

-- 解析所有记录
parseAllRecords :: BS.ByteString -> [PgUserEntry]
parseAllRecords bs
  | BS.length bs < 2 = []
  | BS.take 2 bs == BS.pack [0xFF, 0xFF] = []
  | otherwise =
      let (entry, rest) = parsePgUserEntry bs
      in entry : parseAllRecords rest

-- 解析单条记录
parsePgUserEntry :: BS.ByteString -> (PgUserEntry, BS.ByteString)
parsePgUserEntry bs =
  let fieldCount = bsToWord16BE (BS.take 2 bs)
      r0 = BS.drop 2 bs
      (uid,    r1) = parsePgIntField r0      -- id
      (name,   r2) = parsePgTextField r1     -- name
      (gender, r3) = parsePgTextField r2     -- gender
      (age,    r4) = parsePgIntField r3      -- age
  in
    if fieldCount /= 4
      then error ("Unexpected field count: " ++ show fieldCount)
      else (PgUserEntry uid name gender age, r4)


-- 字段解析
parsePgTextField :: BS.ByteString -> (BS.ByteString, BS.ByteString)
parsePgTextField bs =
  let len = bsToWord32BE (BS.take 4 bs)
      content = BS.take (fromIntegral len) (BS.drop 4 bs)
      rest = BS.drop (4 + fromIntegral len) bs
  in
    trace ("[TextField] len=" ++ show len ++ ", content=" ++ show (BC.unpack content)) (content, rest)

parsePgIntField :: BS.ByteString -> (Word32, BS.ByteString)
parsePgIntField bs =
  let len = bsToWord32BE (BS.take 4 bs)
      val = bsToWord32BE (BS.take 4 (BS.drop 4 bs))
      rest = BS.drop (4 + 4) bs
  in (val, rest)

bsToWord32BE :: BS.ByteString -> Word32
bsToWord32BE bs = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 bs

bsToWord16BE :: BS.ByteString -> Word16
bsToWord16BE bs = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 bs

main :: IO ()
main = do
  bs <- BS.readFile "user.bin"
  let PgBinaryFile header entries = parseBinFile bs
  putStrLn $ "Header: " ++ show header
  putStrLn "Records:"
  mapM_ (\(PgUserEntry uid name gender age) -> do
    putStrLn $ "ID: " ++ show uid
    putStrLn $ "Name: " ++ BC.unpack name
    putStrLn $ "Gender: " ++ BC.unpack gender
    putStrLn $ "Age: " ++ show age
    putStrLn "------------------"
    ) entries
