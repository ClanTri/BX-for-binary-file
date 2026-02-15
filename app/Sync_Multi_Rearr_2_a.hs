import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Word
import FileStructDef_bson
import FileStructDef_bin
import UnifiedStructDef


-- | Parse entire bin file into PgBinaryFile
parseBinFile :: BS.ByteString -> PgBinaryFile
parseBinFile bs =
  let (headerBs, rest) = BS.splitAt 19 bs
      header = parsePgHeader headerBs
      entries = parseAllRecords rest
  in PgBinaryFile header entries

-- | Parse header (19 bytes)
parsePgHeader :: BS.ByteString -> PgHeader
parsePgHeader bs =
  let sig = BS.take 11 bs
      flags = bsToWord32BE (BS.take 4 (BS.drop 11 bs))
      extlen = bsToWord32BE (BS.drop 15 bs)
  in PgHeader sig flags extlen

-- | Parse all user entries until 0xFF 0xFF
parseAllRecords :: BS.ByteString -> [PgUserEntry]
parseAllRecords bs
  | BS.take 2 bs == BS.pack [0xFF, 0xFF] = []
  | BS.null bs = []
  | otherwise =
      let (entry, rest) = parsePgUserEntry bs
      in entry : parseAllRecords rest

-- | Parse a single user entry
parsePgUserEntry :: BS.ByteString -> (PgUserEntry, BS.ByteString)
parsePgUserEntry bs =
  let -- 4 fields
      (name, r1) = parsePgTextField bs
      (gender, r2) = parsePgTextField r1
      (age, r3) = parsePgIntField r2
      (uid, r4) = parsePgIntField r3
  in (PgUserEntry uid name gender age, r4)

-- | Parse a text field (4 bytes len + N bytes content)
parsePgTextField :: BS.ByteString -> (BS.ByteString, BS.ByteString)
parsePgTextField bs =
  let len = bsToWord32BE (BS.take 4 bs)
      content = BS.take (fromIntegral len) (BS.drop 4 bs)
      rest = BS.drop (4 + fromIntegral len) bs
  in (content, rest)

-- | Parse an int field (4 bytes len == 4 + 4 bytes int content)
parsePgIntField :: BS.ByteString -> (Word32, BS.ByteString)
parsePgIntField bs =
  let len = bsToWord32BE (BS.take 4 bs)
      val = bsToWord32BE (BS.take 4 (BS.drop 4 bs))
      rest = BS.drop (4 + 4) bs
  in (val, rest)

-- | Convert 4-byte big-endian ByteString to Word32
bsToWord32BE :: BS.ByteString -> Word32
bsToWord32BE bs = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0 bs


main = do
  binRaw <- BS.readFile "users.bin"
  let binStruct = parseBinFile binRaw
  print binStruct