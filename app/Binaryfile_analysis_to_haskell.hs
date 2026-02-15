import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Word

-- Skip the 19-byte file header
skipHeader :: BL.ByteString -> BL.ByteString
skipHeader = BL.drop 19

-- Parse a single row (assuming no NULLs and fields are in the defined order)
getPerson :: Get Person
getPerson = do
  -- 2 bytes: number of columns
  _ <- getWord16be

  -- id(4 bytes length + 4 bytes content)
  idLen <- getWord32be
  pid <- if idLen == 4 then fromIntegral <$> getWord32be else fail "id error"

  -- name(4 bytes length + 4 bytes content)
  nameLen <- getWord32be
  nameBytes <- getByteString (fromIntegral nameLen)
  let pname = map (toEnum . fromEnum) (BS.unpack nameBytes)

  -- age
  ageLen <- getWord32be
  age <- if ageLen == 4 then fromIntegral <$> getWord32be else fail "age error"
  
  -- gender
  genderLen <- getWord32be
  genderBytes <- getByteString (fromIntegral genderLen)
  let gender = map (toEnum . fromEnum) (BS.unpack genderBytes)
  return (Person pid pname age gender)

-- Parse all rows
parsePersons :: BL.ByteString -> [Person]
parsePersons bs =
  case runGetOrFail getPerson bs of
    Left _ -> []
    Right (rest, _, person) -> person : parsePersons rest

main :: IO ()
main = do
  content <- BL.readFile "users.bin"
  let datapart = skipHeader content
  let persons = parsePersons datapart
  print persons
