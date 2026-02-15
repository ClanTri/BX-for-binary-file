{-# LANGUAGE OverloadedStrings #-}

module Main where

-- ========== imports ==========
-- NOTE: replicateM 未使用，可删除；保留不影响编译
-- import           Control.Monad          (replicateM)
import           Data.Int               (Int32)
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.IO      as TLIO
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Builder as BB
import           Data.Monoid ((<>))
import           System.Environment     (getArgs)
import           System.Random          (randomRIO)
import           Text.Read              (readMaybe)
import qualified Data.Text.Lazy.Builder as TB 

-- MongoDB / PostgreSQL：使用 qualified 避免同名函数冲突
import qualified Database.MongoDB               as M
import qualified Database.PostgreSQL.Simple     as PG
import           Database.PostgreSQL.Simple     (Only(..))

-- ================= common =================
chunk :: Int -> [a] -> [[a]]
chunk k xs
  | k <= 0    = error "chunk size must be positive"
  | null xs   = []
  | otherwise = take k xs : chunk k (drop k xs)

namePool :: [T.Text]
namePool =
  [ "Alice","Bob","Carol","Dave","Eve","Frank","Grace","Heidi","Ivan","Judy"
  , "Mallory","Niaj","Olivia","Peggy","Rupert","Sybil","Trent","Victor","Wendy","Yvonne"
  , "Haruto","Yui","Ren","Aoi","Sota","Hina","Itsuki","Rio","Takumi","Sakura"
  , "Wei","Ming","Hua","Li Hua","Zhang Wei","Wang Fang","Chen Jie","Liu Yang","Zhao Min","Sun Lei"
  ]

randName :: IO T.Text
randName = do
  i <- randomRIO (0, length namePool - 1)
  pure (namePool !! i)

randAge :: IO Int32
randAge = fromIntegral <$> randomRIO (16, 55 :: Int)

randGender :: IO T.Text
randGender = do
  b <- randomRIO (0,1 :: Int)
  pure $ if b == 0 then "Male" else "Female"

-- ================= MongoDB =================
-- 当前集合中最大的 _id
currentMaxId :: M.Pipe -> M.Database -> M.Collection -> IO Int32
currentMaxId pipe db coll = M.access pipe M.master db $ do
  cur  <- M.find (M.select [] coll){ M.sort = ["_id" M.=: (-1 :: Int32)], M.limit = 1 }
  docs <- M.rest cur
  pure $ case docs of
    (d:_) -> maybe 0 id (M.lookup "_id" d :: Maybe Int32)
    _     -> 0

-- 生成一条 Mongo 文档，并返回 (id,name,ageMongo) 供 PG 使用
mkMongoDoc :: Int32 -> IO (M.Document, (Int32, T.Text, Int32))
mkMongoDoc newId = do
  nm <- randName
  a  <- randAge
  let doc = [ "_id" M.=: newId
            , "name" M.=: nm
            , "age"  M.=: a
            ]
  pure (doc, (newId, nm, a))

insertMongoN
  :: M.Pipe -> M.Database -> M.Collection -> Int
  -> IO (Int, [(Int32, T.Text, Int32)])
insertMongoN pipe db coll n = do
  startId <- currentMaxId pipe db coll
  let ids = [startId + 1 .. startId + fromIntegral n]
  pairs <- mapM mkMongoDoc ids
  let docs   = map fst pairs
      tuples = map snd pairs
  inserted <- M.access pipe M.master db $ do
    let batches = chunk 1000 docs
    counts <- mapM (fmap length . M.insertMany coll) batches
    pure (sum counts)
  pure (inserted, tuples)

-- ================= PostgreSQL =================
-- public.users(id int primary key, name varchar(32), gender varchar(8), age int)
-- NOTE: 为了导出 PG 的二进制，我们把返回值改为 (影响行数, 用于写入的四元组列表)
insertPostgres :: PG.Connection -> [(Int32, T.Text, Int32)] -> IO (Int, [(Int, T.Text, T.Text, Int)])
insertPostgres conn triplets = do
  rows <- mapM
    (\(i,nm,ageM) -> do
        g <- randGender
        let ageP = fromIntegral ageM + 1 :: Int
        pure (fromIntegral i :: Int, nm, g, ageP)
    ) triplets
  let q = "INSERT INTO public.users (id, name, gender, age) \
          \VALUES (?,?,?,?) ON CONFLICT (id) DO NOTHING"
  cnt <- fromIntegral <$> PG.executeMany conn q rows
  pure (cnt, rows)

pgCount :: PG.Connection -> IO Int
pgCount conn = do
  [Only c] <- PG.query_ conn "SELECT COUNT(*) FROM public.users"
  pure c

pgNewest :: PG.Connection -> IO [(Int, T.Text, T.Text, Int)]
pgNewest conn =
  PG.query_ conn "SELECT id, name, gender, age FROM public.users ORDER BY id DESC LIMIT 5"

-- ================= Binary Export (custom format) =================
-- 通用：以 uint16BE 写入 UTF-8 文本长度 + 原始字节；超过 65535 字节则截断
putText16 :: T.Text -> BB.Builder
putText16 t =
  let bs = encodeUtf8 t
      (len16, bs') =
        if BS.length bs > 65535
          then (65535, BS.take 65535 bs)
          else (fromIntegral (BS.length bs) :: Int, bs)
  in BB.word16BE (fromIntegral len16) <> BB.byteString bs'

-- Mongo 二进制：MAGIC="MUSR" | VER=int32BE(1) | COUNT=int32BE(n) | n * [id i32BE | name u16BE+utf8 | age i32BE]
buildMongoBin :: [(Int32, T.Text, Int32)] -> BL.ByteString
buildMongoBin triples =
  let header = BB.byteString "MUSR" <> BB.int32BE 1 <> BB.int32BE (fromIntegral (length triples))
      one (i, nm, ageM) =
         BB.int32BE i <> putText16 nm <> BB.int32BE ageM
  in BB.toLazyByteString $ header <> mconcat (map one triples)

writeMongoBin :: FilePath -> [(Int32, T.Text, Int32)] -> IO ()
writeMongoBin fp triples = BL.writeFile fp (buildMongoBin triples)

-- Postgres Binary：MAGIC="PUSR" | VER=int32BE(1) | COUNT | n * [id i32BE | name | gender | age i32BE]
buildPostgresBin :: [(Int, T.Text, T.Text, Int)] -> BL.ByteString
buildPostgresBin rows =
  let header = BB.byteString "PUSR" <> BB.int32BE 1 <> BB.int32BE (fromIntegral (length rows))
      one (i, nm, g, ageP) =
         BB.int32BE (fromIntegral i) <> putText16 nm <> putText16 g <> BB.int32BE (fromIntegral ageP)
  in BB.toLazyByteString $ header <> mconcat (map one rows)

writePostgresBin :: FilePath -> [(Int, T.Text, T.Text, Int)] -> IO ()
writePostgresBin fp rows = BL.writeFile fp (buildPostgresBin rows)

-- ================= JSON Export (Mongo) =================
--  {"_id":Int,"name":String,"age":Int}
jsonEscape :: T.Text -> T.Text
jsonEscape = T.concatMap f
  where
    f '\"' = "\\\""
    f '\\' = "\\\\"
    f '\b' = "\\b"
    f '\f' = "\\f"
    f '\n' = "\\n"
    f '\r' = "\\r"
    f '\t' = "\\t"
    f c    = T.singleton c

buildMongoJSON :: [(Int32, T.Text, Int32)] -> TL.Text
buildMongoJSON triples =
  let kv k v more =
        TB.fromText ("\"" <> k <> "\":") <> v <>
        (if more then TB.fromText "," else mempty)
      one (i,nm,ageM) =
           TB.fromText "{"
        <> kv "_id"  (TB.fromString (show i)) True
        <> kv "name" (TB.fromText ("\"" <> jsonEscape nm <> "\"")) True
        <> kv "age"  (TB.fromString (show ageM)) False
        <> TB.fromText "}"
      items = map one triples
      commaSep []     = []
      commaSep [x]    = [x]
      commaSep (x:xs) = (x <> TB.fromText ",") : commaSep xs
  in TB.toLazyText $ TB.fromText "[" <> mconcat (commaSep items) <> TB.fromText "]"

writeMongoJSON :: FilePath -> [(Int32, T.Text, Int32)] -> IO ()
writeMongoJSON fp triples = TLIO.writeFile fp (buildMongoJSON triples)

-- ================= XML(+XSD) Export (PostgreSQL) =================
xsdContent :: T.Text
xsdContent = T.unlines
  [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
  , "<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\""
  , "           targetNamespace=\"http://example.org/users\""
  , "           xmlns=\"http://example.org/users\""
  , "           elementFormDefault=\"qualified\">"
  , "  <xs:simpleType name=\"Gender\">"
  , "    <xs:restriction base=\"xs:string\">"
  , "      <xs:enumeration value=\"Male\"/>"
  , "      <xs:enumeration value=\"Female\"/>"
  , "    </xs:restriction>"
  , "  </xs:simpleType>"
  , "  <xs:element name=\"users\">"
  , "    <xs:complexType>"
  , "      <xs:sequence>"
  , "        <xs:element name=\"user\" maxOccurs=\"unbounded\">"
  , "          <xs:complexType>"
  , "            <xs:sequence>"
  , "              <xs:element name=\"id\" type=\"xs:int\"/>"
  , "              <xs:element name=\"name\">"
  , "                <xs:simpleType>"
  , "                  <xs:restriction base=\"xs:string\">"
  , "                    <xs:maxLength value=\"32\"/>"
  , "                  </xs:restriction>"
  , "                </xs:simpleType>"
  , "              </xs:element>"
  , "              <xs:element name=\"gender\" type=\"Gender\"/>"
  , "              <xs:element name=\"age\" type=\"xs:int\"/>"
  , "            </xs:sequence>"
  , "          </xs:complexType>"
  , "        </xs:element>"
  , "      </xs:sequence>"
  , "    </xs:complexType>"
  , "  </xs:element>"
  , "</xs:schema>"
  ]

xmlEscape :: T.Text -> T.Text
xmlEscape = T.concatMap g
  where
    g '<'  = "&lt;"
    g '>'  = "&gt;"
    g '&'  = "&amp;"
    g '\"' = "&quot;"
    g '\'' = "&apos;"
    g c    = T.singleton c

buildUsersXML :: [(Int, T.Text, T.Text, Int)] -> TL.Text
buildUsersXML rows =
  let header :: TL.Text
      header =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <>
        "<users xmlns=\"http://example.org/users\"\n"   <>
        "       xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n" <>
        "       xsi:schemaLocation=\"http://example.org/users postgres_users.xsd\">\n"
      footer :: TL.Text
      footer = "</users>\n"
      userElem (i,nm,g,ageP) =
        "  <user>\n"                                               <>
        "    <id>"     <> TL.pack (show i)                 <> "</id>\n"     <>
        "    <name>"   <> TL.fromStrict (xmlEscape nm)     <> "</name>\n"   <>
        "    <gender>" <> TL.fromStrict (xmlEscape g)      <> "</gender>\n" <>
        "    <age>"    <> TL.pack (show ageP)              <> "</age>\n"    <>
        "  </user>\n"
  in  header <> mconcat (map userElem rows) <> footer

writePostgresXML :: FilePath -> FilePath -> [(Int, T.Text, T.Text, Int)] -> IO ()
writePostgresXML xmlPath xsdPath rows = do
  TIO.writeFile xsdPath xsdContent
  TLIO.writeFile xmlPath (buildUsersXML rows)

-- ================= main =================
main :: IO ()
main = do
  args <- getArgs
  let n = case args of
            (x:_) -> maybe 100 id (readMaybe x)
            _     -> 100

  -- ---- MongoDB ----
  pipe <- M.connect (M.host "127.0.0.1")
  let mdb   = "test"  :: M.Database
      mcoll = "users" :: M.Collection

  (insertedM, tuples) <- insertMongoN pipe mdb mcoll n

  totalM <- M.access pipe M.master mdb $ M.count (M.select [] mcoll)
  newestM <- M.access pipe M.master mdb $ do
    cur <- M.find (M.select [] mcoll){ M.sort = ["_id" M.=: (-1 :: Int32)], M.limit = 5 }
    M.rest cur

  -- ---- PostgreSQL ----
  -- if needed add password=...）
  let pgConnStr = "host=127.0.0.1 port=5432 dbname=postgres user=postgres password=621307"
  conn <- PG.connectPostgreSQL pgConnStr

  (insertedP, usedRows) <- insertPostgres conn tuples
  totalP    <- pgCount conn
  newestP   <- pgNewest conn

  -- ---- Close Connection ----
  M.close pipe
  PG.close conn

  -- ---- Export File----
  writeMongoBin     "mongo_users.bson"      tuples         -- binary（Mongo）
  writePostgresBin  "postgres_users.bin"   usedRows       -- binary（PostgreSQL）
  writeMongoJSON    "mongo_users.json"     tuples         -- JSON（Mongo）
  writePostgresXML  "postgres_users.xml"   "postgres_users.xsd" usedRows  -- XML+XSD（PostgreSQL）


  -- ---- Print Log ----
  putStrLn $ "[MongoDB] Inserted " ++ show insertedM ++ " docs into " ++ T.unpack mcoll
  putStrLn $ "[MongoDB] Now total documents in " ++ T.unpack mcoll ++ " = " ++ show totalM
  putStrLn   "[MongoDB] Newest 5 documents:"
  mapM_ print newestM

  putStrLn $ "[PostgreSQL] Inserted " ++ show insertedP ++ " rows into public.users"
  putStrLn $ "[PostgreSQL] Now total rows = " ++ show totalP
  putStrLn   "[PostgreSQL] Newest 5 rows:"
  mapM_ print newestP

  putStrLn   "[Export] Wrote mongo_users.bin"
  putStrLn   "[Export] Wrote postgres_users.bin"
  putStrLn   "[Export] Wrote mongo_users.json"
  putStrLn   "[Export] Wrote postgres_users.xml and postgres_users.xsd"