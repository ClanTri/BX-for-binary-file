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

-- Postgres 二进制：MAGIC="PUSR" | VER=int32BE(1) | COUNT | n * [id i32BE | name | gender | age i32BE]
buildPostgresBin :: [(Int, T.Text, T.Text, Int)] -> BL.ByteString
buildPostgresBin rows =
  let header = BB.byteString "PUSR" <> BB.int32BE 1 <> BB.int32BE (fromIntegral (length rows))
      one (i, nm, g, ageP) =
         BB.int32BE (fromIntegral i) <> putText16 nm <> putText16 g <> BB.int32BE (fromIntegral ageP)
  in BB.toLazyByteString $ header <> mconcat (map one rows)

writePostgresBin :: FilePath -> [(Int, T.Text, T.Text, Int)] -> IO ()
writePostgresBin fp rows = BL.writeFile fp (buildPostgresBin rows)

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
  -- 按需修改连接串（如果设置了密码请加上 password=...）
  let pgConnStr = "host=127.0.0.1 port=5432 dbname=postgres user=postgres password=621307"
  conn <- PG.connectPostgreSQL pgConnStr

  (insertedP, usedRows) <- insertPostgres conn tuples
  totalP    <- pgCount conn
  newestP   <- pgNewest conn

  -- ---- 关闭连接 ----
  M.close pipe
  PG.close conn

  -- ---- 导出二进制文件 ----
  writeMongoBin    "mongo_users.bson"    tuples
  writePostgresBin "postgres_users.bin" usedRows

  -- ---- 输出 ----
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
