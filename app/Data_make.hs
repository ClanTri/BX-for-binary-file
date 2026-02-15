{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad          (replicateM)
import           Data.Int               (Int32)
import qualified Data.Text              as T
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
insertPostgres :: PG.Connection -> [(Int32, T.Text, Int32)] -> IO Int
insertPostgres conn triplets = do
  rows <- mapM
    (\(i,nm,ageM) -> do
        g <- randGender
        let ageP = fromIntegral ageM + 1 :: Int
        pure (fromIntegral i :: Int, nm, g, ageP)
    ) triplets
  let q = "INSERT INTO public.users (id, name, gender, age) \
          \VALUES (?,?,?,?) ON CONFLICT (id) DO NOTHING"
  fromIntegral <$> PG.executeMany conn q rows

pgCount :: PG.Connection -> IO Int
pgCount conn = do
  [Only c] <- PG.query_ conn "SELECT COUNT(*) FROM public.users"
  pure c

pgNewest :: PG.Connection -> IO [(Int, T.Text, T.Text, Int)]
pgNewest conn =
  PG.query_ conn "SELECT id, name, gender, age FROM public.users ORDER BY id DESC LIMIT 5"

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

  insertedP <- insertPostgres conn tuples
  totalP    <- pgCount conn
  newestP   <- pgNewest conn

  -- 关闭连接（注意用各自模块的 close）
  M.close pipe
  PG.close conn

  -- ---- 输出 ----
  putStrLn $ "[MongoDB] Inserted " ++ show insertedM ++ " docs into " ++ T.unpack mcoll
  putStrLn $ "[MongoDB] Now total documents in " ++ T.unpack mcoll ++ " = " ++ show totalM
  putStrLn   "[MongoDB] Newest 5 documents:"
  mapM_ print newestM

  putStrLn $ "[PostgreSQL] Inserted " ++ show insertedP ++ " rows into public.users"
  putStrLn $ "[PostgreSQL] Now total rows = " ++ show totalP
  putStrLn   "[PostgreSQL] Newest 5 rows:"
  mapM_ print newestP
