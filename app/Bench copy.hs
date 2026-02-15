{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.DeepSeq (NFData, force)
import Data.Aeson (eitherDecode, encode)
import Text.XML as X
import Text.XML.Cursor
import Data.XML.Types (Name(..))

-- 你的项目模块
import DataStructs_txt
  ( PgUserEntry(..)
  , BsonUserEntry(..)
  , UnifiedStruct(..)
  )
import qualified Data.ByteString.Char8 as BC

-- 你现有代码中的函数（在需要时把它们抽出到模块里供 bench 导入）
-- 注意：若这些函数目前只在 app Main.hs 内，请把它们挪到 src/ 下的模块中。
parsePgUsers :: Document -> [PgUserEntry]
parsePgUsers = -- ← 贴你已有的定义或从模块导入

assemblePgUsers :: [PgUserEntry] -> Document
assemblePgUsers = -- ← 贴你已有的定义或从模块导入

toUnifiedFromPg :: PgUserEntry -> UnifiedStruct
toUnifiedFromPg = -- ← 贴/导入

toUnifiedFromBson :: BsonUserEntry -> UnifiedStruct
toUnifiedFromBson = -- ← 贴/导入

updatePgFromUnified :: PgUserEntry -> UnifiedStruct -> PgUserEntry
updatePgFromUnified = -- ← 贴/导入

updateBsonFromUnified :: BsonUserEntry -> UnifiedStruct -> BsonUserEntry
updateBsonFromUnified = -- ← 贴/导入

syncAge :: UnifiedStruct -> UnifiedStruct -> UnifiedStruct
syncAge = -- ← 贴/导入

-- 与主程序相同的 JSON 读取（但在 env 中一次性执行）
loadBsonEntries :: FilePath -> IO [BsonUserEntry]
loadBsonEntries fp = do
  raw <- BL.readFile fp
  case (eitherDecode raw :: Either String [BsonUserEntry]) of
    Right xs -> pure xs
    Left _ -> error "Expect array JSON for bench to simplify"

main :: IO ()
main = defaultMain
  [ env setup $ \ ~(pgEntries, bsonEntries, binMap, bsonMap, ids) ->
      bgroup "sync-core"
        [ bench "json2xml-sync-core" $
            nf (\is ->
                let updatedPg =
                      [ updatePgFromUnified (binMap Map.! i)
                        ( syncAge
                            (toUnifiedFromPg   (binMap  Map.! i))
                            (toUnifiedFromBson (bsonMap Map.! i))
                        )
                      | i <- is
                      ]
                in length updatedPg) ids -- 用 length 强制遍历，真实场景可换为生成 Document
        , bench "xml2json-sync-core" $
            nf (\is ->
                let updatedBson =
                      [ updateBsonFromUnified (bsonMap Map.! i)
                        ( syncAge
                            (toUnifiedFromBson (bsonMap Map.! i))
                            (toUnifiedFromPg   (binMap  Map.! i))
                        )
                      | i <- is
                      ]
                in BL.length (encode updatedBson)) ids -- 强制编码以避免惰性
        ]
  , env setupIO $ \ ~(xmlDocPath, jsonPath) ->
      bgroup "io-paths"
        [ bench "read-xml-parse" $
            whnfIO (X.readFile def xmlDocPath >>= \doc -> pure (force (parsePgUsers doc)))
        , bench "read-json-decode" $
            whnfIO (loadBsonEntries jsonPath >>= \xs -> pure (force xs))
        ]
  ]
  where
    xmlPath  = "postgres_users.xml"
    jsonPath = "mongo_users.json"

    -- 仅做一次的准备：解析 XML/JSON 并构造 Map/交集 id 列表
    setup :: IO ([PgUserEntry],[BsonUserEntry], Map.Map Int PgUserEntry, Map.Map Int BsonUserEntry, [Int])
    setup = do
      xmlDoc <- X.readFile def xmlPath
      let pgEntries   = parsePgUsers xmlDoc
      bsonEntries <- loadBsonEntries jsonPath
      let binMap      = Map.fromList [(userId e, e) | e <- pgEntries]
          bsonMap     = Map.fromList [(bsonId e, e) | e <- bsonEntries]
          ids         = Set.toList $ Map.keysSet binMap `Set.intersection` Map.keysSet bsonMap
      pure (pgEntries, bsonEntries, binMap, bsonMap, ids)

    -- 只用于纯 IO 路径的 env
    setupIO :: IO (FilePath, FilePath)
    setupIO = pure (xmlPath, jsonPath)
