{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TemplateHaskell #-}

module Main where

import Generics.BiGUL
import Generics.BiGUL.TH
import Text.XML.Light
import Data.Maybe (mapMaybe)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Map (Map)
import qualified Data.Map as Map

-- 明确分离 Schema 定义和数据内容
data SchemaField = SchemaField { sName :: String, sType :: String } deriving (Show, Eq, Data, Typeable)
data ContentField = ContentField { cName :: String, cValue :: String } deriving (Show, Eq, Data, Typeable)

data Row = Row [ContentField] deriving (Show, Eq, Data, Typeable)
data Table = Table [SchemaField] [Row] deriving (Show, Eq, Data, Typeable)

data Database = 
    MongoDB { dbName :: String, dbSchema :: [SchemaField], dbContent :: [Row] }
  | PostgreSQL { dbName :: String, dbSchema :: [SchemaField], dbContent :: [Row] }
  deriving (Show, Eq, Data, Typeable)

-- 定义核心同步逻辑：同步 _id <-> id 和其他字段
syncDatabases :: BiGUL Database Database
syncDatabases = Case
  [ -- MongoDB -> PostgreSQL
    $(normalSV [p| MongoDB name schema rows |] [p| PostgreSQL name' schema' rows' |] [| True |])
      ==> Update $ \mongodb postgres -> 
            let updatedSchema = syncSchema (dbSchema mongodb)
                updatedRows   = map (syncRow (dbSchema mongodb)) (dbContent mongodb)
            in  PostgreSQL (dbName mongodb) updatedSchema updatedRows
  , -- PostgreSQL -> MongoDB
    $(normalSV [p| PostgreSQL name schema rows |] [p| MongoDB name' schema' rows' |] [| True |])
      ==> Update $ \postgres mongodb -> 
            let updatedSchema = syncSchema (dbSchema postgres)
                updatedRows   = map (syncRow (dbSchema postgres)) (dbContent postgres)
            in  MongoDB (dbName postgres) updatedSchema updatedRows
  ]
  where
    -- Schema 同步规则（处理字段名和类型映射）
    syncSchema :: [SchemaField] -> [SchemaField]
    syncSchema schema = map convertField schema
      where
        convertField (SchemaField "_id" "str")       = SchemaField "id" "integer"
        convertField (SchemaField name "character varying") = SchemaField name "str"
        convertField f                              = f

    -- Row 同步规则（处理数据值转换）
    syncRow :: [SchemaField] -> Row -> Row
    syncRow schema (Row fields) = Row $ map (convertContentField schema) fields
      where
        convertContentField schemaFields (ContentField "_id" val) = 
          ContentField "id" val  -- 直接映射 _id -> id（假设值兼容）
        convertContentField schemaFields (ContentField name val) = 
          case lookupSchemaType name schemaFields of
            "character varying" -> ContentField name val  -- 保持值不变
            _                   -> ContentField name val

        lookupSchemaType name schema = 
          head [ sType | SchemaField n sType <- schema, n == name ]

parseSchemaField :: Element -> Maybe SchemaField
parseSchemaField el = do
    name <- findAttr (unqual "name") el
    typ  <- findAttr (unqual "type") el
    return $ SchemaField name typ

parseContentField :: Element -> Maybe ContentField
parseContentField el = 
    return $ ContentField (qName $ elName el) (strContent el)

parseRow :: Element -> Maybe Row
parseRow el = Row <$> mapM parseContentField (elChildren el)

parseDatabase :: Element -> Maybe Database
parseDatabase el = do
    let dbType = qName (elName el)
    dbName <- findAttr (unqual "database") el
    schema <- mapM parseSchemaField =<< children (findChild (unqual "Schema") el)
    rows   <- mapM parseRow =<< children (findChild (unqual "Content") el)
    return $ case dbType of
        "MongoDB"     -> MongoDB dbName schema rows
        "PostgreSQL" -> PostgreSQL dbName schema rows
        _            -> error "Unknown database type"
  where
    children = maybe [] elChildren

main :: IO ()
main = do
    -- 1. 解析 XML
    xmlContent <- readFile "person_information_export.xml"
    let xmlDoc = parseXMLDoc xmlContent
    case xmlDoc >>= mapM parseDatabase . elChildren of
      Nothing -> putStrLn "XML 解析失败"
      Just [mongo, postgres] -> do
        -- 2. 正向同步（MongoDB -> PostgreSQL）
        let updatedPostgres = put syncDatabases mongo postgres
        putStrLn "更新后的 PostgreSQL:"
        print updatedPostgres

        -- 3. 反向同步（PostgreSQL -> MongoDB）
        let updatedMongo = put syncDatabases postgres mongo
        putStrLn "更新后的 MongoDB:"
        print updatedMongo

      _ -> putStrLn "XML 格式不符合预期"