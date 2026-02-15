{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, TemplateHaskell #-}

module Main where

import Generics.BiGUL
import Generics.BiGUL.TH (update)
import Generics.BiGUL.Interpreter (put)
import Text.XML.Light
import Data.Maybe (mapMaybe)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.Applicative ((<|>))
import Text.Read (readMaybe)
import System.IO

-- **数据结构**
data Row = Row { rId :: String, rName :: String, rAge :: String, rGender :: String }
  deriving (Show, Eq, Data, Typeable)

data Database =
    MongoDB { dbName :: String, dbContent :: [Row] }
  | PostgreSQL { dbName :: String, dbContent :: [Row] }
  deriving (Show, Eq, Data, Typeable)

-- **BiGUL 变换：_id <-> id**
syncId :: BiGUL (String, String, String, String) (String, String, String, String)
syncId = $(update
  [p| (id, name, age, gender) |]
  [p| (id, name, age, gender) |]
  [d| id     = emb (read :: String -> Int) (\_ newId -> show newId)
      name   = Replace
      age    = Replace
      gender = Replace
  |] )

-- **XML 解析**
findChildText :: String -> Element -> Maybe String
findChildText tag el = fmap strContent (findChild (unqual tag) el)

parseRow :: Element -> Maybe Row
parseRow el = do
    rId <- findChildText "_id" el <|> findChildText "id" el
    rName <- findChildText "name" el
    rAge <- findChildText "age" el
    rGender <- findChildText "gender" el
    return $ Row rId rName rAge rGender

parseDatabase :: Element -> Maybe Database
parseDatabase el = do
    let dbType = qName (elName el)
    dbName <- findAttr (unqual "database") el
    rows <- mapM parseRow (elChildren =<< findChild (unqual "Content") el)
    return $ case dbType of
        "MongoDB"     -> Just $ MongoDB dbName rows
        "PostgreSQL"  -> Just $ PostgreSQL dbName rows
        _             -> Nothing

-- **XML 生成**
rowToXML :: Row -> Element
rowToXML (Row rid name age gender) =
    Element { elName = unqual "Row"
            , elAttribs = []
            , elContent = map Elem [
                  mkElem "id" rid,
                  mkElem "name" name,
                  mkElem "age" age,
                  mkElem "gender" gender
              ]
            , elLine = Nothing
            }
  where
    mkElem tag text = Element (unqual tag) [] [Text (CData CDataText text Nothing)] Nothing

databaseToXML :: Database -> Element
databaseToXML (MongoDB name rows) =
    Element (unqual "MongoDB") [Attr (unqual "database") name] [Elem contentElem] Nothing
  where
    contentElem = Element (unqual "Content") [] (map Elem (map rowToXML rows)) Nothing
databaseToXML (PostgreSQL name rows) =
    Element (unqual "PostgreSQL") [Attr (unqual "database") name] [Elem contentElem] Nothing
  where
    contentElem = Element (unqual "Content") [] (map Elem (map rowToXML rows)) Nothing

-- **主函数**
main :: IO ()
main = do
    xmlContent <- readFile "person_information_export.xml"
    let xmlDoc = parseXMLDoc xmlContent
    case xmlDoc >>= mapM parseDatabase . elChildren of
      Nothing -> putStrLn "XML 解析失败"
      Just [MongoDB mName mRows, PostgreSQL pName pRows] -> do
        -- **MongoDB -> PostgreSQL**
        let convertedRows = map (\row -> put syncId (rId row, rName row, rAge row, rGender row) (rId row, rName row, rAge row, rGender row)) mRows
        let updatedPostgres = PostgreSQL pName (map (\(pid, name, age, gender) -> Row pid name age gender) convertedRows)

        -- **PostgreSQL -> MongoDB**
        let reverseConvertedRows = map (\row -> put syncId (rId row, rName row, rAge row, rGender row) (rId row, rName row, rAge row, rGender row)) pRows
        let updatedMongo = MongoDB mName (map (\(mid, name, age, gender) -> Row mid name age gender) reverseConvertedRows)

        -- **写回 XML**
        let updatedXml = unode "Databases" [databaseToXML updatedMongo, databaseToXML updatedPostgres]
        writeFile "updated_person_information.xml" (ppTopElement updatedXml)

        putStrLn "XML 变换完成，已保存至 updated_person_information.xml"
      _ -> putStrLn "XML 格式不符合预期"
