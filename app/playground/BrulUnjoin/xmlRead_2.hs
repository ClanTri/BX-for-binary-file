{-# LANGUAGE OverloadedStrings #-}

module XMLReader where

import Text.XML.Light
import Data.Maybe (mapMaybe)
import System.IO

-- Define basic types
data Field = Field String String deriving (Show, Eq)
data Row = Row [Field] deriving (Show, Eq)
data Table = Table [Field] [Row] deriving (Show, Eq)

data Database = 
    MongoDB { dbName :: String, dbSchema :: [Field], dbContent :: [Row] }
  | PostgreSQL { dbName :: String, dbSchema :: [Field], dbContent :: [Row] }
  deriving (Show, Eq)

-- Parse XML to Database
parseField :: Element -> Maybe Field
parseField el = do
    name <- findAttr (unqual "name") el
    fieldType <- findAttr (unqual "type") el
    return $ Field name fieldType

parseRow :: Element -> Maybe Row
parseRow el = 
    return $ Row $ map parseFieldFromRow (elChildren el)
  where
    parseFieldFromRow fieldEl = 
        Field (qName $ elName fieldEl) (strContent fieldEl)

parseTable :: Element -> Maybe Table
parseTable el = do
    schemaEl <- findChild (unqual "Schema") el
    contentEl <- findChild (unqual "Content") el
    let fields = mapMaybe parseField (elChildren schemaEl)
    let rows = mapMaybe parseRow (elChildren contentEl)
    return $ Table fields rows

parseDatabase :: Element -> Maybe Database
parseDatabase el = do
    let dbType = qName (elName el)
    dbName <- findAttr (unqual "database") el
    table <- parseTable el
    return $ case dbType of
        "MongoDB" -> MongoDB dbName (getSchema table) (getContent table)
        "PostgreSQL" -> PostgreSQL dbName (getSchema table) (getContent table)
        _ -> error "Unknown database type"
  where
    getSchema (Table fields _) = fields
    getContent (Table _ rows) = rows

-- Format rows into a readable string with schema
formatRowWithSchema :: [Field] -> Row -> String
formatRowWithSchema schema (Row fields) =
    unwords $ zipWith formatField schema fields
  where
    formatField (Field name _) (Field _ value) = name ++ ":" ++ value

formatDatabaseContentWithSchema :: Database -> String
formatDatabaseContentWithSchema db =
    let schema = dbSchema db
        rows = dbContent db
    in unlines $ map (formatRowWithSchema schema) rows

-- read and print XML data
main :: IO ()
main = do
    let file = "G:\\haskell_test\\myfirstapp\\app\\playground\\BrulUnjoin\\person_information_export.xml"
    xmlContent <- readFile file
    let xmlDoc = parseXMLDoc xmlContent
    case xmlDoc of
        Nothing -> putStrLn "Failed"
        Just doc -> do
            let databases = mapMaybe parseDatabase (elChildren doc)
            putStrLn "Parsed"
            mapM_ (putStrLn . formatDatabaseContentWithSchema) databases
