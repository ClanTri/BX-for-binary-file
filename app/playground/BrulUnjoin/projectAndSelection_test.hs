{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module XMLBiDirectional where

import Generics.BiGUL
import Generics.BiGUL.TH
import Text.XML.Light
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad
import System.IO

-- Define basic types
data Field = Field String String deriving (Show, Eq)
data Row = Row [Field] deriving (Show, Eq)
data Table = Table [Field] [Row] deriving (Show, Eq)

data Database = 
    MongoDB { dbName :: String, schema :: [Field], content :: [Row] }
  | PostgreSQL { dbName :: String, schema :: [Field], content :: [Row] }
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
        "MongoDB" -> MongoDB dbName (schema table) (content table)
        "PostgreSQL" -> PostgreSQL dbName (schema table) (content table)
        _ -> error "Unknown database type"

-- Bi-directional transformation logic
-- Define the BiGUL program to align MongoDB and PostgreSQL data
transformRow :: BiGUL Row Row
transformRow = emb g p
  where
    g (Row fields) = Row fields
    p (Row _) (Row newFields) = Row newFields

transformContent :: BiGUL [Row] [Row]
transformContent = Case
    [ $(normal [| \src tgt -> length src == length tgt |] [p| _ |]) ==> 
        $(rearrAndUpdate [p| src |] [p| tgt |] [d| src = transformRow |])
    ]

transformDatabase :: BiGUL Database Database
transformDatabase = Case
    [ $(normal [| \src tgt -> dbName src == dbName tgt |] [p| _ |]) ==> 
        $(rearrAndUpdate [p| MongoDB name schema content |] [p| MongoDB name schema content |]
                          [d| name = Replace
                              schema = Replace
                              content = transformContent |])
    , $(normal [| \src tgt -> dbName src == dbName tgt |] [p| _ |]) ==> 
        $(rearrAndUpdate [p| PostgreSQL name schema content |] [p| PostgreSQL name schema content |]
                          [d| name = Replace
                              schema = Replace
                              content = transformContent |])
    ]

-- Main function to test XML bi-directional transformation
main :: IO ()
main = do
    let filePath = "G:\\haskell_test\\myfirstapp\\app\\playground\\BrulUnjoin\\person_information_export.xml"
    xmlContent <- readFile filePath
    let xmlDoc = parseXMLDoc xmlContent
    case xmlDoc of
        Nothing -> putStrLn "Failed to parse XML"
        Just doc -> do
            let databases = mapMaybe parseDatabase (elChildren doc)
            putStrLn "Parsed Databases:"
            mapM_ print databases

            -- Perform bi-directional transformation
            let mongoDB = head databases
            let postgresDB = last databases

            case put transformDatabase mongoDB postgresDB of
                Nothing -> putStrLn "Transformation failed"
                Just transformedDB -> do
                    putStrLn "Transformed Database:"
                    print transformedDB

            case get transformDatabase mongoDB of
                Nothing -> putStrLn "Reverse Transformation failed"
                Just reverseTransformed -> do
                    putStrLn "Reverse Transformed Database:"
                    print reverseTransformed
