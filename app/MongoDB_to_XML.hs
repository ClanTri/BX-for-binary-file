{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MongoDB
import Text.XML.Light
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad (forM_)

-- Convert a BSON Document to an XML Element
documentToXml :: Document -> Element
documentToXml doc = Element
  { elName = QName "document" Nothing Nothing
  , elAttribs = []
  , elContent = map bsonFieldToContent doc
  , elLine = Nothing
  }

-- Convert a single field to XML Content
bsonFieldToContent :: Field -> Content
bsonFieldToContent (k := val) = Elem $ Element
  { elName = QName (T.unpack k) Nothing Nothing
  , elAttribs = []
  , elContent = [Text $ CData CDataText (showValue val) Nothing]
  , elLine = Nothing
  }

-- BSON Value to String
showValue :: Value -> String
showValue (String s) = T.unpack s
showValue (Int32 i)  = show i
showValue (Int64 i)  = show i
showValue (Float f)  = show f
showValue (Bool b)   = show b
showValue Null       = "null"
showValue (Doc d)    = showTopElement $ documentToXml d
showValue (Array a)  = concatMap showValue a
showValue (ObjId oid) = show oid
showValue _          = "unsupported"

-- Export MongoDB data to XML file
exportToXml :: FilePath -> IO ()
exportToXml outputFile = do
  pipe <- connect (host "127.0.0.1")
  docs <- access pipe master "test" $ find (select [] "users") >>= rest
  close pipe
  let xmlData = map documentToXml docs
  let root = Element
        { elName = QName "documents" Nothing Nothing
        , elAttribs = []
        , elContent = map Elem xmlData
        , elLine = Nothing
        }
  writeFile outputFile (showTopElement root)
  putStrLn $ "Exported " ++ show (length docs) ++ " documents to " ++ outputFile

-- Entry point
main :: IO ()
main = exportToXml "output.xml"