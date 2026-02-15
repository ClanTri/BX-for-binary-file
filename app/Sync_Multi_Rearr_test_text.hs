{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Sync_Multi_Rearr_test_text
  ( -- for benchmarks / reuse
    loadBsonEntries
  , parsePgUsers
  , assemblePgUsers
  , toUnifiedFromPg
  , toUnifiedFromBson
  , updatePgFromUnified
  , updateBsonFromUnified
  , syncAge
    -- optional CLI
  , runCLI
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Aeson
import Data.Aeson.Types
import Text.XML
import Text.XML.Cursor
import Data.XML.Types (Name(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter

import           DataStructs_txt
  ( PgUserEntry(..)
  , BsonUserEntry(..)
  , UnifiedStruct(..)
  )

--------------------------------------------------------------------------------
-- JSON
--------------------------------------------------------------------------------

instance FromJSON BsonUserEntry where
  parseJSON = withObject "BsonUserEntry" $ \o ->
    BsonUserEntry <$> o .: "_id" <*> o .: "name" <*> o .: "age"
-- 注意：这里的构造顺序是 (id, name, age)

instance ToJSON BsonUserEntry where
  toJSON (BsonUserEntry i n a) =
    object ["_id" .= i, "name" .= n, "age" .= a]

newtype BsonTop = BsonTop { dataField :: [BsonUserEntry] }
instance FromJSON BsonTop where
  parseJSON = withObject "BsonTop" $ \o ->
    BsonTop <$> o .: "data"

-- 既支持顶层数组，也支持 { "data": [...] }
loadBsonEntries :: FilePath -> IO [BsonUserEntry]
loadBsonEntries fp = do
  raw <- BL.readFile fp
  case (eitherDecode raw :: Either String [BsonUserEntry]) of
    Right xs -> pure xs
    Left _ -> case (eitherDecode raw :: Either String BsonTop) of
      Right (BsonTop xs) -> pure xs
      Left e             -> error $ "[JSON] cannot decode: " ++ e

--------------------------------------------------------------------------------
-- XML
--------------------------------------------------------------------------------

laxElem :: T.Text -> Axis
laxElem ln = checkElement (\e -> nameLocalName (elementName e) == ln)

parsePgUsers :: Document -> [PgUserEntry]
parsePgUsers doc =
  let cur     = fromDocument doc
      pick t  c = T.unpack <$> listToMaybe (c $/ laxElem t &/ content)
      pickI t c = read <$> pick t c
      users   = cur $// laxElem "user"
  in [ PgUserEntry i n g a
     | c <- users
     , Just i <- [pickI "id" c]
     , Just n <- [pick  "name" c]
     , Just g <- [pick  "gender" c]
     , Just a <- [pickI "age" c]
     ]
  where
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x

-- [PgUserEntry] -> XML
assemblePgUsers :: [PgUserEntry] -> Document
assemblePgUsers entries =
  Document (Prologue [] Nothing []) root []
  where
    root = Element "users" mempty (map toNode entries)
    toNode (PgUserEntry i n g a) =
      NodeElement (Element "user" mempty
        [ NodeElement (Element "id"     mempty [NodeContent (T.pack (show i))])
        , NodeElement (Element "name"   mempty [NodeContent (T.pack n)])
        , NodeElement (Element "gender" mempty [NodeContent (T.pack g)])
        , NodeElement (Element "age"    mempty [NodeContent (T.pack (show a))])
        ])

--------------------------------------------------------------------------------
-- BiGUL：只同步年龄
--------------------------------------------------------------------------------

ageLens :: BiGUL UnifiedStruct Int
ageLens = $(rearrS [| \(UnifiedStruct _ a _) -> a |]) Replace

syncAge :: UnifiedStruct -> UnifiedStruct -> UnifiedStruct
syncAge target source =
  case get ageLens source of
    Just srcAge ->
      case put ageLens target srcAge of
        Just t' -> t'
        Nothing -> target
    Nothing -> target

toUnifiedFromPg :: PgUserEntry -> UnifiedStruct
toUnifiedFromPg (PgUserEntry i n _g a) = UnifiedStruct i a n

toUnifiedFromBson :: BsonUserEntry -> UnifiedStruct
toUnifiedFromBson (BsonUserEntry i n a) = UnifiedStruct i a n
-- 注意：这里 BsonUserEntry 的字段顺序是 (id, name, age)

updatePgFromUnified :: PgUserEntry -> UnifiedStruct -> PgUserEntry
updatePgFromUnified orig u = orig { userAge = age u }

updateBsonFromUnified :: BsonUserEntry -> UnifiedStruct -> BsonUserEntry
updateBsonFromUnified orig u = orig { bsonAge = age u }

--------------------------------------------------------------------------------
-- CLI（可选）
--------------------------------------------------------------------------------

runCLI :: IO ()
runCLI = do
  putStrLn "input: json2xml or xml2json"
  dir <- getLine

  bsonEntries <- loadBsonEntries "mongo_users.json"
  xmlDoc <- Text.XML.readFile def "postgres_users.xml"
  let pgEntries = parsePgUsers xmlDoc

  putStrLn $ "[DEBUG] JSON users: " ++ show (length bsonEntries)
  mapM_ (\e -> putStrLn $ "  json -> id=" ++ show (bsonId e)
                         ++ ", name=" ++ bsonName e
                         ++ ", age="  ++ show (bsonAge e)) bsonEntries
  putStrLn $ "[DEBUG] XML users:  " ++ show (length pgEntries)
  mapM_ (\e -> putStrLn $ "  xml  -> id=" ++ show (userId e)
                         ++ ", name=" ++ userName e
                         ++ ", age="  ++ show (userAge e)) pgEntries

  let pgMap   = Map.fromList [(userId e, e) | e <- pgEntries]
      bsonMap = Map.fromList [(bsonId e, e) | e <- bsonEntries]
      ids     = Set.toList $ Map.keysSet pgMap `Set.intersection` Map.keysSet bsonMap

  putStrLn $ "[DEBUG] common ids: " ++ show ids

  case dir of
    "json2xml" -> do
      let updatedPg =
            [ updatePgFromUnified (pgMap Map.! i)
              (syncAge (toUnifiedFromPg   (pgMap  Map.! i))
                       (toUnifiedFromBson (bsonMap Map.! i)))
            | i <- ids
            ]
      Text.XML.writeFile def "users_synced.xml" (assemblePgUsers (if null updatedPg then pgEntries else updatedPg))
      putStrLn "Written users_synced.xml"

    "xml2json" -> do
      let updatedBson =
            [ updateBsonFromUnified (bsonMap Map.! i)
              (syncAge (toUnifiedFromBson (bsonMap Map.! i))
                       (toUnifiedFromPg   (pgMap  Map.! i)))
            | i <- ids
            ]
      BL.writeFile "users_synced.json" (encode (if null updatedBson then bsonEntries else updatedBson))
      putStrLn "Written users_synced.json"

    _ -> putStrLn "Unknown direction"
