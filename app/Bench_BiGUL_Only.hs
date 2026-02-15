{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Criterion.Types (Config(..), Verbosity(..))
import Criterion.Main (defaultMainWith, defaultConfig)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Data.List       as List

import Control.DeepSeq (force)
import Control.Exception (evaluate)

import qualified Text.XML as X
import qualified DataStructs_txt as DTxt

import DataStructs (PgUserEntry(..), BsonUserEntry(..), UnifiedStruct(..))

-- bin <-> BSON
import Sync_Multi_Rearr_test
  ( parseBinFile, parseBsonArray
  , toUnifiedFromPg, toUnifiedFromBson
  , syncAge
  )

-- text <-> JSON
import qualified Sync_Multi_Rearr_test_text as Txt

main :: IO ()
main = defaultMainWith timeConfig benchmarks
  where
    timeConfig = defaultConfig
      { timeLimit = 60.0
      , resamples = 1000
      , verbosity = Normal
      }

    benchmarks =
      [ --------------------------------------------------------------------------
        -- BiGUL core only (BIN <-> BSON)
        env setupBinPairs $ \pairs ->
          bgroup "binary-sync/bigul-core-only"
            [ bench "bin2bson-syncAge-only" $
                nf runBin_bin2bson pairs
            , bench "bson2bin-syncAge-only" $
                nf runBin_bson2bin pairs
            ]

        --------------------------------------------------------------------------
        -- BiGUL core only (XML <-> JSON)
      , env setupTxtPairs $ \pairs ->
          bgroup "text-sync/bigul-core-only"
            [ bench "xml2json-syncAge-only" $
                nf runTxt_xml2json pairs
            , bench "json2xml-syncAge-only" $
                nf runTxt_json2xml pairs
            ]
      ]

    --------------------------------------------------------------------------
    -- Paths (adjust if needed)
    binPath  = "postgres_users.bin"
    bsonPath = "mongo_users.bson"
    xmlPath  = "postgres_users.xml"
    jsonPath = "mongo_users.json"

    --------------------------------------------------------------------------
    -- env: prepare pairs for bin bigul-only
    -- pairs are (uT, uS) so that syncAge uT uS corresponds to "bin2bson" direction in your previous setup
    setupBinPairs :: IO [(UnifiedStruct, UnifiedStruct)]
    setupBinPairs = do
      binRaw  <- BS.readFile binPath
      bsonRaw <- BS.readFile bsonPath
      let binEntries  = parseBinFile   binRaw
          bsonEntries = parseBsonArray bsonRaw

          binMap  = Map.fromList [(userId e, e) | e <- binEntries]
          bsonMap = Map.fromList [(bsonId e, e) | e <- bsonEntries]
          ids     = Set.toList $ Map.keysSet binMap `Set.intersection` Map.keysSet bsonMap

          pairs =
            [ let uS = toUnifiedFromPg   (binMap  Map.! i)
                  uT = toUnifiedFromBson (bsonMap Map.! i)
              in (uT, uS)
            | i <- ids
            ]

      evaluate (force pairs)
      pure pairs

    --------------------------------------------------------------------------
    -- env: prepare pairs for text bigul-only
    setupTxtPairs :: IO [(DTxt.UnifiedStruct, DTxt.UnifiedStruct)]
    setupTxtPairs = do
      doc <- X.readFile X.def xmlPath
      let pgEntries = Txt.parsePgUsers doc
      jbEntries <- Txt.loadBsonEntries jsonPath

      let pgMap = Map.fromList [(DTxt.userId e,  e) | e <- pgEntries]
          jbMap = Map.fromList [(DTxt.bsonId e, e) | e <- jbEntries]
          ids   = Set.toList $ Map.keysSet pgMap `Set.intersection` Map.keysSet jbMap

          pairs =
            [ let uS = Txt.toUnifiedFromPg   (pgMap Map.! i)
                  uT = Txt.toUnifiedFromBson (jbMap Map.! i)
              in (uT, uS)
            | i <- ids
            ]

      evaluate (force pairs)
      pure pairs

    --------------------------------------------------------------------------
    -- bench kernels: ONLY syncAge, force via strict fold
    -- BIN
    runBin_bin2bson :: [(UnifiedStruct, UnifiedStruct)] -> Int
    runBin_bin2bson =
      List.foldl' (\acc (uT, uS) -> acc + fromIntegral (age (syncAge uT uS))) 0

    runBin_bson2bin :: [(UnifiedStruct, UnifiedStruct)] -> Int
    runBin_bson2bin =
      List.foldl' (\acc (uT, uS) -> acc + fromIntegral (age (syncAge uS uT))) 0

    -- TEXT
    runTxt_xml2json :: [(DTxt.UnifiedStruct, DTxt.UnifiedStruct)] -> Int
    runTxt_xml2json =
      List.foldl' (\acc (uT, uS) -> acc + DTxt.age (Txt.syncAge uT uS)) 0

    runTxt_json2xml :: [(DTxt.UnifiedStruct, DTxt.UnifiedStruct)] -> Int
    runTxt_json2xml =
      List.foldl' (\acc (uT, uS) -> acc + DTxt.age (Txt.syncAge uS uT)) 0
