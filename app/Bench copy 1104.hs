{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import Criterion.Types (Config(..), Verbosity(..))
import Criterion.Main (defaultMainWith, defaultConfig)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import qualified Text.XML as X
import Data.Aeson (encode)
import qualified DataStructs_txt      as DTxt
import System.IO.Temp   (getCanonicalTemporaryDirectory, createTempDirectory)
import System.FilePath  ((</>))
import Data.IORef       (IORef, newIORef, atomicModifyIORef')

import qualified Sync_Multi_Rearr_test_text as Txt

import DataStructs
  ( PgUserEntry(..)
  , BsonUserEntry(..)
  , UnifiedStruct(..)
  )

-- bin <-> BSON 
import Sync_Multi_Rearr_test
  ( parseBinFile, parseBsonArray
  , assembleBinFile, assembleBsonFile
  , toUnifiedFromPg, toUnifiedFromBson
  , updatePgFromUnified, updateBsonFromUnified
  , syncAge
  )

main :: IO ()
main = defaultMainWith timeConfig benchmarks
  where
    timeConfig = defaultConfig
      { timeLimit = 180.0     
      , resamples = 2000      
      , verbosity = Normal    
      }
    benchmarks =  
      [ ----------------------------------------------------------------------------
        --(BIN <-> BSON)
        env setupBin $ \ ~(binEntries, bsonEntries, binMap, bsonMap, ids) ->
          bgroup "binary-sync/core"
            [ bench "bin2bson-core" $
                nf (\is ->
                    let updatedBson =
                          [ let uS = toUnifiedFromPg   (binMap  Map.! i)
                                uT = toUnifiedFromBson (bsonMap Map.! i)
                            in updateBsonFromUnified (bsonMap Map.! i) (syncAge uT uS)
                          | i <- is
                          ]
                    in BS.length (assembleBsonFile updatedBson)
                   ) ids
            , bench "bson2bin-core" $
                nf (\is ->
                    let updatedBin =
                          [ let uS = toUnifiedFromBson (bsonMap Map.! i)
                                uT = toUnifiedFromPg   (binMap  Map.! i)
                           in updatePgFromUnified (binMap Map.! i) (syncAge uT uS)
                          | i <- is
                          ]
                    in BS.length (assembleBinFile updatedBin)
                   ) ids
            ]
      , env setupBinIO $ \ ~(binPath, bsonPath) ->
          bgroup "binary-sync/io"
            [ bench "read-bin-parse"  $
                nfIO (BS.readFile binPath  >>= evaluate . force . parseBinFile)
            , bench "read-bson-parse" $
                nfIO (BS.readFile bsonPath >>= evaluate . force . parseBsonArray)
            ]

      , env setupBin $ \ ~(binEntries, bsonEntries, binMap, bsonMap, ids) ->
        envWithTemp $ \ ~(tmpDir, nextIx) ->
          bgroup "binary-sync/write"
            [ bench "write-bin"  $ nfIO $ do
                -- 1) Synchronization + assembly done in memory first
                let updatedBin =
                      [ let uS = toUnifiedFromBson (bsonMap Map.! i)
                            uT = toUnifiedFromPg   (binMap  Map.! i)
                        in updatePgFromUnified (binMap Map.! i) (syncAge uT uS)
                      | i <- ids
                      ]
                    bs = assembleBinFile updatedBin
                -- 2) 选一个不重复的临时文件名并写入Select a non-repeating temporary file name and write
                ix <- atomicModifyIORef' nextIx (\n -> (n+1, n))
                let fp = tmpDir </> ("out-bin-"  ++ show ix ++ ".bin")
                BS.writeFile fp bs
                -- 3) 严格化一点点结果，避免写被拖延
                evaluate (BS.length bs)

            , bench "write-bson" $ nfIO $ do
                let updatedBson =
                      [ let uS = toUnifiedFromPg   (binMap  Map.! i)
                            uT = toUnifiedFromBson (bsonMap Map.! i)
                        in updateBsonFromUnified (bsonMap Map.! i) (syncAge uT uS)
                      | i <- ids
                      ]
                    bs = assembleBsonFile updatedBson
                ix <- atomicModifyIORef' nextIx (\n -> (n+1, n))
                let fp = tmpDir </> ("out-bson-" ++ show ix ++ ".bson")
                BS.writeFile fp bs
                evaluate (BS.length bs)
            ]

        ----------------------------------------------------------------------------
        --(XML <-> JSON)
        ----------------------------------------------------------------------------
      , env setupTxt $ \ ~(pgEntries, jbEntries, pgMap, jbMap, ids) ->
          bgroup "text-sync/core"
            [ bench "xml2json-core" $
                nf (\is ->
                    let updatedBson =
                          [ let uS = Txt.toUnifiedFromPg   (pgMap Map.! i)
                                uT = Txt.toUnifiedFromBson (jbMap Map.! i)
                            in Txt.updateBsonFromUnified (jbMap Map.! i) (Txt.syncAge uT uS)
                          | i <- is
                          ]
                    in BL.length (encode updatedBson)  
                  ) ids
            , bench "json2xml-core" $
                nf (\is ->
                    let updatedPg =
                          [ let uS = Txt.toUnifiedFromBson (jbMap Map.! i)
                                uT = Txt.toUnifiedFromPg   (pgMap Map.! i)
                            in Txt.updatePgFromUnified (pgMap Map.! i) (Txt.syncAge uT uS)
                          | i <- is
                          ]
                    --  renderLBS 
                    in BL.length (X.renderLBS X.def (Txt.assemblePgUsers updatedPg))
                  ) ids
            
            ]
      , env setupTxtIO $ \ ~(xmlPath, jsonPath) ->
          bgroup "text-sync/io"
            [ bench "read-xml-parse" $
                nfIO (X.readFile X.def xmlPath >>= evaluate . force . Txt.parsePgUsers)
            , bench "read-json-decode" $
                nfIO (Txt.loadBsonEntries jsonPath >>= evaluate . force)
            ]

      , env setupTxt $ \ ~(pgEntries, jbEntries, pgMap, jbMap, ids) ->
        envWithTemp $ \ ~(tmpDir, nextIx) ->
          bgroup "text-sync/write"
            [ bench "write-xml" $ nfIO $ do
                let updatedPg =
                      [ let uS = Txt.toUnifiedFromBson (jbMap Map.! i)
                            uT = Txt.toUnifiedFromPg   (pgMap Map.! i)
                        in Txt.updatePgFromUnified (pgMap Map.! i) (Txt.syncAge uT uS)
                      | i <- ids
                      ]
                    lbs = X.renderLBS X.def (Txt.assemblePgUsers updatedPg)
                ix <- atomicModifyIORef' nextIx (\n -> (n+1, n))
                let fp = tmpDir </> ("out-users-" ++ show ix ++ ".xml")
                BL.writeFile fp lbs
                evaluate (BL.length lbs)

            , bench "write-json" $ nfIO $ do
                let updatedBson =
                      [ let uS = Txt.toUnifiedFromPg   (pgMap Map.! i)
                            uT = Txt.toUnifiedFromBson (jbMap Map.! i)
                        in Txt.updateBsonFromUnified (jbMap Map.! i) (Txt.syncAge uT uS)
                      | i <- ids
                      ]
                    lbs = encode updatedBson
                ix <- atomicModifyIORef' nextIx (\n -> (n+1, n))
                let fp = tmpDir </> ("out-users-" ++ show ix ++ ".json")
                BL.writeFile fp lbs
                evaluate (BL.length lbs)
            ]      
      ]
      where
        binPath  = "postgres_users.bin"
        bsonPath = "mongo_users.bson"

        xmlPath  = "postgres_users.xml"
        jsonPath = "mongo_users.json"

        envWithTemp :: ((FilePath, IORef Int) -> Benchmark) -> Benchmark
        envWithTemp k =
          env (do
                base <- getCanonicalTemporaryDirectory
                dir  <- createTempDirectory base "bench-out"
                ref  <- newIORef 0
                pure (dir, ref)
              ) k  

        -- ========== env: bin ==========
        setupBin = do
          binRaw  <- BS.readFile binPath
          bsonRaw <- BS.readFile bsonPath
          let binEntries  = parseBinFile  binRaw
              bsonEntries = parseBsonArray bsonRaw
              binMap      = Map.fromList [(userId e, e) | e <- binEntries]
              bsonMap     = Map.fromList [(bsonId e, e) | e <- bsonEntries]
              ids         = Set.toList $ Map.keysSet binMap `Set.intersection` Map.keysSet bsonMap
          pure (binEntries, bsonEntries, binMap, bsonMap, ids)

        setupBinIO  = pure (binPath, bsonPath)

        -- ========== env: text ==========
        setupTxt = do
          doc       <- X.readFile X.def xmlPath
          let pgEntries = Txt.parsePgUsers doc
          jbEntries <- Txt.loadBsonEntries jsonPath
          let pgMap  = Map.fromList [(DTxt.userId e,  e) | e <- pgEntries]
              jbMap  = Map.fromList [(DTxt.bsonId e, e) | e <- jbEntries]
              ids    = Set.toList $ Map.keysSet pgMap `Set.intersection` Map.keysSet jbMap
          pure (pgEntries, jbEntries, pgMap, jbMap, ids)

        setupTxtIO = pure (xmlPath, jsonPath)
