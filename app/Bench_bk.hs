{-# LANGUAGE OverloadedStrings #-}
module Main where

import Criterion.Main
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Aeson (encode)  
import Control.DeepSeq (force)
import Control.Exception (evaluate)

import DataStructs
  ( PgUserEntry(..)
  , BsonUserEntry(..)
  , UnifiedStruct(..)
  )

import Sync_Multi_Rearr_test
  ( parseBinFile, parseBsonArray
  , assembleBinFile, assembleBsonFile
  , toUnifiedFromPg, toUnifiedFromBson
  , updatePgFromUnified, updateBsonFromUnified
  , syncAge
  )

main :: IO ()
main = defaultMain
  [ env setup $ \ ~(binEntries, bsonEntries, binMap, bsonMap, ids) ->
      bgroup "sync-core"
        [ bench "bin2bson-core" $
            nf (\is ->
                let updatedBson =
                      [ let uniSrc = toUnifiedFromPg   (binMap  Map.! i)
                            uniTgt = toUnifiedFromBson (bsonMap Map.! i)
                            uniOut = syncAge uniTgt uniSrc
                        in updateBsonFromUnified (bsonMap Map.! i) uniOut
                      | i <- is
                      ]
                in BS.length (assembleBsonFile updatedBson)  
               ) ids
        , bench "bson2bin-core" $
            nf (\is ->
                let updatedBin =
                      [ let uniSrc = toUnifiedFromBson (bsonMap Map.! i)
                            uniTgt = toUnifiedFromPg   (binMap  Map.! i)
                            uniOut = syncAge uniTgt uniSrc
                        in updatePgFromUnified (binMap Map.! i) uniOut
                      | i <- is
                      ]
                in BS.length (assembleBinFile updatedBin)   
               ) ids
        ]
  , env setupIO $ \ ~(binPath, bsonPath) ->
      bgroup "io-paths"
        [ bench "read-bin-parse" $
            nfIO (BS.readFile binPath >>= evaluate . force . parseBinFile)
        , bench "read-bson-parse" $
            nfIO (BS.readFile bsonPath >>= evaluate . force . parseBsonArray)
        ]
  ]
  where
    binPath  = "user.bin"
    bsonPath = "user.bson"

    setup = do
      binRaw  <- BS.readFile binPath
      bsonRaw <- BS.readFile bsonPath
      let binEntries  = parseBinFile  binRaw
          bsonEntries = parseBsonArray bsonRaw
          binMap      = Map.fromList [(userId e, e) | e <- binEntries]
          bsonMap     = Map.fromList [(bsonId e, e) | e <- bsonEntries]
          ids         = Set.toList $ Map.keysSet binMap `Set.intersection` Map.keysSet bsonMap
      pure (binEntries, bsonEntries, binMap, bsonMap, ids)

    setupIO = pure (binPath, bsonPath)
