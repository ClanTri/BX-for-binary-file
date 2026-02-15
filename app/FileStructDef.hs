{-# LANGUAGE DeriveGeneric #-}
module FileStructDef (FileStruct(..)) where
import Generics.BiGUL
import Generics.BiGUL.Interpreter
-- import Generics.BiGUL.TH
import qualified Data.ByteString as BS
import GHC.Generics
import Data.Word

data FileStruct = FileStruct {
    prefix :: BS.ByteString,
    age    :: Word32,
    suffix :: BS.ByteString
} deriving (Show, Eq, Generic)
