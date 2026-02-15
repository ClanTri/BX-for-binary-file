{-# LANGUAGE TemplateHaskell #-}

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter
import Data.Word
import qualified Data.ByteString as BS

main :: IO ()
main = do
    let bs1 = BS.pack [1,2,3,4]   -- 原始 ByteString
    let s = (bs1, 100 :: Word32)  -- 源：(ByteString, Word32)
    let v = 999 :: Word32         -- 视图：只同步 Word32

    -- RearrS: 只抽取 Word32 参与同步，ByteString 保持原样
    let prog = $(rearrS [| \(_, w) -> w |]) Replace

    print $ put prog s v     -- Just (bs1,999)
    print $ get prog s       -- Just 100

    -- RearrV 反过来也是可以的，只要 lambda 右侧“纯构造”
    -- 比如：同步目标是 tuple，把视图 Word32 变成 (bs1, Word32)
    let prog2 = $(rearrV [| \w -> (bs1, w) |]) Replace
    print $ put prog2 (bs1, 50) 321   -- Just (bs1,321)
    print $ get prog2 (bs1, 50)      -- Just 50
