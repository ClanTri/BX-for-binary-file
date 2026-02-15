{-# LANGUAGE TemplateHaskell #-}

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter

main :: IO ()
main = do
    -- RearrS 示例：把 (Int, ()) 作为源，同步 Int 视图
    let prog1 = $(rearrS [| \(x, ()) -> x |]) Replace

    let s = (42, ())
    let v = 123

    let putResult1 = put prog1 s v
    print putResult1  -- Just (123, ())

    let getResult1 = get prog1 s
    print getResult1  -- Just 42

    -- RearrV 示例：把 Int 视图变成 (Int, ())
    let prog2 = $(rearrV [| \v -> (v, ()) |]) Replace

    -- 注意：prog2 :: BiGUL (Int, ()) Int
    let putResult2 = put prog2 (42, ()) 123
    print putResult2  -- Just (123, ())

    let getResult2 = get prog2 (42, ())
    print getResult2  -- Just 42
