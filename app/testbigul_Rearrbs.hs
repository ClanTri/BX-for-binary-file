{-# LANGUAGE TemplateHaskell #-}

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter
import Data.Word

main :: IO ()
main = do
    let s :: (Word32, ())
        s = (123, ())
    let v :: Word32
        v = 456

    let prog1 = $(rearrS [| \(x, ()) -> x |]) Replace

    let putResult1 = put prog1 s v
    print putResult1  -- Just (456,())

    let getResult1 = get prog1 s
    print getResult1  -- Just 123

    let prog2 = $(rearrV [| \v -> (v, ()) |]) Replace

    let putResult2 = put prog2 (789, ()) 321
    print putResult2  -- Just (321,())

    let getResult2 = get prog2 (789, ())
    print getResult2  -- Just 789
