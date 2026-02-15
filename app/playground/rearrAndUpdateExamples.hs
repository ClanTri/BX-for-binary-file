{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


import GHC.Generics
import Generics.BiGUL
import Util
import Generics.BiGUL.AST
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH
import Data.Set

data Bar = X { x0 :: Int , x1 :: Int , x2 :: Int}
         | Y { y0 :: Int , y1 :: Int } deriving(Eq,Show)
deriveBiGULGeneric ''Bar


test1 :: MonadError' ErrorInfo m => BiGUL m Bar Int
test1 = $(rearrAndUpdate [p| z |] [p| X {x1 = z} |] [d| z = Replace|])

{-
test1 is equivalent to the following program
$(rearr [| \z -> X { x1 = z } |]) $(update [p| X { x1 = z } |] [d| z = Replace|])

and then it will be translated to low level form
$(rearr [| \z -> ((),(z,())) |]) $(update [p| X _ z _ |] [d| z = Replace|])

*Main> testPut test1 (X {x0 = 1, x1 = 2, x2 = 3}) 5
Right (X {x0 = 1, x1 = 5, x2 = 3})
*Main> testGet test1  (X {x0 = 1, x1 = 5, x2 = 3})
Right 5
*Main> testPut test1 (Y {y0 = 1, y1 = 2}) 5
Left (ErrorInfo "Either Left not match")
-}

test2 :: MonadError' ErrorInfo m => BiGUL m (Int,Int) Bar
test2 = $(rearrAndUpdate [p| Y { y0 = n, y1 = 3 }|] [p| (n,_) |] [d| n = Replace |])

{-
test2 is equivalent to the following program
$(rearr [| \(Y n 3) -> (n,()) |]) $(update [p| (n,_) |] [d| n = Replace |])

*Main> testPut test2 (3,4) (Y {y0 = 1, y1 = 2})
Left (ErrorInfo "view must be a constant")
*Main> testPut test2 (3,4) (Y {y0 = 1, y1 = 3})
Right (1,4)
*Main> testGet test2 (1,4)
Right (Y {y0 = 1, y1 = 3})
-}