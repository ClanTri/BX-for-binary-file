{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric, ViewPatterns  #-}

import Generics.BiGUL
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import GHC.Generics

data Foo = X Int Int Int
         | Y Int Int deriving(Eq,Show)
deriveBiGULGeneric ''Foo

data Bar = M { x0 :: Int , x1 :: Int , x2 :: Int}
         | N { y0 :: Int , y1 :: Int } deriving(Eq,Show)
deriveBiGULGeneric ''Bar

data Fooo = P | Q Fooo Fooo deriving(Eq,Show)
deriveBiGULGeneric ''Fooo

{-
rearrVEqTest :: BiGUL Fooo Fooo
rearrVEqTest = $(rearrV [| \p -> Q p p|]) Replace
-}

rearrSEqTest :: BiGUL Fooo Fooo
rearrSEqTest = $(rearrS [| \p -> Q p p |]) Replace


rearrVTest1 :: BiGUL (Int,(Int,Int)) (Int,Int)
rearrVTest1 = $(rearrV [| \(x,y) -> (x,(y,x))|]) Replace

rearrVTest2 :: BiGUL Foo Foo
rearrVTest2 = $(rearrV [| \(Y x y) -> X x y x |]) Replace

rearrVTest3 :: BiGUL Bar Bar
rearrVTest3 = $(rearrV [| \(N {y0 = x , y1 = y}) -> M {x1 = y , x0 = x , x2 = x}|]) Replace


rearrSTest1 :: BiGUL (Int,(Int,Int)) (Int,Int)
rearrSTest1 = $(rearrS [| \(x,(_,y)) -> (x,y)|]) Replace

rearrSTest2 :: BiGUL Foo Foo
rearrSTest2 = $(rearrS [| \(X x y _) -> Y x y |]) Replace

rearrSTest3 :: BiGUL Bar Bar
rearrSTest3 = $(rearrS [| \M {x1 = y , x0 = x , x2 = _} -> N {y0 = x , y1 = y} |]) Replace


rearrSVTest1 :: BiGUL (Int,(Int,Int)) Foo
rearrSVTest1 = $(update  [p| Y a b|]
                         [p| (a,(b,_)) |]
                         [d| a = Replace ; b = Replace|])

rearrSVTest2 :: BiGUL Foo Bar
rearrSVTest2 = $(update  [p| N {y1 = a , y0 = b} |]
                         [p| X a _ b |]
                         [d| a = Replace ; b = Replace|])