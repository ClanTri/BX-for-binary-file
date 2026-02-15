{-#  LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies  #-}

module List where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Data.List
import Data.Maybe
import Control.Monad.Except
import GHC.Generics

import Basic

lensFoldr :: (Show a, Show v)
          => BiGUL (a, v) v -> (v -> Bool) -> BiGUL ([a], v) v
lensFoldr bx pv =
  Case [ $(adaptive [| \(x,y) v -> pv v && length x /= 0 |])
         ==> \(x,y) v -> ([],y)
       , $(normal [| \(xs,_) v -> null xs |] [| \(xs,_) -> null xs |])
         ==> $(rearrV [| \v -> ((),v) |]) $
               $(update [p|( _, v) |] [p|( (),v) |] [d|v  = Replace |])
       , $(normalSV [p|_ |] [p|_ |] [| \(xs,_) -> not (null xs) |])
         ==> $(rearrS [| \((x:xs), e) -> (x, (xs,e)) |])$
               (Replace `Prod` lensFoldr bx pv) `Compose` bx
       ]

lensMapAppend :: (Show a, Show b) => BiGUL a b -> BiGUL ([a],[b]) [b]
lensMapAppend pf = lensFoldr bx null
   where bx = $(rearrV [| \(v:vs) -> (v,vs) |]) $
                pf `Prod` Replace

dec1 :: (Eq a, Num a) => BiGUL a a
dec1 =  emb g p
  where g s   = s + 1
        p s v = v - 1

lensReverse :: Show a => BiGUL [a] [a]
lensReverse =
  Case [ $(adaptive [| \s v -> length s < length v |])
         ==> \s v -> v
       , $(normalSV [p|_ |] [p|_ |] [| \s -> True |])
         ==> $(rearrS [| \s -> (s,[]) |]) $
               lensFoldr (lensSwap `Compose` lensSnoc) null
       ]

lensSnoc :: Show a => BiGUL ([a],a) [a]
lensSnoc =
  Case [ $(normal [| \s v -> length v == 1 |] [| \(s,_) -> null s |])
         ==> $(rearrV [| \[v] -> ([],v) |]) Replace
       , $(normal [| \(s,_) v -> length s > 0 |] [| \(s,_) -> length s > 0 |])
         ==> $(rearrS [| \(y:ys,x) -> (y,(ys,x)) |]) $
               $(rearrV [| \(v:vs) -> (v,vs) |]) $
                 Replace `Prod` lensSnoc
       , $(adaptive [| \(s,_) v -> null s |])
         ==> \(s,x) _ -> ([undefined], x)
       ]

lensSwap :: (Show a, Show b) => BiGUL (a,b) (b,a)
lensSwap =  $(rearrS [| \(x,y) -> (y,x) |]) Replace

lensSum :: BiGUL ([Int], Int) Int
lensSum =  lensFoldr pSum2 (const False)

lensSum' :: BiGUL ([Int], Int) Int
lensSum' =  lensFoldr ($(rearrS [| \(x,y) -> (y,x) |]) pSum2) (const False)
