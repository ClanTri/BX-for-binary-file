{-#  LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies  #-}

module Basic where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import Data.List
import Data.Maybe
import Control.Monad.Except
import GHC.Generics

square   :: Num a => a -> a
square x =  x * x

skip1 :: BiGUL s ()
skip1 =  Skip (const ())

putPairOverNPair :: (Show s0, Show s1, Show s2)
                 => BiGUL ((s0,s1),s2) (s1,s2)
putPairOverNPair =  $(rearrV [| \(v1,v2) -> (((),v1),v2) |]) $
                      (skip1 `Prod` Replace) `Prod` Replace

pHead :: Show s => BiGUL [s] s
pHead =  $(rearrS [| \(s:ss) -> (s, ss) |])$
           $(rearrV [| \v -> (v, ()) |])$
             Replace `Prod` skip1

pNth   :: Show s => Int -> BiGUL [s] s
pNth i =  if i == 0 then pHead
                    else $(rearrS [| \(x:xs) -> (x,xs) |]) $
                           $(rearrV [| \v -> ((), v) |]) $
                             skip1 `Prod` pNth (i-1)

testUpdate :: (Show a, Show b, Show c) => BiGUL ((a,b),c) (((),b),c)
testUpdate =  $(update [p| ((x,y),z) |]
                       [p| ((x,y),z) |]
                       [d| x = skip1; y = Replace; z = Replace |])

testUpdate' :: (Show a, Show b, Show c) => BiGUL ((a,b),c) (((),b),c)
testUpdate' =  $(update [p| ((_  , y), z) |]
                        [p| ((() , y), z) |]
                        [d| y = Replace; z = Replace |])

replaceAll :: (Eq s, Show s) => BiGUL [s] s
replaceAll =
  Case [ $(normal [| \s v -> length s == 1 |]  [| \s -> length s == 1 |])
         ==> $(rearrS [| \[x] -> x |]) Replace
       , $(normal [| \s v -> length s > 1 |]  [| \s -> length s > 1 |])
         ==> $(rearrS [| \(x:xs) -> (x,xs) |])$
               $(rearrV [| \v -> (v, v) |])$
                 Replace `Prod` replaceAll
       , $(adaptive [| \s v -> length s == 0 |])
         ==> \s v -> [undefined]
       ]

pSum2 :: BiGUL (Int, Int) Int
pSum2 =  emb g p
  where g (x,y)   = x + y
        p (x,y) v = (v - y, y)

repHead :: BiGUL [Int] Int
repHead =
  Case [ $(normal [| \s v -> length s > 0 |]  [| \s -> length s > 0 |])
         ==> $(rearrS [| \(x:xs) -> x |]) Replace
       , $(adaptive [| \s v -> length s == 0 |])
         ==> \s v -> [0]
       ]

replaceAll2 :: BiGUL [Int] (Int, Bool)
replaceAll2 =  Dep even replaceAll

pHead2 :: Show a => BiGUL [[a]] a
pHead2 =  pHead `Compose` pHead

pHead' :: Show s => BiGUL [s] s
pHead' =
  Case [ $(normal [| \s v -> not (null s) |]  [| not  . null |])
         ==> pHead
       , $(adaptive [| \s v -> null s |])
         ==> \s v -> [v]
       ]

pEither :: (Show a, Show b, Eq a)
        => a -> BiGUL (Either a b) a
pEither x0 =
  Case [ $(normalSV [p| Left _ |] [p| _ |] [p| Left _ |])
         ==> $(update [p| Left x |]  [p| x |] [d| x  = Replace |]),
       $(normalSV [p| Right y |] [| \x -> x == x0 |] [p| Right _ |])
         ==> Skip (const x0),
       $(adaptive [| \_ _ -> True |])
         ==> \s v -> Left v
       ]
