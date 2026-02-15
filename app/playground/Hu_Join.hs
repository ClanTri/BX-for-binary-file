{-# LANGUAGE TemplateHaskell, TypeFamilies, TupleSections #-}

module Hu_Join where

import GHC.Generics

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List
import Generics.BiGUL.Interpreter

import Data.List

import Hu_Prelude

-- Memo: unsuccessful implementation of pJoin:
-- This is not an implemenation of my intended put-join.
-- The version commented out has a very restrictive (partial) get,
-- which only accepts two lists that are well key-aligned.
-- The new version: embKey seems incorrected; do not know the reason yet ...

pJoin :: (Show k, Show v1, Show v2, Eq k, Eq v1, Eq v2) =>
             BiGUL ([(k,v1)], [(k,v2)]) [(k,(v1,v2))]
pJoin =
  Case [
    $(normalSV [p| ([],[]) |] [p| [] |] [p| ([],[]) |])
      ==> $(update [p| _ |] [p| [] |] [d| |]),

    $(adaptive [| \_ v -> null v |])
            ==> \(_,s2) v -> ([],s2),

    $(normal [| \s v -> not (null v) |]
             [| \(s1,s2) -> not (null s1) |])
      ==> Case [
            $(adaptive [| \(s1,s2) (b:v) -> not (hasHdKey (fst b) s1) |])
              ==> \(s1,s2) ((k,(v1,v2)):_) -> (adjust (k,v1) s1, s2),

            $(normal [| \(a1:s1,s2) (b:v) -> fst a1 == fst b |]
                     [| \(a1:s1,s2) -> hasKey (fst a1) s2 |])
              ==> $(rearrS [| \((k1,v1):s1, s2) -> (((k1,v1),s2), (s1,s2)) |]) $
                    $(rearrV [| \((k,(v1,v2)):vs) -> (((k,v1),(k,v2)),vs) |]) $
                        $(update [p| ((kv1,kv2),ss) |] [p| ((kv1,kv2),ss) |]
                                 [d| kv1 = Replace
                                     kv2 = embKV
                                     ss = pJoin |])
          ]
  ]
  where
     adjust (k,v) xs = let (xs1,xs2) = partition (\(k',_)->k'==k) xs
                       in if xs1 == [] then (k,v):xs else xs1++xs2
     hsaKey k [] = False
     hasKey k ((k',_):xs) = k==k' || hasKey k xs
     hasHdKey k xs = if null xs then False
                     else k == fst (head xs)

embKV :: (Show k, Show v, Eq k) =>
         BiGUL [(k,v)]  (k,v)
embKV =
  Case [
    $(normal [| \((k',a):s) (k,v) -> k==k' |]
             [| const True |])
      ==> $(update [p| kv:_ |] [p| kv |] [d| kv = Replace |]),
    $(normal [| \((k',a):s) (k,v) -> k/=k'  |]
             [| const True |])
      ==> $(update [p| _:kvs |] [p| kvs |] [d| kvs = embKV |]),
    $(adaptive [| \s v -> null s |])
      ==> \s kv -> [kv]
    ]

{-

put embKV [] (1,100)

-}


{-

pJoin :: (Show k, Show v1, Show v2, Eq k, Eq v1, Eq v2) =>
          BiGUL ([(k,v1)], [(k,v2)]) [(k,(v1,v2))]
pJoin =
  Case [
          $(normalSV [p| ([],[]) |] [p| [] |] [p| ([],[]) |])
            ==> $(update [p| _ |] [p| [] |] [d| |]),
  
          $(normal [| \((k1,_):_, (k2,_):_) ((k3,(_,_)):_) -> k1==k2 && k2==k3 |]
                   [| \(s1,s2) -> not (null s1) && not (null s2) |])
            ==> $(rearrS [| \((k1,v1):kv1, (k2,v2):kv2) -> ((k1,v1,v2), (kv1,kv2)) |]) $
                  $(update [p| ((k,v1,v2),kv12) |] [p| (k,(v1,v2)):kv12 |]
                           [d| k = Replace; v1 = Replace; v2 = Replace;
                               kv12 = pJoin |]),

          $(adaptive [| \s v -> True |])
            ==> adapt
       ]
   where
     adapt s [] = ([],[])
     adapt (t1,t2) ((k,(v1,v2)):_) = let t1' = adjust (k,v1) t1
                                         t2' = adjust (k,v2) t2
                                     in (t1',t2')
     adjust (k,v) xs = let (xs1,xs2) = partition (\(k',_)->k'==k) xs
                   in if xs1 == [] then (k,v):xs else xs1++xs2

-}

{-

*Hu_Join> put pJoin ([(1,10),(2,20),(3,30)],[(1,100),(2,200),(3,300)]) [(1,(11,100)),(2,(12,200)),(3,(13,300))]
Just ([(1,11),(2,12),(3,13)],[(1,100),(2,200),(3,300)])

*Hu_Join> get pSplit ([(1,10),(2,20),(3,30)],[(1,100),(2,200),(3,300)])
Just [(1,(10,100)),(2,(20,200)),(3,(30,300))]

-}

