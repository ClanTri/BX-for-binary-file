{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric, ViewPatterns  #-}

import GHC.Generics
import Generics.BiGUL.Interpreter
import Generics.BiGUL.AST
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH
import Data.List
import Data.Maybe
import Control.Monad.Except



data Nat = Z | S Nat 
  deriving (Show ,Eq)

deriveBiGULGeneric ''Nat

lessOrEqual :: Nat -> Nat -> Bool
lessOrEqual Z     _     = True
lessOrEqual (S a) (S b) = lessOrEqual a b
lessOrEqual _     _     = False

greatOrEqual :: Nat -> Nat -> Bool
greatOrEqual _     Z     = True
greatOrEqual (S a) (S b) = greatOrEqual a b
greatOrEqual _     _     = False

int2Nat :: Int -> Nat
int2Nat 0 = Z
int2Nat n = S (int2Nat (n-1))

nat2Int :: Nat -> Int
nat2Int Z     = 0
nat2Int (S n) = 1 + nat2Int n

-- update the rs of the source pair using (v - ls)
-- by recursively reduce ls and view until ls is Z, and then 
-- replace rs with the v.
sub :: BiGUL (Nat, Nat) Nat
sub = Case [ $(normal [| \(ls, _) v -> not (lessOrEqual ls v)|]) $
              Fail "v must be greatOrEqual than ls",
             $(normalS [p| (Z, _) |] ) $
              $(rearrV [| \v -> (Z, v) |]) Replace,
             $(normalSV [p| (S _ , _) |] [p| S _ |]) $
              $(rearrV [| \(S v) -> v |])
               ($(rearrS [| \(S ls, rs) -> (ls , rs) |]) sub)
           ]

-- helper function:
-- when view is Zero, replace all elements in the source with Zero.
putSumVZ :: BiGUL [Nat] Nat
putSumVZ = Case [ $(normalSV [p| [] |] [p| Z |]) $
                   $(rearrV [| \Z -> [] |]) Replace,
                  $(normalV [p| Z |]) $
                   $(rearrV [| \Z -> (Z, Z) |]) $
                    ($(rearrS [| \(x: xs) -> (x, xs) |]) (Prod Replace putSumVZ)),
                  $(normalV [p| _ |]) $
                    Fail "View Nat is not Z"
                ]



-- try to keep the original source as much as possible from the first to the last.
putSum :: BiGUL [Nat] Nat
putSum =  Case [  -- view is Z, replace all source elements with Z
                 $(normalV [p| Z |]) $
                  putSumVZ,
                  -- source is [], adapt source to contain v
                 $(adaptiveS [p| [] |]) $
                  \_ v -> [v],
                  -- the first element of source is Z, then update the rest.
                 $(normalS [p| Z: _ |]) $
                  $(rearrS [| \(_: s) -> s |]) putSum,
                  -- reduce both the first element of source and view 
                 $(normalSV [p| S _ :_ |] [p| S _ |]) $
                  $(rearrV [| \(S v) -> v |])
                    ($(rearrS [| \((S x): xs) -> x: xs |]) putSum),
                 $(normalSV [p| _ |] [p| _ |]) $
                  Fail "putSum error"
               ]


-- helper function:
-- 1. when view is Z, try to keep all Z in the source.
-- 2. otherwise, replace any kind of source by [Z].
putSumVZ1 :: BiGUL [Nat] Nat
putSumVZ1 = Case [ $(normalSV [| all (== Z) |] [p| Z |]) $ 
                   $(rearrV [| \Z -> () |]) Skip,
                   $(adaptiveSV [| \ss -> not (all (== Z) ss) |] [p| Z |]) $
                    \s v -> [v],
                   $(normalV [p| _ |]) $
                    Fail "View Nat is not Z"
                ]


putSum1 :: BiGUL [Nat] Nat
putSum1 =  Case [  -- view is Z
                 $(normalV [p| Z |]) $
                  putSumVZ1,
                  -- source is [], adapt source to contain v
                 $(adaptiveS [p| [] |]) $
                  \_ v -> [v],
                  -- the first element of source is Z, then update the rest.
                 $(normalS [p| Z: _ |]) $
                  $(rearrS [| \(_: s) -> s |]) putSum1,
                  -- reduce both the first element of source and view 
                 $(normalSV [p| S _ :_ |] [p| S _ |]) $
                  $(rearrV [| \(S v) -> v |])
                    ($(rearrS [| \((S x): xs) -> x: xs |]) putSum1),
                 $(normalSV [p| _ |] [p| _ |]) $
                  Fail "putSum error"
               ]





lrotateHelper :: Eq a =>  BiGUL [a] [a]
lrotateHelper = $(update    [p| x:xs |]
                            [p| x:xs |]
                            [d| x = Replace ; xs = lrotate|])


lrotate :: Eq a =>  BiGUL [a] [a]
lrotate = Case [ $(normalV [p| [] |])  Replace,
                 $(normalV [p| [_] |]) Replace,
                 $(adaptiveS [p| [] |]) $ -- view is not empty.
                   \_ _ -> [undefined] ,
                 $(normalV [p| _ |]) $
                    ((flip Compose)
                        ($(rearrV [| \(a : b :xs) -> b :a :xs |]) Replace)
                        lrotateHelper)
               ]

lreverse :: Eq a => BiGUL [a] [a]
lreverse = Case [ $(normalV [p| [] |]) Replace,
                  $(adaptiveS [p| [] |]) $
                   \_ _ -> [undefined] ,
                  $(normalV [p| _ |]) $
                    ((flip Compose)
                 ($(update  [p| x:xs |]
                            [p| x:xs |]
                            [d| x = Replace ; xs = lreverse|]))
                   lrotate)
                ]

putSum2 :: BiGUL [Nat] Nat
putSum2 = ((flip Compose) putSum
             lreverse)


s :: [Int]
s = [1, 2, 3]
v :: Int
v = 3

putHelper  b s v = either (\ _ -> []) (map nat2Int) $ put b (map int2Nat s) (int2Nat v)
getHelper  b s  = either (\ _ -> (-1)) nat2Int $ get b (map int2Nat s)
