{-# LANGUAGE TemplateHaskell, TypeFamilies, TupleSections #-}

module Hu_Prelude where

import GHC.Generics

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Lib.List
import Generics.BiGUL.Interpreter

(@@) :: (Show s, Show m, Show v) =>
        BiGUL s m -> BiGUL m v -> BiGUL s v
(@@) = Compose

keyAlign :: (Show k, Show v, Eq k) =>
            BiGUL [(k,v)] [(k,v)]
keyAlign = align (\_ -> True)
                 (\(k1,_) (k2,_) -> k1==k2)
                 Replace
                 id
                 (\_ -> Nothing)

{-
*Hu_Prelude> put keyAlign [(1,100),(3,300)] [(2,200),(3,3000),(4,400)]
Just [(2,200),(3,3000),(4,400)]

*Hu_Prelude> get keyAlign [(2,200),(3,3000),(4,400)]
Just [(2,200),(3,3000),(4,400)]
-}

posAlign :: (Show a, Eq a) =>
            BiGUL [a] [a]
posAlign = spanBX [] (addKeys 0) Replace @@
           keyAlign @@
           spanBX [] Replace (addKeys 0) 

{-
*Hu_Prelude> put posAlign [1,2,3] [10,2,4,5]
Just [10,2,4,5]

*Hu_Prelude> get posAlign [1,2,3]
Just [1,2,3]
-}

-- swapping the source and the view of a bidirectional transformation
swapSV :: Eq s =>
          s -> BiGUL s v -> BiGUL v s
swapSV s0 bx = spanBX s0 bx Replace

-- implementing a bx for a span:  v1 <- s -> v2
spanBX :: Eq v2 =>
          s -> BiGUL s v1 -> BiGUL s v2 -> BiGUL v1 v2
spanBX s0 bx1 bx2 = emb g p
  where g v1 = case (do s <- put bx1 s0 v1; get bx2 s) of
                  Just v -> v
                  Nothing -> error "Error in running spanBX"
        p v1 v2 = case (do s <- put bx2 s0 v2; get bx1 s) of
                     Just v -> v
                     Nothing -> error "Error in running spanBX"

-- implementation of a bx for a co-span: s1 -> v <- s2
co_spanBX :: Eq s2 =>
             s2 -> BiGUL s1 v -> BiGUL s2 v -> BiGUL s1 s2
co_spanBX s20 bx1 bx2 = emb g p
  where g s1 = case (do v <- get bx1 s1; put bx2 s20 v) of
                  Just v -> v
                  Nothing -> error "Error in running co_spanBX"
        p s1 s2 = case (do v <- get bx2 s2; put bx1 s1 v) of
                    Just v -> v
                    Nothing -> error "Error in running co_spanBX"
          
-----------------------
-- auxillary functions
-----------------------
addKeys :: Show a =>
           Int -> BiGUL [(Int,a)] [a]
addKeys k0 =
  Case [ $(normalSV [p| [] |] [p| [] |] [p| [] |])
            ==> $(update [p| _ |] [p| [] |] [d| |]),
         $(normalSV [p| _:_ |] [p| _:_ |] [p| _:_ |])
            ==> $(update [p| (_,a):as |] [p| a:as |]
                         [d| a=Replace; as=addKeys (k0+1) |]),
         $(adaptive [| \s v -> not (null s) && null v |])
            ==> \_ _ -> [],
         $(adaptive [| \s v -> null s && not (null v) |])
            ==> \_ (v:_) -> [(k0,v)]
  ]

{-

*Hu_Prelude> put (addKeys 0) [] [1..10]
Just [(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]

*Hu_Prelude> get (addKeys 0) [(0,1),(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8),(8,9),(9,10)]
Just [1,2,3,4,5,6,7,8,9,10]

-}

