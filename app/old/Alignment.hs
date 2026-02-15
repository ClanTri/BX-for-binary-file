{-#  LANGUAGE TemplateHaskell, TypeFamilies, ScopedTypeVariables  #-}

module Alignment where

import Generics.BiGUL
import Generics.BiGUL.TH
import Generics.BiGUL.Lib
import Generics.BiGUL.Interpreter

import Data.Tuple
import Data.Maybe
import Data.List

type Source = (Id, (Name, Salary))

type Id     = Int
type Name   = String
type Salary = Int

employees :: [Source]
employees =  [ (0, ("Zhenjiang", 1000))
             , (1, ("Josh"     ,  400))
             , (2, ("Jeremy"   , 2000)) ]

type View = (Id, Name)

bx :: BiGUL Source View
bx =  $(rearrV [| \(id, name) -> (id, (name, ())) |])$
        Replace `Prod` (Replace `Prod` Skip (const ()))

cr :: View -> Source
cr (i, n) = (i, (n, 0))

posAlign :: (Show s, Show v) => BiGUL s v -> (v -> s) -> BiGUL [s] [v]
posAlign b c = Case
  [ $(normalSV [p| [] |] [p| [] |] [p| [] |])
    ==> $(update [p| [] |] [p| [] |] [d|  |])
  , $(normalSV [p| _:_ |] [p| _:_ |] [p| _:_ |])
    ==> $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = posAlign b c |])
  , $(adaptiveSV [p| _:_ |] [p| [] |])
    ==> \_ _ -> []
  , $(adaptiveSV [p| [] |] [p| _:_ |])
    ==> \_ (v:_) -> [c v]
  ]

updatedEmployees0 :: [View]
updatedEmployees0 =  [(0, "Zhenjiang"), (2, "Jeremy")]

updatedEmployees1 :: [View]
updatedEmployees1 =  [(2, "Jeremy"), (0, "Zhenjiang"), (1, "Josh")]

keyAlign :: forall s v k . (Show s, Show v, Eq k)
         => (s -> k) -> (v -> k) -> BiGUL s v -> (v -> s) -> BiGUL [s] [v]
keyAlign ks kv b c = Case
  [ $(normalSV [p| [] |] [p| [] |] [p| [] |])
    ==> $(update [p| [] |] [p| [] |] [d|  |])
  , $(normal [| \(s:ss) (v:vs) -> ks s == kv v |] [p| _:_ |])
    ==> $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = keyAlign ks kv b c |])
  , $(adaptiveSV [p| _:_ |] [p| [] |])
    ==> \_ _ -> []
  , $(adaptive [| \ss (v:vs) -> kv v `elem` map ks ss |])
    ==> \ss (v:_) -> uncurry (:) (extract (kv v) ss)
  , $(adaptiveSV [p| _ |] [p| _:_ |])
    ==> \ss (v:_) -> c v : ss
  ]
  where
    extract :: k -> [s] -> (s, [s])
    extract k (x:xs) | ks x == k  = (x, xs)
                     | otherwise  = let (y, ys) = extract k xs
                                    in  (y, x:ys)

updatedEmployees2 :: [View]
updatedEmployees2 =  [(0, "Zhenjiang"), (100, "Josh"), (1, "Jeremy")]

updatedEmployees3 :: [View]
updatedEmployees3 =  [(0, "Zhenjiang"), (1, "Mike"), (2, "Jeremy")]

updatedEmployees4 :: [View]
updatedEmployees4 =  []

type Delta = [(Int, Int)]

-- idDelta    :: [s] -> Delta
-- idDelta ss =  [ (i, i) | i <- [0..length ss] ]

idDelta :: [s] -> Delta
idDelta ss = [ (i, i) | i <- [0..length ss - 1] ]

deltaAlign :: (Show s, Show v)
           => BiGUL s v -> (v -> s) -> BiGUL ([s], Delta) [v]
deltaAlign b c = Case
  [ $(normal [| \(ss, d) vs -> length ss == length vs && d == idDelta ss |]
             [p| _ |])
    ==> $(rearrV [| \vs -> (vs, ()) |])$ posAlign b c `Prod` Skip (const ())
  , $(adaptive [| \_ _ -> otherwise |])
    ==> \(ss, d) vs ->
          let d'  = map swap d
              ss' = [ maybe (c v) (ss !!) (lookup j d') | (v, j) <- zip vs [0..] ]
          in (ss', idDelta ss')
  ]

putDeltaAlign :: (Show s, Show v)
              => BiGUL s v -> (v -> s) -> [s] -> Delta -> [v] -> Maybe [s]
putDeltaAlign b c ss d vs = fmap fst (put (deltaAlign b c) (ss, d) vs)

getDeltaAlign :: (Show s, Show v)
              => BiGUL s v -> (v -> s) -> [s] -> Maybe [v]
getDeltaAlign b c ss = get (deltaAlign b c) (ss, idDelta ss)

type DeltaStrategy s v = [s] -> [v] -> Delta

putDeltaAlignS :: (Show s, Show v) => DeltaStrategy s v
               -> BiGUL s v -> (v -> s) -> [s] -> [v] -> Maybe [s]
putDeltaAlignS dst b c ss vs = putDeltaAlign b c ss (dst ss vs) vs

byPosition :: DeltaStrategy s v
byPosition ss _ = idDelta ss

byKey :: Eq k => (s -> k) -> (v -> k) -> DeltaStrategy s v
byKey ks kv ss vs =
  let sis = zip ss [0..]
  in  catMaybes [ fmap (\(_, i) -> (i, j)) (find (\(s, _) -> ks s == kv v) sis)
                | (v, j) <- zip vs [0..] ]

