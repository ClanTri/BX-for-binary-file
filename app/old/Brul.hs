{-#  LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies, ScopedTypeVariables  #-}

module Brul where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Generics.BiGUL.Lib

import GHC.Generics
import Data.List
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Maybe

import Alignment (employees,bx,updatedEmployees0,updatedEmployees4,cr)

type RT     = [Record]
type Record = [RType]
data RType  = RInt Int
            | RString String
            | RFloat Float
            | RDouble Double
            deriving (Show, Eq, Ord)

deriveBiGULGeneric ''RType

showRType :: RType -> String
showRType (RInt i)      = show i
showRType (RString str) = str
showRType (RFloat f)    = show f
showRType (RDouble d)   = show d

tshow :: [Record] -> String
tshow []        = ""
tshow (line:ls) = tshow1 line ++ "\n" ++ tshow ls

tshow1 :: Record -> String
tshow1 []     = ""
tshow1 (r:rs) = showRType r ++ ", " ++ tshow1 rs

showTable :: [Record] -> IO ()
showTable t = putStr (tshow t)

showResult (Right t)    = showTable t
showResult (Left error) = putStrLn (show error)

showTuple :: ([Record], [Record]) -> IO ()
showTuple (s1, s2) = putStrLn "s1:" >>
                     showTable s1 >>
                     putStrLn "\ns2:" >>
                     showTable s2

showResultTuple (Right t)    = showTuple t
showResultTuple (Left error) = putStrLn (show error)

s = [ [RString "Lullaby"   , RInt 1989, RInt 3, RString "Galore"  , RInt 1]
    , [RString "Lullaby"   , RInt 1989, RInt 3, RString "Show"    , RInt 3]
    , [RString "Lovesong"  , RInt 1989, RInt 5, RString "Galore"  , RInt 1]
    , [RString "Lovesong"  , RInt 1989, RInt 5, RString "Paris"   , RInt 4]
    , [RString "Trust"     , RInt 1992, RInt 4, RString "Wish"    , RInt 5]
    ]

pAlign :: forall s v k . (Show s, Show v, Eq k)
       => (s -> Bool) --  predicate
       -> (s -> k) -> (v -> k) -> BiGUL s v -> (v -> s)
       -> (s -> Maybe s) --  conceal function
       -> BiGUL [s] [v]
pAlign p ks kv b c h = Case
  [ $(normalSV [p| [] |] [p| [] |] [p| [] |])
    ==> $(update [p| [] |] [p| [] |] [d|  |])
  , $(normal [| \(s:ss) (v:vs) -> p s && ks s == kv v |] [| \(s:ss) -> p s |])
    ==> $(update [p| x:xs |]  [p| x:xs |] [d| x = b; xs = pAlign p ks kv b c h |])
  , $(adaptive [| \(s:ss) v -> p s && null v |])
    ==> \(s:ss) v -> maybe [] (:[]) (h s) ++ ss
  , $(normal [| \(s:ss) v -> not (p s) |] [| \(s:ss) -> not (p s) |])
    ==> $(update [p| _:xs |] [p| xs |] [d| xs = pAlign p ks kv b c h |])
  , $(adaptive [| \ss (v:vs) -> kv v `elem` map ks (filter p ss) |])
    ==> \ss (v:_) -> uncurry (:) (extract (kv v) ss)
  , $(adaptiveSV [p| _ |] [p| _ :_ |])
    ==> \ss (v:_) -> filterCheck p (c v) : ss
  ]
  where
    extract :: k -> [s] -> (s, [s])
    extract k (x:xs) | p x && ks x == k = (x, xs)
                     | otherwise        = let (y, ys) = extract k xs
                                          in  (y, x:ys)
    filterCheck p v | p v       = v
                    | otherwise = error "error in filter checking"

pSelProj = pAlign (\(k,(n,s)) -> s > 1000) fst fst bx cr' (const Nothing)
  where cr' (k,n) = (k,(n, 2000))

pSelProj' = pAlign (\(k,(n,s)) -> s > 800) fst fst bx cr' (const Nothing)
  where cr' (k,n) = (k,(n, 2000))

u0   :: RType -> BiGUL [Record] [Record]
u0 d =  pAlign
          (\r -> (r !! 4) > RInt 2)
          (\s -> (s !! 0, s !! 3))
          (\v -> (v !! 0, v !! 2))
          $(update  [p| ( t : _ : r : a : q : []) |]
                    [p| ( t : r : a : q : []) |]
                    [d| t = Replace; r = Replace; a = Replace; q = Replace |])
          (\(t : r : a : q : []) -> (t : d : r : a : q : []))
          (const Nothing)

u1   :: RType -> BiGUL [Record] [Record]
u1 d =  pAlign
          (\r -> (r !! 4) > RInt 2)
          (\s -> (s !! 0, s !! 3))
          (\v -> (v !! 0, v !! 2))
          $(update [p| ( t : _ : r : a : q : []) |]
                   [p| ( t : r : a : q : []) |]
                   [d| t = Replace; r = Replace; a = Replace; q = Replace |])
          (\(t : r : a : q : []) -> (t : d : r : a : q : []))
          (\(t : d : r : a : _ : []) -> Just (t : d : r : a : RInt 0 : []))

v =  [ [RString "Lullaby" , RInt 4, RString "Show" , RInt 3]
     , [RString "Lovesong", RInt 5, RString "Paris", RInt 7]
     , [RString "Newsong", RInt 2, RString "New Album", RInt 5]
     ]
