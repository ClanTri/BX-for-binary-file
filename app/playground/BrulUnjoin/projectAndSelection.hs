{-# LANGUAGE FlexibleContexts, TemplateHaskell, TypeFamilies #-}

import GHC.Generics
import Generics.BiGUL.Interpreter
import Generics.BiGUL
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH
import Data.List
import Control.Monad.Except
import Brul
import qualified Data.Map as Map
import Data.Maybe
{-
0. Original database state: Tracks1
   { Track -> Date, Rating; Album -> Quantity}

   Track      Date   Rating  Album    Quantity
   Lullaby    1989   3       Galore   1
   Lullaby    1989   3       Show     3
   Lovesong   1989   5       Galore   1
   Lovesong   1989   5       Paris    4
   Trust      1992   4       Wish     5


2. View: Tracks3

   Track      Rating  Album    Quantity
   Lullaby    3       Show     3
   Lovesong   5       Paris    4
   Trust      4       Wish     5

2.1 updated view
   Track      Rating  Album    Quantity
   Lullaby    4       Show     3
   Lovesong   5       Paris    7


  relational lenses:
    drop Date determined by (Track, unknown)
      from Tracks1 as Tracks2;
    select from Tracks2
      where Quantity > 2 as Tracks3


  Note: Track && Album as the key.

   Track      Rating  Album    Quantity
   Lullaby    3       Galore   1
   Lullaby    4       Show     3
   Lovesong   5       Galore   1
   Lovesong   5       Paris    7
-}

-- Define the functional dependency on source
sfdMap :: FDMap
sfdMap = Map.fromList [(0,[1,2]), (3,[4])]

-- Define the functional dependency on view
vfdMap :: FDMap
vfdMap = Map.fromList [(0, [1]), (2, [3])]

-- Define the mapping relation between source and view.
svMap :: SVMap
svMap = Map.fromList [(0,0), (2,1), (3,2), (4,3)]

-- Specifying the default value when creating a source.
type Default = RType

--------------------------------------------------------------
-- The first update program
-- Delete the unmatched source record
u0 :: Default -> (Record -> Record) -> Brul [Record] [Record]
u0 d =
  align
    (\r -> (r !! 4) > RInt 2)
    (\s v -> (s !! 0 == v !! 0) && (s !! 3 == v !! 2))
    $(update [p| (t: _: r: a: q: [])|]
             [p| (t: r: a: q: []) |]
             [d| t = Replace; r = Replace; a = Replace; q = Replace |])
    (\(t: r: a: q: []) -> (t: d: r: a: q: []))
    (\rs -> Nothing)

--------------------------------------------------------------
-- The second update program
-- Update the unmatched source record
u1 :: Default -> (Record -> Record) -> Brul [Record] [Record]
u1 d =
  align
    (\r -> (r !! 4) > RInt 2)
    (\s v -> (s !! 0 == v !! 0) && (s !! 3 == v !! 2))
    $(update [p| (t: _: r: a: q: [])|]
             [p| (t: r: a: q: []) |]
             [d| t = Replace; r = Replace; a = Replace; q = Replace |])
    (\(t: r: a: q: []) -> (t: d: r: a: q: []))
    (\(t: d: r: a: _: []) -> Just (t: d: r: a: RInt 1:[]))

--------------------------------------------------------------
-- An example that will fail in the put direction
-- since the update of unmatched source still
-- satisfy the filter condition.
u15 :: Default -> (Record -> Record) -> Brul [Record] [Record]
u15 d =
  align
    (\r -> (r !! 4) > RInt 2)
    (\s v -> (s !! 0 == v !! 0) && (s !! 3 == v !! 2))
    $(update [p| (t: _: r: a: q: [])|]
             [p| (t: r: a: q: []) |]
             [d| t = Replace; r = Replace; a = Replace; q = Replace |])
    (\(t: r: a: q: []) -> (t: d: r: a: q: []))
    (\(t: d: r: a: _: []) -> Just (t: d: r: a: RInt 3:[]))


-- An environment that is used as a third party information for updating the source.
type Env = Map.Map RType RType

--------------------------------------------------------------
-- The third update program
-- Update the unmatched source with another environment.
u2 :: Env -> Default -> (Record -> Record) -> Brul [Record] [Record]
u2 env dft =
  align
    (\r -> (r !! 4) > RInt 2)
    (\s v -> (s !! 0 == v !! 0) && (s !! 3 == v !! 2))
    $(update [p| (t: _: r: a: q: [])|]
             [p| (t: r: a: q: []) |]
             [d| t = Replace; r = Replace; a = Replace; q = Replace |])
    (\(t: r: a: q: []) -> (t: dft: r: a: q: []))
    (\rs -> uSwithEnv rs env)

-- suppose we know the index of
-- Album : 3
-- Quantity: 4
uSwithEnv :: Record
          -> Env
          -> Maybe Record
uSwithEnv r env =
  case Map.lookup (r !! 3) env of
    Just q  -> Just $ uRecord 4 q r
    Nothing -> Just $ uRecord 4 (RInt (-1)) r


-------------------------------------------------------------------------
-- Test

s = [[RString "Lullaby",  RInt 1989, RInt 3, RString "Galore", RInt 1]
    ,[RString "Lullaby",  RInt 1989, RInt 3, RString "Show"  , RInt 3]
    ,[RString "Lovesong", RInt 1989, RInt 5, RString "Galore", RInt 1]
    ,[RString "Lovesong", RInt 1989, RInt 5, RString "Disintegration" , RInt 4]
    ,[RString "Trust",    RInt 1992, RInt 4, RString "Wish"  , RInt 5]
    ]

v =[
    [RString "Lullaby" , RInt 4, RString "Show"  , RInt 3]
   ,[RString "Lovesong", RInt 5, RString "Disintegration" , RInt 7]
   ]


-- Mapping "Wish" to 2
env = Map.insert (RString "Wish") (RInt 2)  Map.empty

-- set default value to -1
d = RInt (-1)


brul0 :: BiGUL Source View
brul0 = emb (\s -> fromJust $ get (u0 d id) s)
                  (\s v -> fromJust $ put (u0 d (fd sfdMap vfdMap svMap v)) s v)

tb0 = showTable $ fromJust $ put brul0 s v
tf0 = showTable $ fromJust $ get brul0 s

--------------------------------------------------------------------

brul1 :: BiGUL Source View
brul1 = emb (\s -> fromJust $ get (u1 d id) s)
           (\s v -> fromJust $ put (u1 d (fd sfdMap vfdMap svMap v)) s v)

tb1 = showTable $ fromJust $ put brul1 s v
tf1 = showTable $ fromJust $ get brul1 s


brul15 :: BiGUL Source View
brul15 = emb (\s -> fromJust $ get (u15 d id) s)
           (\s v -> fromJust $ put (u15 d (fd sfdMap vfdMap svMap v)) s v)

tb15 = showTable $ fromJust $ put brul15 s v
tf15 = showTable $ fromJust $ get brul15 s

--------------------------------------------------------------------

brul2 :: BiGUL Source View
brul2 = emb (\s -> fromJust $ get (u2 env d id) s)
           (\s v -> fromJust $ put (u2 env d (fd sfdMap vfdMap svMap v)) s v)

tb2 = showTable $ fromJust $ put brul2 s v
tf2 = showTable $ fromJust $ get brul2 s


