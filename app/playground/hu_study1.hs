{- Studying notes by Zhenjiang Hu @ 22/09/2015
   It would be better to read this together with the paper
   submitted to PEPM'16 .
-}

import GHC.Generics
import Generics.BiGUL.Interpreter
import Generics.BiGUL.AST
import Language.Haskell.TH as TH hiding(Name)
import Generics.BiGUL.TH

import Data.Maybe (catMaybes)
import Data.List (find, delete)

-----------------------------------------------
--  Test on basic combinators of BiGul
-----------------------------------------------

-- We prepare two simpler functions for testing put/get of
-- a bigul lens; it will give the result if it succeeds and
-- shows an error message if it fails.

testPut :: BiGUL s v -> s -> v -> s
testPut u s v = either (error . show) id (put u s v)

testGet :: BiGUL s v -> s -> v
testGet u s = either (error . show) id (get u s)

---------------------------------
-- testing Fail, Skip and Replace
---------------------------------

{-

*Main> testGet Fail 1
Left (ErrorInfo "get failed")
*Main> testPut Fail 1 2
Left (ErrorInfo "update fails")

*Main> testGet Skip 1
Right ()
*Main> testPut Skip 1 ()
Right 1

Note that the view of Skip should be ().

*Main> testGet Replace 1
Right 1
*Main> testPut Replace 1 2
Right 2

-}

---------------------------------
-- testing source update: Update
---------------------------------

update1 :: BiGUL (a, b) (a, ())
-- update1 = Replace `Prod` Skip
update1 = $(update [p| (x,()) |] [p| (x,_) |] [d| x = Replace |])


{-

*Main> testGet update1 (1,2)
Right (1,())
*Main> testPut update1 (1,2) (100,())
Right (100,2)

-}

-- (x,_ ,_ ...) in new syntax is right associative by default
--update2 = (Replace `Prod` Skip) `Prod` Skip
update2 = $(update [p| ((x, ()), ()) |]  [p| ((x,_),_) |] [d| x = Replace |])

{-

*Main> testGet update2 ((1,2),3)
Right ((1,()),())
*Main> testPut update2 ((1,2),3) ((100,()),())
Right ((100,2),3)

-}


-- !!!!!!!!! if you use data constructors , you need add type declaration explicitly
update3 :: BiGUL (Either (a,b) c) (a,())
--update3 = RearrS
--            (PLeft $ PVar' `PProd` PVar')
--            (EDir (DLeft DVar) `EProd` EDir (DRight DVar))
--            (Replace `Prod` Skip)
update3 = $(update [p| (x,()) |] [p| Left (x,_) |] [d| x = Replace|])


{-

*Main> testGet update3 (Left (1,2))
Right (1,())
*Main> testPut update3 (Left (1,2)) (100,())
Right (Left (100,2))

*Main> testGet update3 (right (1,2))
<interactive>:66:18: Not in scope: ‘right’
*Main> testGet update3 (Right (1,2))
Left (ErrorInfo "ULeft pat not match")

-}

-- testing on other cases

{-

*Main> testGet (Update (UConst 5)) 5
Right ()
*Main> testPut (Update (UConst 5)) 5 ()
Right 5

*Main> testGet (Update (UConst 5)) 1
Left (ErrorInfo "source is not a constant.")

*Main> testGet (Update (UElem (UVar Replace) (UVar Replace))) [1,2,3]
Right (1,[2,3])
*Main> testPut (Update (UElem (UVar Replace) (UVar Replace))) [1,2,3] (100,[200,300])
Right [100,200,300]

*Main> testPut (Update (UElem (UVar Replace) (UVar Replace))) [1,2,3] (100,[200,300,400])
Right [100,200,300,400]



*Main> testGet  $(update [p| 5 |] [d| |]) 5
Right ()
*Main> testPut  $(update [p| 5 |] [d| |]) 5 ()
Right 5

*Main> testGet  $(update [p| 5 |] [d| |]) 1
Left (ErrorInfo "source is not a constant.")

*Main> testGet  $(update [p| x:xs |] [d| x = Replace; xs = Replace |]) [1,2,3]
Right (1,[2,3])
*Main> testPut  $(update [p| x:xs |] [d| x = Replace; xs = Replace |]) [1,2,3] (100,[200,300])
Right [100,200,300]

*Main> testPut  $(update [p| x:xs |] [d| x = Replace; xs = Replace |]) [1,2,3] (100,[200,300,400])
Right [100,200,300,400]
-}

-- Question: Why UOut is necessary?

---------------------------------------------------
-- testing view arrangement: Rearr
--   rearrange the view of pattern RPat to an intermediate
--   view of pattern EPat and then apply the lens between
--   the soruce and the intemediate view.
---------------------------------------------------

rearr1 :: (Eq a0, Eq b0) => BiGUL (b0, a0) (a0, b0)
--rearr1 = RearrV rp1 ep1 Replace
--  where
--    rp1 = PVar `PProd` PVar
--    ep1 = EDir (DRight DVar) `EProd` EDir (DLeft DVar)

rearr1 = $(update [p| (x,y) |] [p| (y,x) |] [d| x = Replace; y = Replace |])

{-

*Main> testGet rearr1 (1,2)
Right (2,1)
*Main> testPut rearr1 (1,2) (100,200)
Right (200,100)

-}

rearr2 :: (Eq a0, Eq b0) => BiGUL ((b0, ()), a0) (a0, b0)
--rearr2 = RearrV rp1 ep2 Replace
--  where
--    rp1 = PVar `PProd` PVar
--    ep2 = (EDir (DRight DVar) `EProd` EConst ()) `EProd` EDir (DLeft DVar)
--    u   = (Replace `Prod` Skip) `Prod` Replace

rearr2 = $(update [p| (y,x) |] [p| ((x,_),y) |] [d| x = Replace ; y = Replace|])

{-

*Main> testGet rearr2 ((1,()),2)
Right (2,1)
*Main> testPut rearr2 ((1,()),2) (100,200)
Right ((200,()),100)

-}

-- Todo: to test RElem, EElem, ECompare

---------------------------------------
-- Testing dependency in the view: Dep
---------------------------------------

dep1 :: BiGUL Int (Int, Int)
dep1 = Dep Replace (\_ v -> v+1)

{-

*Main> testPut dep1 5 (5,6)
Right 5
*Main> testPut dep1 5 (10,11)
Right 10

*Main> testPut dep1 5 (5,10)
Left dependency mismatch

-}

-----------------------------------------
-- Testing analysis on the soruce: CaseS
-----------------------------------------

cases1 :: BiGUL Int Int
--cases1 = Case [ (\s _ -> s >= 100, Normal (Fail "source should not exceed 100" ) (>= 100) )
--              , (\s _ -> s >= 0 && s < 100, Normal Replace (\s -> s >= 0 && s < 100) )
--              , (\s _ -> s < 0, Adaptive (\s v -> -s)) ]

cases1 = Case [ $(normalS [| \s -> s >= 100 |]) (Fail "source should not exceed 100")
              , $(normalS [| \s -> s >= 0 && s < 100 |]) Replace
              , $(adaptiveS [|\s -> s < 0 |]) (\s v -> -s) ]

{-

*Main> testGet cases1 90
Right 90
*Main> testGet cases1 120
Left (ErrorInfo "get failed")
*Main> errorTrace $ get cases1 (-10)
Left error
case exhausted
branch 0
  branch unmatched
branch 1
  branch unmatched
branch 2
  adaptive branch matched

*Main> testPut cases1 20 50
Right 50
*Main> testPut cases1 120 50
Left "source should not exceed 100"
*Main> testPut cases1 (-12) 50
Right 50

-}

-- todo: test over lists or trees

---------------------------------------
-- testing analysis on the view: CaseV
---------------------------------------

casev1 :: (Eq b, Eq b') => BiGUL (Either b' b) (Either b' ())
--casev1 = Case [ (\_ v  -> case v of Left _ -> True; _ -> False; ,
--                Normal (RearrS (PLeft PVar) (EDir DVar) (RearrV (PLeft PVar) (EDir DVar) Replace) ) (const True))
--              , (\_ v -> case v of Right _ -> True; _ -> False; ,
--                Normal (RearrS (PRight PVar) (EDir DVar) (RearrV (PRight (PConst ())) (EConst ()) Skip)) (const True)) ]

casev1 = Case [ $(normalV [p| Left _ |])
                  ($(update [p| Left x |] [p| Left x |] [d| x = Replace |]))
              , $(normalV [p| Right _ |])
                  ($(update [p| Right () |] [p| Right _ |] [d| |]))
         ]

{-

*Main> testGet casev1 (Left 1)
Right (Left 1)
*Main> testPut casev1 (Left 1) (Left 100)
Right (Left 100)
*Main> testGet casev1 (Right 1)
Right (Right ())
*Main> testPut casev1 (Right 1) (Right ())
Right (Right 1)

-}

----------------------------------------
-- testing source-view alignment: Align
----------------------------------------

type Persons = [(Name, Salary)]
type Name = String
type Salary = Int

-- Suppose the view contains those persons
-- who have salary greater than 10.

align1 :: BiGUL [(String, Int)] [(String, Int)]
align1 = align (\s -> salary s > 10)
               (\s v -> name s == name v)
               Replace {- case matched -}
               id {- case unmatchS -}
               (\s -> Nothing) {- case unmatchV -}
  where
    salary (n,s) = s
    name (n,s) = n


{-

*Main> testGet align1 [("a",1), ("b", 20)]
Right [("b",20)]
*Main> testPut align1 [("a",1), ("b", 20)] [("b",50), ("c",5)]
Left (ErrorInfo "created source not satisfy the source filter condition")
*Main> testPut align1 [("a",1), ("b", 20)] [("b",50), ("c",60)]
Right [("a",1),("b",50),("c",60)]
*Main> testPut align1 [("a",1), ("b", 20)] [("c",60)]
Right [("a",1),("c",60)]
*Main>

-}


align :: (Eq a, Eq b)
      => (a -> Bool)
      -> (a -> b -> Bool)
      -> BiGUL a b
      -> (b -> a)
      -> (a -> Maybe a)
      -> BiGUL [a] [b]
align p match b create conceal =
  Case [ $(normalSV [| null . filter p |] [p| [] |]) $
           $(rearrV [| \ [] -> () |]) Skip
       , $(adaptiveSV [| not . null . filter p |] [p| [] |]) $ -- conceal == delete
           \ss _ -> catMaybes (map (\s -> if p s then conceal s else Just s) ss)

         -- preserve the items not satisfying predicate p in the source
       , $(normalSV [| \ss -> not (null (filter p ss)) && not (p (head ss)) |] [p| _:_ |]) $
           $(update [p| vs |] [p| _:vs |] [d| vs = align p match b create conceal |])
       , $(normal' [| \ss vs -> not (null (filter p ss)) && p (head ss) && match (head ss) (head vs) |]
                   [| \ss -> not (null (filter p ss)) && p (head ss) |]) $
           $(update [p| v : vs |] [p| v : vs |] [d| v  = b; vs = align p match b create conceal |])
       , $(adaptiveV [p| _:_ |]) $
           \ss (v:_) -> case find (flip match v) (filter p ss) of
                          Nothing -> create v:ss
                          Just s  -> s: delete s ss ]


