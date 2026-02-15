{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric, ViewPatterns, ScopedTypeVariables, TemplateHaskell #-}

import Generics.BiGUL.Error
import Generics.BiGUL.AST
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH
import Data.List
import Data.Maybe
import Control.Monad.Except
import GHC.Generics

{-
################################

basic putback operators

Replace :: BiGUL s s
Skip :: BiGUL s ()
Fail :: String -> BiGUL s v

################################
-}

tp1 = put Replace 1 2
tg1 = get Replace 1

tp2 = put Skip 1 ()
tg2 = put Skip 1

tp3 = put (Fail "I am wrong here") 1 2
tg3 = get (Fail "I am wrong here") 1

{-
################################

Pair updating 
Prod :: BiGUL s1 v1 -> BiGUL s2 v2 -> BiGUL (s1,s2) (v1,v2)

################################
-}

bx1 :: BiGUL (s1,(s2,s3)) (s1,((),s3)) 
bx1 = Replace `Prod` (Skip `Prod` Replace)

-- put bx1 (1,(2,3)) (100,((),200))

{-

More convenient way of pair updating (using meta programming)
$(update  [p| vpat |]  [p| spat |]  [d| bs |])

-}

bx1' = $(update [p| (x,((),z)) |] [p| (x,(_,z)) |] [d| x = Replace; z = Replace |])

-- Note that the view pattern should be fully described.
-- So it is wrong if we replace x,((),z)) by x,(_,z))

-- With update, we can do very complicated update (dealing with
-- more complicated structure and correspondence

bx2 :: BiGUL (s1,((s2,s3),s4)) (s3,s1)
bx2 = $(update [p| (x,y) |] [p| (y,((_,x),_)) |] [d| x = Replace; y = Replace |])

bx2' :: BiGUL (s1,((s2,s3),s4)) (s3,s1)
bx2' = $(rearrS [| \(x1,((x2,x3),x4)) -> (x3,x1) |]) Replace

-- put bx2 (1,((2,3),4)) (300,100)

jin1 :: Eq s => BiGUL (s,s) s
jin1 = $(rearrV [| \v -> (v,v) |]) Replace


-- What if we wish to consider more involved structures such as lists or trees?

{-
################################

Rearrangement of the structure of source/view

RearrS 
$(rearrS [| structural transformation :: str1 -> str2 |]) ::
  BiGUL str2 v -> BiGUL str1 v
* the structural transformation can lose information
  but should not be duplciated

$(rearrV [| structural transformation :: str1 -> str2 |]) ::
  BiGUL s str2 -> BiGUL s str1 
* the structural transformation can be duplicated
  but can lose information

Note for rearrV, for the information should not be lost in the
structural transformation (but allow duplication).

################################
-}

bx3 :: BiGUL [s] s
bx3 = $(rearrS [| \(s:_) -> s |]) Replace

-- put bx3 [1,2,3,4] 100

bx3' :: BiGUL [s] s
bx3' = $(rearrS [| \(x:xs) -> x |]) Replace

jin2 :: Int -> BiGUL [s] s
jin2 i = if i == 0 then bx3
         else $(rearrS [| \(x:xs) -> (x,xs) |]) $
                 $(rearrV [| \v -> ((), v) |]) $
                    Skip `Prod` jin2 (i-1) 

-- Note that bx3' could be written as follows without using
-- weaker application $.

bx3'' :: BiGUL [s] s
bx3'' = $(rearrS [| \(x:xs) -> (x,xs) |]) 
          ($(rearrV [| \v -> (v, ()) |])
             (Replace `Prod` Skip))

-- What if the original source is empty for bx3?
-- Could we report an error or adapt the source to a form
-- that can be applied by bx3?

{-
################################

Case Analysis
Case [ $(normal [| cond1 :: s -> v -> Bool |]) $ bx1,
       $(normal [| cond2 :: s -> v -> Bool |]) $ bx2,
       ...,
       $(normal [| condn :: s -> v -> Bool |]) $ bxn,
       $(adaptive [| cond1' :: s -> v -> Bool |]) $ (f1 :: s -> v -> s),
       $(adaptive [| cond2' :: s -> v -> Bool |]) $ (f2 :: s -> v -> s),
       ...
     ]

################################
-}

-- improve bx3 by returning a suitable error message if the source is empty
bx4 :: BiGUL [s] s
bx4 = Case [
        $(normal [| \s v -> not (isEmpty s) |]) $ bx3,
        $(normal [| \s v -> isEmpty s |]) $
           Fail "It is wrong to putback on an empty source"
      ]

isEmpty [] = True
isEmpty _ = False

-- improve bx3 by adapt the soruce to an nonempty one if the source is empty
bx5 :: BiGUL [Int] Int
bx5 = Case [
        $(normal [| \s v -> not (isEmpty s) |]) $ bx3,
        $(adaptive [| \s v -> isEmpty s |]) $ (\s v -> [undefined,0,0,0])
      ]

-- Suppose that we want to replace all the elements in the source with the view.
-- We could implement it by recursive applying the Replace to each element of the source.
bx6 :: Eq s => BiGUL [s] s
bx6 = Case [
        $(normal [| \s v -> length s == 1 |]) $
          $(rearrS [| \[x] -> x |]) Replace,
        $(normal [| \s v -> length s > 1 |]) $
           $(rearrS [| \(x:xs) -> (x,xs) |]) $
              $(rearrV [| \v -> (v, v) |]) $
                  Replace `Prod` bx6,
        $(adaptive [| \s v -> length s == 0 |]) $
           \s v -> [undefined]
      ]

-- Let us see another recursive definition, which use the view to
-- update the ith element in the source.

iter :: Eq v => BiGUL s v -> BiGUL [s] v
iter bx = Case [
        $(normal [| \s v -> length s == 1 |]) $
           $(rearrS [| \[x] -> x |]) bx,
        $(normal [| \s v -> not (isEmpty s) |]) $
           $(rearrS [| \(x:xs) -> (x,xs) |]) $
              $(rearrV [| \v -> (v, v) |]) $
                  bx `Prod` iter bx,
        $(adaptive [| \s v -> isEmpty s |]) $
           \s v -> [undefined]
      ]

-- now bx6 can be defined as follows
bx6' :: Eq s => BiGUL [s] s
bx6' = iter Replace

jin3 :: Eq s1 => BiGUL [(s1,s2)] s1

-- put jin3 [(1,100),(2,200),...] 9999
-- [(9999,100),(9999,200), ...]

jin3 = iter upFst
  where upFst = $(rearrS [| \(x,y) -> x |]) Replace

-- Application: View-based adaptation rule:
-- put (jin4 2) [1,2,3,4,5] 100 ==> [1,2,100,99,5]
--  [x0,x1,x2,x3,x4]: v==100 |- x0 ==1 => x3 := 99
--                    v==0   |- x0==1 && x1==2 => x3 := 88
jin4 :: Int -> BiGUL [Int] Int
jin4 i = Case [
            $(adaptive [| \s v -> v==100 && s !! 0 == 1 && s !! 3 /= 99 |]) $
                \s v -> set s 3 99,
            $(adaptive [| \s v -> v==0 && s !! 0 == 1 &&
                                          s !! 1 == 2 &&
                                          s !! 3 /= 88 |]) $
                \s v -> set s 3 88,
            $(normal [| \s v -> True |]) $
                jin2 i
             ]
  where set s i v = take i s ++ [v] ++ drop (i+1) s

-- "safe" embedding for introducing library functons from outside
emb :: Eq v => (s -> v) -> (s -> v -> s) -> BiGUL s v
emb g p = Case
  [ $(normal [| \x y -> g x == y |])$
      $(rearrV [| \x -> ((), x) |])$
        Dep Skip (\x () -> g x)
  , $(adaptive [| \_ _ -> True |])
      p
  ]

-- demonstration of encoding iso instead of just Replace
-- put jin5 3 9 = 8
-- get jin5 8 = 9     get: increase by 1, put: decrease by 1
jin5 :: BiGUL Int Int
jin5 = emb inc1 dec1
  where inc1 s = s+1
        dec1 s v = v-1

-- consistenct: (3,4) <-> (4,4)
-- (8,8) -> (9,8)
-- (9,10) <-> (10,10)

jin6 :: BiGUL (Int,Int) (Int,Int)
jin6 = jin5 `Prod` Replace

-- make "Case" be more intuitive
infixr 0 ==>
(==>) :: (a->b) -> a -> b
(==>)  = ($)

-- now bx6 can be written as follows.

bx6'' :: Eq s => BiGUL [s] s
bx6'' = Case [
        $(normal [| \s v -> length s == 1 |]) 
          ==> $(rearrS [| \[x] -> x |]) Replace,
        $(normal [| \s v -> length s > 1 |])
          ==> $(rearrS [| \(x:xs) -> (x,xs) |]) $
                 $(rearrV [| \v -> (v, v) |]) $
                   Replace `Prod` bx6'',
        $(adaptive [| \s v -> length s == 0 |]) 
          ==> \s v -> [undefined]
      ]

