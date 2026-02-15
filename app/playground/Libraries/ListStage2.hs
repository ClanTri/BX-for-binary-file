{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module ListStage2 where

import ListStage1

import GHC.Generics
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter

import Language.Haskell.TH as TH hiding (Name)

import Data.Typeable
import Data.List (elemIndex, maximum)
import Data.Maybe (catMaybes)
import Data.List (find, delete, inits, tails)


-- TODO
-- put semantics. composition.
-- higher order function
-- linear time complexity fold

-- map composition . map fold . higher order function
-- linear time complexity fold

bHead  = get lensHead
bpHead = put lensHead

bLast  = get lensLast
bpLast = put lensLast

bTail  :: (Typeable a, Defaultable a) => [a] -> GetResult [a] [a]
bTail  = get lensTailNonStrict
bpTail :: (Typeable a, Defaultable a) => [a] -> [a] -> PutResult [a] [a]
bpTail = put lensTailNonStrict

bTail'  = get lensTailStrict
bpTail' = put lensTailStrict

bInit  :: (Typeable a, Defaultable a) => [a] -> GetResult [a] [a]
bInit  = get lensInitNonStrict
bpInit :: (Typeable a, Defaultable a) => [a] -> [a] -> PutResult [a] [a]
bpInit = put lensInitNonStrict

bInit'  = get lensInitStrict
bpInit' = put lensInitStrict

bNull  = get lensNull
bpNull = put lensNull

bLength  :: (Typeable a, Defaultable a) => [a] -> GetResult [a] Int
bLength  = get lengthEmb
bpLength :: (Typeable a, Defaultable a) => [a] -> Int -> PutResult [a] Int
bpLength = put lengthEmb

bLensDrop :: (Typeable a, Defaultable a) => Int -> [a] -> GetResult [a] [a]
bLensDrop n = get (lensDrop n)

bpLensDrop :: (Typeable a, Defaultable a) => Int -> [a] -> [a] -> PutResult [a] [a]
bpLensDrop n = put (lensDrop n)

bLensMaximum :: (Ord a) => [a] -> GetResult [a] a
bLensMaximum  = get lensMaximum

bpLensMaximum :: (Ord a) => [a] -> a -> PutResult [a] a
bpLensMaximum  = put lensMaximum

bLensMinimum :: (Ord a) => [a] -> GetResult [a] a
bLensMinimum = get lensMinimum

bpLensMinimum :: (Ord a) => [a] -> a -> PutResult [a] a
bpLensMinimum = put lensMinimum

bReplicate :: (Eq a) => Int -> a -> GetResult a [a]
bReplicate n = get (lensReplicate n)

bpReplicate :: (Eq a) => Int -> a -> [a] -> PutResult a [a]
bpReplicate n = put (lensReplicate n)

bRotate  = get lensGetRotate
bpRotate = put lensGetRotate

bReverse  = get lensReverse
bpReverse = put lensReverse

bTakeWhile  p = get (lensTakeWhile p)
bpTakeWhile p = put (lensTakeWhile p)

bDropWhile  p = get (lensDropWhile p)
bpDropWhile p = put (lensDropWhile p)




-- foldr ::   (a -> b -> b)   ->   b -> [a] -> b
-- lensFoldr :: (BiGUL (x, xs) result) -> (BiGUL ([x], e) result)
lensFoldr :: (BiGUL (a, b) b) -> (BiGUL ([a], b) b)
lensFoldr bx =
  Case  [ $(normalS [| \(s, e) -> length s == 0 |] ) $
            $(rearrV [| \v -> ((),v) |]) $
              $(update [p| ((),v ) |] [p| (_, v) |] [d| v = Replace |])
        , $(normalSV [p| _ |] [p| _ |] ) $
            $(rearrS [| \((x:xs), e) -> (x, (xs,e))  |])
              (Replace `Prod` lensFoldr bx)
              `Compose`
              bx
        ]

-- get (lensFoldr (+) 0) [1,2,3,4]  ==  10
-- put (lensFoldr plusl) ([1,2,3], 0) 8 -> ([3,2,3], 0)
-- \([1,2,3],0) -> (1, ([2,3], 0))
-- get (Replace `Prod` lensFoldr bx) (1, ([2,3], 0)) == (1, 5)
-- put bx (1, 5) 8 = (3, 5)
-- put (Replace `Prod` lensFoldr bx) (1, ([2,3], 0)) (3, 5) == (3, put (lensFoldr bx) ([2,3], 0) 5 )
-- == ...
-- ==
-- Note: the get direction of lensFoldr is executed many times (as length of the source list). time complexity is quadratic.
-- (1, sum(2,3,4)) 12 = (1, 9) 12 = (3, 9)
-- (2, sum(3,4)) 9    = (2 , 7) 9 = (2, 7)
-- ...

-- the wrong version but the same as List.foldr1.
lensFoldr1 :: (BiGUL (a, a) a) -> (BiGUL [a] a)
lensFoldr1 bx =
  Case  [ $(normalS [| \s -> length s == 1 |] ) $
            $(update [p| v |] [p| [v] |] [d| v = Replace |])
        , $(normalSV [p| _ |] [p| _ |] ) $
            $(rearrS [| \(x:xs) -> (x,xs)  |])
              (Replace `Prod` lensFoldr1 bx)
              `Compose`
              bx
        ]


-- correct version of lensFoldr1.
lensFoldr1C :: BiGUL a b -> BiGUL (a, b) b -> (BiGUL [a] b)
lensFoldr1C f bx =
  Case  [ $(normalS [| \s -> length s == 1 |] ) $
            $(update [p| v |] [p| [v] |] [d| v = f `Compose` Replace |])
        , $(normalSV [p| _ |] [p| _ |] ) $
            $(rearrS [| \(x:xs) -> (x,xs)  |])
              (Replace `Prod` lensFoldr1C f bx)
              `Compose`
              bx
        ]


-- works only when (length s == length v) holds
lensMap :: BiGUL a b -> BiGUL [a] [b]
lensMap bx = $(rearrS [| \s -> (s, []) |])
  (lensFoldr ($(rearrV [| \(v:vs) -> (v,vs) |]) $ bx `Prod` Replace))

-- compose hell. I am not clear about the put semantics ... and the time complexity
lensScanr :: (Eq b) => (BiGUL (a, b) b) -> (BiGUL ([a], b) [b])
lensScanr bx =
  Case  [ $(normalS [| \(s, e) -> length s == 0 |] ) $
            $(rearrV [| \[v] -> ((),v) |]) $
              $(update [p| ((),v ) |] [p| (_, v) |] [d| v = Replace |])
        , $(normalSV [p| _ |] [p| _ |] ) $
            $(rearrS [| \((x:xs), e) -> (x, (xs,e))  |]) $
              (((Replace `Prod` lensScanr bx) -- source (x, xs) --> (x, done)
                  `Compose`
                    $(rearrS [| \(a, b) -> (a, (b, b))  |]) (Replace `Prod` lensHead `Prod` Replace)) -- (x, (done, done)) --> (x, (head done, done))
                      `Compose`
                      $(rearrS [| \(a, (hb, b)) -> ((a, hb), b)  |]) (bx `Prod` Replace)) -- ((x, head done), done) --> (f x (head done), done)
                        `Compose`
                        lensCons -- f x (head done) : done
        ]


replaceByPosition :: BiGUL [a] [a]
replaceByPosition = Case  [ $(normalSV [p| [] |] [p| [] |] )
                              $(update [p| [] |] [p| [] |] [d| |])
                          , $(normalSV [p| _:_ |] [p| _:_ |] )
                              $(update [p| x:xs |] [p| x:xs |] [d| x = Replace; xs = replaceByPosition |])
                     ]

-- put semantics: replace the first element in list with the provided one
-- work only with non-empty list
lensHead :: BiGUL [a] a
lensHead = lensFoldr1 ($(update [p| v |] [p| (v,_) |] [d| v = Replace |]))


-- work only with non-empty list
lensLast :: BiGUL [a] a
lensLast = Case [$(normalS [p| [_] |])
                  $(update [p| x |] [p| x:[] |] [d| x = Replace  |])
              ,$(normalS [p| _:_ |])
                  $(update [p| x |] [p| _:x |] [d| x = lensLast |])  ]


-- giving a non-empty source list,
-- replace its tail list with a view list of any length
lensTailNonStrict :: (Typeable a, Defaultable a) => BiGUL [a] [a]
lensTailNonStrict = Case  [ $(adaptive [|\s v -> length s /= 1 + length v |])
                              (\s v ->  if length s > length v
                                          then drop (length s - (1 + length v)) s -- try to preserve the source (the rear part)
                                          else replicate (1 + length v) defaultVal)
                          , $(normalS [p| _:_ |])
                              $(update [p| x |] [p| _:x |] [d| x = replaceByPosition |])
                          , $(normalS [| null |])
                              (Fail "empty source list detected")
                          ]

-- given a non-empty source list of length n,
-- replace its tail list with a view list of length (n-1)
lensTailStrict :: BiGUL [a] [a]
lensTailStrict = Case [ $(normal [|\s v -> length s /= 1 + length v |])
                          (Fail "length mismatch")
                      , $(normalS [p| _:_ |])
                            $(update [p| x |] [p| _:x |] [d| x = replaceByPosition |])
                      , $(normalS [| null |])
                           (Fail "empty source list detected")
                      ]


lensInitNonStrict :: (Typeable a, Defaultable a) => BiGUL [a] [a]
lensInitNonStrict =
  Case  [ $(adaptive [|\s v -> length s /= 1 + length v |])
              (\s v ->  if length s > length v
                          then take (1 + length v) s -- try to preserve the source (the front part). especially the last element.
                          else replicate (1 + length v) defaultVal)  -- all the elements in source should be replaced by view later. and we need a default element as the last element in the source.
        , $(normalSV [p| _:_ |] [p| _:_ |] )
            $(update [p| x:xs |] [p| x:xs |] [d| x = Replace; xs = lensInitStrict |])
        , $(normalSV [p| [_] |] [p| [] |] )
            $(update [p| [] |] [p| [_] |] [d| |])
        , $(normalS [| null |])
             (Fail "empty source list detected")
        ]


-- given a non-empty source list of length n,
-- replace its init list with a view list of length (n-1)
lensInitStrict  :: BiGUL [a] [a]
lensInitStrict = Case [ $(adaptive [|\s v -> length s /= 1 + length v |])
                        (\_ _ -> error "length mismatch")
                      , $(normalSV [p| _:_ |] [p| _:_ |] )
                            $(update [p| x:xs |] [p| x:xs |] [d| x = Replace; xs = lensInitStrict |])
                      , $(normalSV [p| [_] |] [p| [] |] )
                            $(update [p| [] |] [p| [_] |] [d| |])
                      , $(normalS [| null |])
                           (Fail "empty source list detected")
                      ]


lensNull :: BiGUL [a] Bool
lensNull = Case [$(normalSV [p| [] |] [p| True |]  ) $
                    $(rearrV [| \True -> () |])
                      $(update [p| () |] [p| _ |] [d|  |])
                , $(adaptiveSV [p| [] |] [p| False |] )
                    (\_ _ -> [undefined])
                , $(adaptiveSV [p| _:_ |] [p| True |] )
                    (\_ _ -> [])
                , $(normalSV [p| _:_ |] [p| False |] ) $
                    $(rearrV [| \False -> ()  |])
                      $(update [p| () |] [p| _ |] [d|  |])
                ]


-- put semantics : replace the first n elements of the source list
-- well-bahaved state consistency: length v <= n
-- parameters should satisfy either length s < n && length s == length v
-- or length s >= n && length v == n
lensTake :: Int -> BiGUL [a] [a]
lensTake n =
  Case  [ $(normal [| \s v -> let c1 = length s < n && length s == length v
                                  c2 = length s >= n && length v == n
                              in  not (c1 || c2) |])
            (Fail $ "parameters should satisfy either length s < n && length s == length v\n" ++ "or length s >= n && length v == n\n")  -- for PutGet:  get (take n) s (put (take n) s v)
        -- condition c1
        ,$(normal [| \s v -> length s == 0 && length v == 0 && n > 0 |]  ) $
            $(update [p| [] |] [p| [] |] [d|  |])
        -- condition c1
        , $(normal [| \s v -> length s > 0 && length s == length v && n > length s |]  ) $
            $(update [p| x:xs |] [p| x:xs |] [d| x = Replace; xs = lensTake (n-1) |])
        -- condition c2
        , $(normal [| \s v -> length s >= 0 && length v == 0 && n == 0 |]  ) $
            $(update [p| [] |] [p| _ |] [d| |])
        -- condition c2
        , $(normal [| \s v -> length s > 0 && n == length v && length v > 0 |]  ) $
            $(update [p| x:xs |] [p| x:xs |] [d| x = Replace; xs = lensTake (n-1) |])
        ]

-- put semantics: replace the last n elements of a source list
-- well-behaved state consistency: length s <= n + length v
-- well-behaved state consistency (precise): length s = n + length v || (length s <= n && length v = 0)
lensDrop :: (Typeable a, Defaultable a) => Int -> BiGUL [a] [a]
lensDrop n =
  Case  [ $(adaptive [| \s v -> not (length s == n + length v || (length s <= n && length v == 0) ) |] ) $
            (\s v ->  let ls = length s
                          lv = length v
                      in  if lv > ls - n
                            then  if ls >= n -- should add missing elements to the source
                                    then
                                      -- source is larger enough, just keep the first n elements and replace the remainings)
                                      take n s ++ v
                                    else
                                      -- source is not larger enough. should fill the source into a proper length according to n. then concate the view to the source.
                                      (replicate (n - ls) defaultVal) ++ s ++ v
                            else  if ls - n >= 0
                                    then
                                      -- source is large enough. should preserve the first n elements. then concate view to the source
                                      take n s ++ v
                                    else
                                      -- source is not large enough. enlarge the source. then concate the view.
                                      (replicate (n - ls) defaultVal) ++ s ++ v
            )
        , $(normal [| \s v -> n > 0 && (length s == n + length v || (length s <= n && not (null v))) |] )
            -- skip the first n elements
            $(update [p| xs |] [p| _:xs |] [d| xs = lensDrop (n-1) |])
        , $(normal [| \s v -> n == 0 && (length s == n + length v || (length s <= n && not (null v) )) |] )
            $(update [p| xs |] [p| xs |] [d| xs = replaceByPosition |])
            -- caution. pattern overlaps. this incomplete condition should be put last
        , $(normal [| \s v -> null v |] )
            $(update [p| [] |] [p| _ |] [d|  |])
        ]


-- work on non-empty list
-- put semantics: replace the maximum value with a new one
-- pre condition: in general, the view should be no less than the second large element in the source
-- however, we enforce the view should be larger than the second largest element. The following situation is prohibited:
-- put [4,5,6,1] 5 -> [4,5,5,1]. the first 5 is chosen when performing get. though the result is the same, the branch is different.
-- however after revision. put [6,6,6] 7 is valid - TODO
lensMaximum :: (Ord a) => BiGUL [a] a
lensMaximum = lensFoldr1 lensMaxInner

lensMaxInner :: (Ord a) => BiGUL (a, a) a
lensMaxInner =
  Case  [ $(normal [| \(elem, acc) v -> elem >= acc && v > acc |] ) $
            $(rearrV [| \v -> (v,()) |]) $
              Replace `Prod` Skip
        , $(normal [| \(elem, acc) v -> elem < acc && v > elem |] ) $
            $(rearrV [| \v -> ((),v) |]) $
              Skip `Prod` Replace
        , $(normal [| \ _ _ -> True |] ) (Fail "Possible reason: the view is less than the second largest elements in source")
        ]


-- work on non-empty list
-- put semantics: replacet he minimum value with a new one
-- explanation: the same as the maximum functions.
lensMinimum :: (Ord a) => BiGUL [a] a
lensMinimum = lensFoldr1 lensMinInner

lensMinInner :: (Ord a) => BiGUL (a, a) a
lensMinInner =
  Case  [ $(normal [| \(elem, acc) v -> elem <= acc && v < acc |] ) $
            $(rearrV [| \v -> (v,()) |]) $
              Replace `Prod` Skip
        , $(normal [| \(elem, acc) v -> elem > acc && elem > v |] ) $
            $(rearrV [| \v -> ((),v) |]) $
              Skip `Prod` Replace
        , $(normal [| \ _ _ -> True |] ) (Fail "Possible reason: the view is larger than the second least elements in source")
        ]


-- put semantics: check if the elements in view list are all the same. if not, report error.
-- if the predicate holds, replace the source with any (in fact all) element in the view list.
-- problem: the additional restriction (Eq a) have to be introduced.
lensReplicate :: (Eq a) => Int -> BiGUL a [a]
lensReplicate n =
  Case  [ $(normal [| \s v -> length v /= n || not (sameElems' v) |] ) $
            (Fail $ "the length of the v is not the same as parameter n. Or elements in view are not the same.")
        , $(normal [|\_ v -> null v && n == 0  |] ) $
            $(update [p| [] |] [p| _ |] [d| |])
        , $(normal [|\_ v -> length v == 1 && n == 1  |] ) $
            $(update [p| [v] |] [p| v |] [d| v = Replace;|])
        , $(normalSV [p| _ |] [p| _:_ |] ) $
            $(rearrS [|\s -> (s,s) |]) $
              $(update [p| v:vs |] [p| (v,vs) |] [d| v=Replace; vs = lensReplicate (n-1) |])
        ]


-- put direction is rorate: put [] [1,2,3] -> [2,3,1]; get [1,2,3] = [3,1,2]
lensPutRotate :: BiGUL [a] [a]
lensPutRotate =
  Case  [ $(adaptive [|\s v -> length s /= length v |]) (\_ v -> v) -- just for convenience.
        , $(normalSV [p| [] |] [p| [] |] ) $
            $(update [p| [] |] [p| [] |] [d| |])
        , $(normalSV [p| [x] |] [p| [x] |] ) $
             $(update [p| [x] |] [p| [x] |] [d| x = Replace |])
        , $(normalSV [p| x1:x2:r |] [p| x1:x2:r |]) $
            $(rearrV [| \(x1:x2:r) -> (x2,(x1:r)) |]) $
             $(update [p| (x2,r) |] [p| x2:r |] [d| x2=Replace; r = lensPutRotate |])
        ]


-- get direction is rotate: put [] [1,2,3] -> [3,1,2]; get [1,2,3] -> [2,3,1]
lensGetRotate :: BiGUL [a] [a]
lensGetRotate =
  Case  [ $(adaptive [|\s v -> length s /= length v |]) (\_ v -> v) -- just for convenience. generate a source of length v
        , $(normalSV [p| [] |] [p| [] |] ) $
            $(update [p| [] |] [p| [] |] [d| |])
        , $(normalSV [p| [x] |] [p| [x] |] ) $
             $(update [p| [x]  |] [p| [x] |] [d| x = Replace |])
        , $(normalSV [p| x1:x2:r |] [p| x1:x2:r |]) $
            ($(rearrV [| \(x1:x2:r) -> (x2,(x1:r)) |]) $
               $(update [p| (x2,r) |] [p| x2:r |] [d| x2=Replace; r = replaceByPosition |])) --replace for simplicity
            `Compose`
            ( $(rearrV [| \(x1:x2:r) -> (x1,(x2:r)) |]) $
                $(update [p| (x1,r) |] [p| x1:r |] [d| x1=Replace; r = lensGetRotate |]) --replace for simplicity
              :: BiGUL [a] [a] )
        ]


lensReverse :: BiGUL [a] [a]
lensReverse =
  Case  [ $(adaptive [| \s v -> not (length s == length v) |]) (\_ v -> v) -- for simplicity
        , $(normalSV [p| [] |] [p| [] |])
            $(update [p| [] |] [p| [] |] [d|  |])
        , $(normalSV [p| _:_ |] [p| _:_ |] ) $
            ($(update [p| x:xs |] [p| x:xs |] [d| x = Replace; xs = lensReverse |]))
              `Compose` lensGetRotate
        ]


lensTakeWhile :: (a -> Bool) -> BiGUL [a] [a]
lensTakeWhile p =
  Case  [ $(normal [| \_ v -> not (all p v)  |] ) (Fail "some elements in view does not satisfy predicate p")
        , $(normal [| \s v -> null v && (null s || (not . p . head) s) |]) $
              $(rearrV [| \[] -> () |]) $
                $(update [p| vs |] [p| vs |] [d| vs = Skip |])
        , $(normal [| \s v -> not (null v) && not (null s) && p (head s)  |]) $
              $(update [p| x:xs |] [p| x:xs |] [d| x = Replace; xs = lensTakeWhile p |])
        , $(adaptive [| \s v -> null v && p (head s)  |] ) (\s _ -> dropWhile p s) -- the elemetns in source still satisfying p should be deleted.

        , $(adaptive [| \s v -> not (null v) && null s |]) (\s v -> [head v] )     -- enlarge the source
        , $(adaptive [| \s v -> not (p (head s)) && not (null v) |] ) (\s v -> head v : s) --inserte elements from the view into the source
        ]


-- somehow like the lensDrop, the putback semantics can be tricky and various...
-- the elements in view do not necessarily need to not satisfy predicate p:
-- (a possible implementation) put lensDropWhile (< 3) [1,2,3,4] [33,44,1,2] = [1,2, 33,44,1,2]. get (...) = [33,44,1,2] == v
-- specification of the put : 1 verify the view. a valid view is either (p (head v) == false), or (v is empty).
-- 2 find the first element (k) in source satisfying p. 3 replace the remaning source with view from k.
-- if you were going to use a single BiGUL function performing local changes, everying shall be in a mess.
lensDropWhile :: (a -> Bool) -> BiGUL [a] [a]
lensDropWhile p =
  Case  [ $(normal [| \_ v -> not (null v) && p (head v) |] ) (Fail "the head element in view should not satisfy predicate p")
        -- view is null. so we can only preserve some elements in source satisfying p.
        -- all elements satisfy p. Skip.
        , $(normal [| \s v -> all p s && null v |] ) ($(update [p| [] |] [p| _ |] [d|  |]))
        -- only some elements satisfy p. we preserve the elements satisfying p to the points where (p elem) does not hold.
        -- after adaptation, (all p s) is satisfied. and thus this branch will be visited at most once.
        , $(adaptive [| \s v -> not (all p s) && null v |] ) (\s _ -> takeWhile p s)
        , $(normalSV [p| _ |] [p| _ |]) (lensDropWhile_ p)
        ]

lensDropWhile_ :: (a -> Bool) -> BiGUL [a] [a]
lensDropWhile_ p =
  Case  [ -- preserve the elements in source satisfying p
          $(normal [| \s v -> not (null s) && not (null v) && p (head s) |]) $
            $(rearrS [| \(x:xs) -> xs |]) $
              lensDropWhile_ p
        , $(normalSV [p| _ |] [p| _ |])
            (lensDropWhile__ p)
        ]

-- basically 2 cases are enough:
--  $(normal [| \s v -> s == v |]) $
--    Replace
--, $(adaptive [| \s v -> not (null v) && v /= s |]) (\_ v -> v) -- v is never empty. concate remaining view to the source
-- use several cases to eliminate the Eq Class restriction.
lensDropWhile__ :: (a -> Bool) -> BiGUL [a] [a]
lensDropWhile__ p =
  Case  [ $(normalSV [p| _:_ |] [p| _:_ |] ) $
            $(update [p| x:xs |] [p| x:xs |] [d| x = Replace; xs = lensDropWhile__ p |])
        , $(normalSV [p| [] |] [p| [] |] ) $
            $(update [p| [] |] [p| [] |] [d| |])
        , $(adaptiveSV [p| [] |] [p| _:_ |] ) (\[] v -> [head v] )
        , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\_ [] -> [] )
        ]



-- put semantics: check if view is a valid inits. Then generate the original list from view only
-- source <-> view is just isomorphism. data could be fully recovered from either side. tedious.
-- the general framework for isomorphisms is: adaptive + recursively Replace

-- initial value
-- (rearranged)source ( 1, [[],[2],[2,3],[2,3,4]] ) -> ([ [], [1],[2],[2,3],[2,3,4]] )
-- view       [[],[1],[1,2],[1,2,3],[1,2,3,4]]
lensInits :: Eq a => BiGUL [a] [[a]]
lensInits =
  Case  [ $(adaptive  [| \s v -> isInits v && length s /= length v - 1 |]) (\_ v -> replicate (length v - 1) undefined)
        , $(normalSV [p| _ |] [p| _ |] ) $
            $(rearrS [| \s -> (s, [[]]) |]) (lensFoldr xd)
        ]
  where
    -- seperate three conditions into two functions for simplicity
    -- the view has one more element thant the source. so the first function handle the first element in the view, which is always an empty list ([]).
    -- after that, pass the remaining data to the second function.
    xd :: Eq a => BiGUL (a, [[a]]) [[a]]
    xd =
      Case  [ $(normalSV [p| (_, []:_) |] [p| []:_ |] ) $ -- view is [[],[1],[1,2] ...] there is at least one element, and the first element is []
                $(update [p| ([]:vs) |] [p| vs |] [d| vs = xd1 |])
            ]
    xd1 :: Eq a => BiGUL (a, [[a]]) [[a]]
    xd1 =
      Case  [ $(normalSV [p| (_, [_]) |] [p| [_:_] |] ) $ -- there is only one element left.
                $(rearrS [| \(s, [t]) -> s:t |]) $        -- ( 1, [[2,3,4]] )  --> 1:[2,3,4]
                  $(rearrV [| \[v] -> v |]) $             -- [[1,2,3,4]]       --> [1,2,3,4]
                    Replace

            , $(normal' [| \s v -> case (s, v) of ( (_, _:_) , (_:_):_) -> True; _ -> False |] [p| (_, _:_:_) |]) $
                $(rearrS [| \(s, t:ts) -> (s:t, (s,ts) ) |]) $ -- ( 1, [[],[2],[2,3],[2,3,4]] )  --> ( 1:[], (1, [[2],[2,3],[2,3,4]]) )
                  $(rearrV [| \(v:vs) -> (v,vs) |]) $          -- [[1],[1,2],[1,2,3],[1,2,3,4]]  --> ([1]  , [[1,2],[1,2,3],[1,2,3,4]])
                    Replace `Prod` xd1
            ]


-- put semantics: check if view is a valid tails. then generate the original list from view only
-- using put to write simple isomorphisms is always tedious.
-- adaptive is used for reshaping source.
-- (rearranged)source ( 1 , [[2,3,4],[3,4],[4],[]] )
-- view       [[1,2,3,4],[2,3,4],[3,4],[4],[]]
lensTails :: Eq a => BiGUL [a] [[a]]
lensTails =
  Case  [ $(adaptive  [| \s v -> isTails v && length s /= length v - 1 |]) (\_ v -> replicate (length v - 1) undefined)
        , $(normalSV [p| _ |] [p| _ |] ) $
            $(rearrS [| \s -> (s,[[]]) |]) $
              lensFoldr xd
        ]
  where
    xd :: Eq a => BiGUL (a,[[a]]) [[a]]
    xd = $(rearrS [| \(s, t:ts) -> (s:t, t:ts) |])
            $(update [p| v:vs |] [p| (v, vs) |] [d| v = Replace ; vs = Replace |])


---- trivial well-behaved wrapper
emb :: Eq v => (s -> v) -> (s -> v -> s) -> BiGUL s v
emb g p = Case
  [ $(normal [| \x y -> g x == y |])$
      $(rearrV [| \x -> ((), x) |])$
        Dep Skip (\x () -> g x)
  , $(adaptive [| \_ _ -> True |])
      p
  ]


-- for now we can only use emb or Nat datatype.
lengthEmb :: (Typeable a, Defaultable a) => BiGUL [a] Int
lengthEmb = emb (length)
  (\s v ->
    let ls = length s
    in  if ls == v
          then s
          else  if ls > v
                  then drop (ls - v) s
                  --else  if ls == 0
                  --        then error "a proper source cannot be generated: the original source is empty and the type is unknown"
                          else s ++ (replicate (v - ls) defaultVal))


lensFoldrMapFusion :: (BiGUL a c) -> (BiGUL (c, b) b) -> (BiGUL ([a], b) b)
lensFoldrMapFusion mapBX foldBx =
  Case  [ $(normalS [| \(s, e) -> length s == 0 |] ) $
            $(rearrV [| \v -> ((),v) |]) $
              $(update [p| ((),v ) |] [p| (_, v) |] [d| v = Replace |])
        , $(normalSV [p| _ |] [p| _ |] ) $
            $(rearrS [| \((x:xs), e) -> (x, (xs,e))  |])
              ((mapBX `Compose` Replace) `Prod` lensFoldrMapFusion mapBX foldBx)
              `Compose`
              foldBx
        ]

lensFoldr1MapFusion :: BiGUL a c -> BiGUL (c, c) c -> BiGUL [a] c
lensFoldr1MapFusion mapBX foldBX =
  Case  [ $(normalS [| \s -> length s == 1 |] ) $
            $(update [p| v |] [p| [v] |] [d| v = mapBX `Compose` Replace |])
        , $(normalSV [p| _ |] [p| _ |] ) $
            $(rearrS [| \(x:xs) -> (x, xs)  |])
              ((mapBX `Compose` Replace) `Prod` lensFoldr1MapFusion mapBX foldBX)
              `Compose`
              foldBX
        ]

--                       map ->  change the last element -> foldr  -> fused foldr
lensFoldr1CMapFusion :: BiGUL a c -> BiGUL c b -> BiGUL (c, b) b -> BiGUL [a] b
lensFoldr1CMapFusion mapBX f foldBX =
  Case  [ $(normalS [| \s -> length s == 1 |] ) $
            $(update [p| v |] [p| [v] |] [d| v = mapBX `Compose` f `Compose` Replace |])
        , $(normalSV [p| _ |] [p| _ |] ) $
            $(rearrS [| \(x:xs) -> (x, xs)  |])
              ((mapBX `Compose` Replace) `Prod` lensFoldr1CMapFusion mapBX f foldBX)
              `Compose`
              foldBX
        ]


lensMapMapFusion :: BiGUL a b -> BiGUL b c -> BiGUL [a] [c]
lensMapMapFusion bx1 bx2 = $(rearrS [| \s -> (s, []) |])
  (lensFoldr ($(rearrV [| \(v:vs) -> (v,vs) |]) $ (bx1 `Compose` bx2) `Prod` Replace))


-- can bigul handle infinite data?
-- repeat :: a -> [a]
-- cycle :: [a] -> [a]




-- generate default value for some common datatypes
-- not that useful. cannot correctly handle types such as Either a b
class Defaultable a where
  defaultVal :: a

instance Defaultable Int where
  defaultVal = 0

instance Defaultable Integer where
  defaultVal = 0

instance Defaultable String where
  defaultVal = ""

instance Defaultable Bool where
  defaultVal = False

instance Defaultable Char where
  defaultVal = '\0'

instance Defaultable Float where
  defaultVal = 0

instance Defaultable (Either a b) where
  defaultVal = Left undefined


data Nat = Zero | Succ Nat deriving (Show, Eq)

deriveBiGULGeneric ''Nat



-- backups

-- works only when the length of the view equals to the length of the source
--lensMap :: BiGUL a b -> BiGUL [a] [b]
--lensMap b =
--  Case  [ $(normalSV [p| _:_ |] [p| _:_ |] ) $
--            $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = lensMap b |])
--        , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\s [] -> [])
--        , $(normalSV [p| [] |] [p| _:_ |] ) (Fail "length of the view should be less than that of the source")
--        , $(normalSV [p| [] |] [p| [] |] ) $
--             $(update [p| []  |] [p| [] |] [d|  |])
--        ]

-- works on arbitrary sources and views of any length.
--lensMapTest :: (Typeable a, Defaultable a) => BiGUL a b -> BiGUL [a] [b]
--lensMapTest b =
--  Case  [ $(normalSV [p| _:_ |] [p| _:_ |] ) $
--            $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = lensMap b |])
--        , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\s [] -> [])
--        , $(adaptiveSV [p| [] |] [p| _:_ |] ) (\s v -> [fromRight (put b defaultVal (head v))] ) -- put the extra elements in view a default value as source...
--        , $(normalSV [p| [] |] [p| [] |] ) $
--             $(update [p| []  |] [p| [] |] [d|  |])
--        ]

