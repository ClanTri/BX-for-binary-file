-- | The module contains many examples, from easy to difficult, showing how to program in BiGUL.

module Generics.BiGUL.Lib.HuStudies where

import Generics.BiGUL
import Generics.BiGUL.Interpreter
import Generics.BiGUL.TH

-- |
-- > alwaysFail = Fail "always fail"
-- the combinator 'Fail' will always fail all the transformation, reporting the given error message.
--
-- >>> get alwaysFail "Please succeed"
-- Left fail: always fail
-- >>> put alwaysFail 23 False
-- Left fail: always fail
alwaysFail :: BiGUL a b
alwaysFail = Fail "always fail"


-- |
-- > constSquare = Skip (\s -> s * s)
-- (Skip f) means that in the get direction, the view is fully computed by apply function f to the source.
-- So in the put direction, the update is skipped and the source is unchanged.
-- in the put direction, if (f source) /= view, an error is raised.
-- the view is the square of the source.
--
-- >>> put constSquare 10 100
-- Right 10
--
-- >>> put constSquare 10 225
-- Left *** Exception blabla...
--
-- >>> get constSquare 5
-- Right 25
constSquare :: BiGUL Int Int
constSquare = Skip (\s -> s * s)


-- |
-- > repFirst = Replace `Prod` Skip (const ())
-- the example shows a simplest case to chain basic constructors of BiGUL together.
-- 'Prod' is right associative with priority 1 (0 is the lowest, e.g. $ is 0)
-- To use 'Prod', the source and view are assumed to be tuple.
-- the first elements of both tuples are associated by 'Replace', the second by 'Skip'.
--
-- >>> put repFirst (False, 9) (True, ())
-- Right (True,9)
--
-- >>> get repFirst (True, 3)
-- Right (True,())
repFirst :: (Show a, Show b) => BiGUL (a, b) (a, ())
repFirst = Replace `Prod` Skip (const ())


-- |
-- > repFirst' = $(update [p| (x,_) |] [p| (x,()) |] [d| x = Replace |])
-- This is the 'repFirst' example rewritten with syntactic sugar.
-- The syntax is
--
-- > $(update [p| source-pattern |] [p| view-pattern |] [d| updating-strategy |])
-- Source and view are decomposed by the patterns in the [p| ... |].
-- In this concrete example, the first elements of the tuple (both in the source and in the viwe) are bound to variable x,
-- and they are sent to the combinator 'Replace' as arguments by (/x=Replace/) in the [d| ... |] part.
-- anything we want to perform 'Skip' should be marked as underline(_) in the [p| source-pattern |] and it should not appear in the [d| ... |] part.
--
-- The source-pattern and view-pattern can be very different, for example:
--
-- > $([p| Left x : _  |] [p| ((), x) |] [d| x = blabla... |])
-- where the source-pattern stands for a non-empty list of Either type, and we bind variable x to the inner part of the Left constructor.
-- However the view is a tuple, and we bind the second element to x.
-- The rearrangement for source and view is automatically done.
repFirst' :: Show a => BiGUL (a, b) (a, ())
repFirst' = $(update [p| (x,_) |] [p| (x,()) |] [d| x = Replace |])



-- |
-- > repFirstV2 = RearrV PVar' (EDir DVar `EProd` EConst ()) repFirst
-- Skip to 'repFirstV2'' if you want to use the syntactic sugar only.
--
-- In the previous example, suppose what we really want is that, the view has type a rather than (a,()).
-- In order to do so, we can use 'RearrV', which first rearranges the view into the desired structure,
-- and then uses another BiGUL program to perform the transformation.
--
-- The usage of 'RearrV' is:
--
-- > RearrV (old-pattern) (new-pattern) (bigul-program)
--
-- 'PVar' means there is a variable (a hole to be update), and 'PVar'' is the same as 'PVar' except that it does not require the 'Eq' constraint.
-- In this concrete example, since the view is a single variable, the /old-pattern/ is 'PVar''.
-- 'DVar' still means there is a variable. But DVar should be used in the second argument of the 'RearrV'.
-- Since there may be many variables in the old-pattern, we need to mark the origin of each variable in the new-pattern: where does it come from.
-- In this concrete example, because there is only one variable in the old-pattern, a 'DVar' is enough to locate it.
-- Then we use 'EDir' to make 'DVar' becomes a direction, 'EProd' it with a constant 'EConst' ().
-- A more complex situation about the new-pattern is in the 'repFirstV3' example.

--
-- >>> put repFirstV2 (undefined , Nothing) 3
-- Right (3,Nothing)
--
-- >>> get repFirstV2 (True, undefined)
-- Right True
repFirstV2 :: (Show a, Show b) => BiGUL (a,b) a
repFirstV2 = RearrV PVar' (EDir DVar `EProd` EConst ()) repFirst



-- |
-- > repFirstV2' = $(rearrV [| \v -> (v,()) |]) repFirst
-- It is difficult to use primitive combinators, especially when the problem becomes complex.
-- Here we introduce another syntactic sugar:
--
-- > $(rearrV [| \old-pattern -> new-pattern |]) bigul-program
-- When rearranging the view, it's possible to duplicate information:
--
-- > $(rearrV [| \v -> (v,v)|]) bigul-program
-- but it's __/not allowed/__ to drop information:
--
-- > $(rearrV [| \(vl,vr) -> vl |]) bigul-program           -- this is WRONG
repFirstV2' :: (Show a, Show b) => BiGUL (a,b) a
repFirstV2' = $(rearrV [| \v -> (v,()) |]) repFirst



-- |
-- > repFirstV3 = RearrS (PVar' `PProd` PVar') (EDir (DLeft DVar)) Replace
-- The function produces exactly the same result as 'repFirstV2'.
-- The difference is that, instead of using 'RearrV' to rearrange the view into a tuple,
-- here we use 'RearrS' to rearrange the source into a single value of type a, and then simply use 'Replace' to update the source.
--
-- The usage of 'RearrS' is:
--
-- > RearrS (old-pattern) (new-pattern) (bigul-program)
-- The original source is a tuple, that is to say, there are two variables. So the old-pattern is (PVar' \`Prod\` PVar').
-- In the new-pattern, We need to tell BiGUL that, it is the first element of the tuple, i.e. the left element, we want to update.
-- This is achieved by (DLeft DVar). Finally, we convert it into a direction using 'EDir'.
repFirstV3 :: Show a => BiGUL (a,b) a
repFirstV3 = RearrS (PVar' `PProd` PVar') (EDir (DLeft DVar)) Replace


-- |
-- > repFirstV3' = $(rearrS [| \(l, _) -> l |]) Replace
-- The syntactic sugar version for 'repFirstV3'.
-- The usage of the syntactic sugar is basically the same as $(rearrV ...):
--
-- > $(rearrS [| \old-pattern -> new-pattern |]) bigul-program
repFirstV3' :: Show a => BiGUL (a,b) a
repFirstV3' = $(rearrS [| \(l, _) -> l |]) Replace


-- |
-- > repFirstV4 = Dep (const ()) ($(rearrS [| \(l, _) -> l |]) Replace)
-- Yet another version of 'repFirst'. This artificial example briefly introduces the constructor 'Dep', which is rather seldom used.
-- 'Dep' can be used to add or eliminate information on the view.
-- In this concrete example, since the second element of the view is a unit (()), it can be produced from any existing view v by
-- (const ()), which is equivalent to (\v -> const () v).
-- Now we can consider a bidirectional transformation f
--
-- > ($(rearrS [| \(l, _) -> l |]) Replace)
-- which is between source (a,b) and view a only.
-- Then both the transformation f and the function (const ()) are passed to 'Dep' to finally produce the transformation between (a, b) and (a, ())
repFirstV4 :: (Show a, Show b) => BiGUL (a, b) (a, ())
repFirstV4 = Dep (const ()) ($(rearrS [| \(l, _) -> l |]) Replace)



-- |
-- >  Case  [ $(normal [| \(a,b) c -> a <= b && c <= b |]
-- >                   [|\(a,b) -> a <= b |])
-- >            $(update [p| (a,_) |] [p| a |] [d| a = Replace  |])
-- >        , $(normal [| \(a,b) c -> b < a  && c < a  |]
-- >                   [|\(a,b) -> b < a |])
-- >            $(update [p| (_,b) |] [p| b |] [d| b = Replace  |])
-- >        , $(normal [|\ _ _ -> True|]
-- >                   [|\(a,b) -> False |])
-- >            (Fail "the source view are not consistent.")
-- >        ]
-- Here we introduce the 'Case' combinator, which is extremely useful. 'Case' resembles the conditional branch in most languages.
-- In this concrete example, the 'Case' combinator enables us to do the following: Suppose the source is (a,b) and the view is c,
-- if (a <= b && c <= b), then we replace a with c; if (b < a && c < a), then we replace b with c; otherwise, the program fails.
-- Because in this case once the minimum element in the source is replaced, it is no longer the minimum element.
--
-- The general structure for 'Case' is:
--
-- > Case [ $(normal   [| enteringCond1  :: s -> v -> Bool |] [|exitCond1 :: s -> Bool |]) $
-- >          (bx1 :: BiGUL s v)
-- >      , $(adaptive [| enteringCond1' :: s -> v -> Bool |]) $
-- >          (f1 :: s -> v -> s)
-- >      , ...
-- >      , $(normal   [| enteringCondn  :: s -> v -> Bool |] [|exitCond1 :: s -> Bool |]) $
-- >         (bxn :: BiGUL s v)
-- >      , ...
-- >      , $(adaptive [| enteringCondm' :: s -> v -> Bool |]) $
-- >         (fm :: s -> v -> s)
-- >      ]
-- >     :: BiGUL s v
--
-- It contains a sequence of cases. For each case, it is either normal or adaptive. For
-- the normal case, if the condition is satisfied, a corresponding putback transformation
-- is applied. For the adaptive case, if the condition is satisfied, a function
-- is used to update the source with the view so that for the next step one of the
-- normal cases can be applied. Note that if adaptation does not lead the source
-- and the view to a normal case, an error will be reported at runtime.
-- The example for /adaptive/ branch is in the next example.
--
-- Note that $(normal ... ...) takes two predicates. The first one is the entering-condition while the second one is the exit-condition.
-- The predicate for entering-condition is very general, and we can use any function f of type (s -> v -> Bool) to examine the source and view.
-- If the condition is matched, then the BiGUL program after the predicate is executed. If the condition is not satisfied, the next branch is tried.
-- The predicate for exit-condition checks the source only. The exit-condition in different branches should be always NOT overlapped.
-- Eg: (a <= b), (b < a), (False) are not overlapped.
--
-- Note: instead of a general function, we can use patterns for predicate. The syntax is:
--
-- > $(normalSV [p| source-pattern |] [p| view-pattern |] [| exitCond |] )
-- > ...
-- > $(adaptiveSV [p| source-pattern |] [p| view-pattern |])
-- For example:
--
-- > $(normalSV [p| Left _:_ |] [p| [] |]
-- >            [| exitCond |] )
-- states that the source is a non-empty list with the first element in a /Left/ constructor,
-- and the view is an empty list. This feature is heavily used in the 'naiveMap' example.
--
-- Please avoid using variables in the pattern-predicate: always use an underline.
--
-- >>> put replaceMin (2,7) 4
-- Right (4,7)
--
-- >>> put replaceMin (2,7) 10
-- Left fail: the source view are not consistent.
--
-- >>> get replaceMin (2,7)
-- Right 2
replaceMin :: BiGUL (Int, Int) Int
replaceMin =
  Case  [ $(normal [| \(a,b) c -> a <= b && c <= b |]
                   [|\(a,b) -> a <= b |])
            $(update [p| (a,_) |] [p| a |] [d| a = Replace  |])
        , $(normal [| \(a,b) c -> b < a  && c < a  |]
                   [|\(a,b) -> b < a |])
            $(update [p| (_,b) |] [p| b |] [d| b = Replace  |])
        , $(normal [|\ _ _ -> True|]
                   [|\(a,b) -> False |])
            (Fail "the source view are not consistent.")
        ]



-- |
-- > lensLength def =
-- >   Case [ $(adaptive [| \s v -> length s /= v |])
-- >            (\s v -> let ls = length s
-- >                     in  if ls > v then drop (ls - v) s
-- >                                   else replicate (v - ls) def ++ s)
-- >        , $(normal [|\s v -> length s == v |]
-- >                   [| const True |])
-- >            (Skip length)
-- >        ]
-- In this example, the source is any list and the view is the length of the source.
-- Note that The function is not a lens: we should provide the function with a default value to make it a lens.
-- The default value is used to generate new elements and thus expand the source, when the view is greater than the length of the source.
-- If the view is less than the length of the source, the source will be shortened.
--
-- Here we introduce the adaptive branch of 'Case', which takes a predicate (just like 'Normal' branch),
-- and a function (f :: s -> v -> s) that is used to create a new source.
-- Adaptive branch can be placed anywhere in a 'Case'.
-- Once the adaptive branch is executed and the new source is created, the whole 'Case' will be re-executed from the first branch.
-- If again an adaptive branch is matched, an error is thrown.
--
-- Another point is that, adaptive branch is chosen in the put direction only. In the get direction, it will never be chosen.
--
-- >>> put (lensLength 10) [2,2,1] 2
-- Right [2,1]
--
-- >>> put (lensLength 10) [2,2,1] 6
-- Right [10,10,10,2,2,1]
--
-- >>> get (lensLength undefined) [1..10]
-- Right 10
lensLength :: a -> BiGUL [a] Int
lensLength def =
  Case [ $(adaptive [| \s v -> length s /= v |])
           (\s v -> let ls = length s
                    in  if ls > v then drop (ls - v) s
                                  else replicate (v - ls) def ++ s)
       , $(normal [|\s v -> length s == v |]
                  [| const True |])
           (Skip length)
       ]


-- |
-- > lensLength' def = emb length p
-- >   where p = \s v -> let ls = length s
-- >                     in  if ls > v then drop (ls - v) s
-- >                                   else replicate (v - ls) def ++ s
-- In fact what lensLength expresses is just that: We have two functions g and p,
-- g is used to do the get (by a 'Normal' branch), while p is used to do the put (by an 'Adaptive' branch).
-- this intention can be expressed in a more simple and modular way: using the 'emb' (embed) function.
-- the definition of 'emb' can be found in the next example.
lensLength' :: a -> BiGUL [a] Int
lensLength' def = emb length p
  where p = \s v -> let ls = length s
                    in  if ls > v then drop (ls - v) s
                                  else replicate (v - ls) def ++ s


-- |
-- > (==>) = ($)
-- make it more elegant to write ($). Later we may use (==>) instead of ($).
(==>) :: (a -> b) -> a -> b
(==>) = ($)

-- |
-- > emb g p = Case
-- >   [ $(normal [| \s v -> g s == v |] [p| _ |])
-- >     ==> Skip g
-- >   , $(adaptive [| \s v -> True |])
-- >     ==> p
-- >   ]
-- emb g p: invoke g to do the get, and invoke p to do the put.
emb :: Eq v => (s -> v) -> (s -> v -> s) -> BiGUL s v
emb g p = Case
  [ $(normal [| \s v -> g s == v |] [p| _ |])
    ==> Skip g
  , $(adaptive [| \s v -> True |])
    ==> p
  ]


-- |
-- > lensSucc = emb (flip (+) 1) (\_ v -> v - 1)
-- Sometimes 'emb' is useful. For instance, Int is a primitive datatype without any constructor in Haskell,
-- and cannot be manipulated in a way like list in 'ReaarV' or 'RearrS'.
-- For list, we can write (x:xs) -> (x:x:xs) using its constructor. But we cannot decompose Int.
-- Making use of 'emb', we can manipulate basic operations for Int, whose well-behavedness should be proved by hand.
-- (But here, the well-behavedness is easily seen.)
--
-- >>> put lensSucc 0 10
-- Right 9
--
-- >>> get lensSucc 8
-- Right 9
lensSucc :: BiGUL Int Int
lensSucc = emb (flip (+) 1) (\_ v -> v - 1)

-- |
-- > naiveMap b =
-- >   Case  [ $(normalSV [p| _:_ |] [p| _:_ |]
-- >                      [p| _:_ |])
-- >           ==> $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = naiveMap b |])
-- >         , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\_ _ -> [])
-- >         , $(normalSV [p| [] |] [p| _:_ |]
-- >                      [| const False |])
-- >           ==> (Fail "length of the view should be less than that of the source.")
-- >         , $(normalSV [p| [] |] [p| [] |]
-- >                      [p| [] |])
-- >           ==> $(update [p| [] |] [p| [] |] [d| |])
-- >         ]
-- A naive map function, which takes a BiGUL program and yields another BiGUL program working on list.
-- The first branch deals with recursive condition.
-- The second branch handles the boundary conditions where the source list is longer than the view list:
-- drop all the remaining elements in the source list and thus make it an empty list.
-- The third branch will throw an error when the view list is longer than the source list.
-- The last branch is the termination condition: both the source and view reach the empty constructor.
--
-- (For the sake of completeness.) In fact 'normalSV' means that we use separate condition for source and view.
-- So we can still use a general function in the predicate:
--
-- > $(normalSV [| \s -> case s of _:_ -> True; _ -> False |] [p| _:_  |] [p| _:_ |])
--
-- >>> put (naiveMap lensSucc) [1,2,3,4] [7,8,9]
-- Right [6,7,8]
--
-- >>> get (naiveMap (lensLength undefined)) ["123", "xyz"]
-- Right [3,3]
--
-- >>>get (naiveMap replaceMin) [(3,9), (-2,10),(10,2)]
-- Right [3,-2,2]

naiveMap :: (Show a, Show b) => BiGUL a b -> BiGUL [a] [b]
naiveMap b =
  Case  [ $(normalSV [p| _:_ |] [p| _:_ |]
                     [p| _:_ |])
          ==> $(update [p| x:xs |] [p| x:xs |] [d| x = b; xs = naiveMap b |])
        , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\_ _ -> [])
        , $(normalSV [p| [] |] [p| _:_ |]
                     [| const False |])
          ==> (Fail "length of the view should be less than that of the source.")
        , $(normalSV [p| [] |] [p| [] |]
                     [p| [] |])
          ==> $(update [p| [] |] [p| [] |] [d| |])
        ]

-- |
-- > compose = Compose
-- The last combinator we are going to introduce is 'Compose',
-- which takes two BiGUL programs and behaves like \"function composition\".
--
-- Given two BiGUL programs,
--
-- > f :: BiGUL a b, g :: BiGUL b c
-- we have
--
-- > f `Compose` g :: BiGUL a c
-- In the get direction, the semantics of @get (f \`Compose\` g) s@ is:
-- (suppose the function 'get' and 'put' always return a value rather than a value wrapped in 'Right'.)
--
-- > get g (get f s)
-- In the put direction, the semantics of @put (f \`Compose\` g) s v@ is a little bit complex:
--
-- > put f s (put g (get f s) v)
-- Let us make it more clear:
--
-- > let a = get f s
-- >     b = put g a v
-- > in  put f s b
-- Check the type of these transformations by yourself will help you understand deeper.
--
-- Let us try some examples:
--
-- >>> put ((naiveMap replaceMin) `compose` (naiveMap lensSucc)) [(1,-1),(-2,2)] [-8, 1]
-- Right [(1,-9),(0,2)]
--
-- >>> get ((naiveMap replaceMin) `compose` (naiveMap lensSucc)) [(1,-1),(-2,2)]
-- Right [0,-1]
compose :: (Show a, Show b, Show c) => BiGUL a b -> BiGUL b c -> BiGUL a c
compose = Compose

-- |
-- The last example in this tutorial is a simple map-map fusion.
-- It makes the composition of two map functions run more efficiently, compared to using 'Compose' combinator.
--
-- In the get direction, (get (f \`Compose\` g)) traverse the list twice, while (get (mapFusion f g)) traverse the list only once.
-- And in the put direction, (put f \`Compose\` g) traverse the two lists up to five times (get counts up once, two put count up four times, since a put takes two lists as argument),
-- while (put mapFusion f g) traverses the lists only twice.
--
-- Compare the following result (in GHCI)
--
-- > t1 :: Int
-- > t1 = last $ fromRight $ put (naiveMap lensSucc `Compose` naiveMap lensSucc) [1..100000] [2..20001]
-- > t2 :: Int
-- > t2 = last $ fromRight $ put (mapFusion lensSucc lensSucc) [1..100000] [2..20001]
-- > fromRight (Right x) = x
--
-- >>> t1
-- 19999
-- (1.24 secs, 512,471,456 bytes)
--
-- >>> t2
-- 19999
-- (0.23 secs, 122,920,792 bytes)
--
-- More examples can be found in the list library of BiGUL.
mapFusion :: (Show a, Show b, Show c) => BiGUL a b -> BiGUL b c -> BiGUL [a] [c]
mapFusion f g =
  Case  [ $(normalSV [p| _:_ |] [p| _:_ |]
                     [p| _:_ |])
          ==> $(update [p| x:xs |] [p| x:xs |] [d| x = f `Compose` g; xs = mapFusion f g |])
        , $(adaptiveSV [p| _:_ |] [p| [] |] ) (\_ _ -> [])
        , $(normalSV [p| [] |] [p| _:_ |]
                     [| const False |])
          ==> (Fail "length of the view should be less than that of the source")
        , $(normalSV [p| [] |] [p| [] |]
                     [p| [] |])
          ==> $(update [p| [] |] [p| [] |] [d| |])
        ]
