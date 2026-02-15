import ListStage1
import ListStage2

import GHC.Generics
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter

import Language.Haskell.TH as TH hiding (Name)
import Data.Typeable

-------------test cases
tg1 = get (lensMap lensMaximum `Compose` lensHead) ([[1,2,3], [4,5,6]])
-- 3
tp1 = put (lensMap lensMaximum `Compose` lensHead) ([[1,10,3], [19]]) 7
-- [[1,7,3],[19]]
-- get lensMap lensMaximum .. .. = [10,19]. put lensHead [10, 19] 7 = [7, 19]. put lensMaximum .. [7,19] = [[1,7,3],[19]]


tg2 = get (lensHead `Compose` lensMaximum) ([[1,10,3], [4,5,6]])
tp2 = put (lensHead `Compose` lensMaximum) ([[1,10,3], [4,5,6]]) 7

tg3 :: [Int]
tg3 = fromRight $ get (lensDrop 2 `Compose` lensDrop 3) [1,2,3,10,20,30]

tp3 :: [Int]
tp3 = fromRight $ put (lensDrop 2 `Compose` lensDrop 10) [1,2,3,10,20,30] [100,200,300]
-- [1,2,0,0,0,0,0,0,3,10,20,30,100,200,300]
-- must think in this way: put1 s (put2 (get1 s) v)

tg4 :: [Int]
tg4 = fromRight $ get (lensDrop 12) [1,2,3,10,20,30]

tp4 :: [Int]
tp4 = fromRight $ put (lensDrop 12) [1,2,3,10,20,30] [100,200,300]
-- [0,0,0,0,0,0,1,2,3,10,20,30,100,200,300]

tg5 = get (lensDropWhile (< 5) `Compose` lensTakeWhile (< 10)) [1..12]
tp5 = put (lensDropWhile (< 5) `Compose` lensTakeWhile (< 10)) [1..12] [6,6,6]
-- [1,2,3,4,6,6,6,10,11,12]

tg6 = get (lensTailNonStrict `Compose` lensTailNonStrict) ([1..10] :: [Int])
tp6 = put (lensTailNonStrict `Compose` lensTailNonStrict) ([1..10] :: [Int]) ([3,4,5,6,7,8,9,10] :: [Int])
--  [1,2,3,4,5,6,7,8,9,10]

tp7 = put (lensTailNonStrict `Compose` lensTailNonStrict) ([1..10] :: [Int]) ([3,4,5,7,8,9,10] :: [Int])
--  [2,3,3,4,5,7,8,9,10]
-- put2 [2..10] [3,4,5,7,8,9,10] = [3,3,4,5,7,8,9,10]
-- put1 [1..10] [3,3,4,5,7,8,9,10] = [2,3,3,4,5,7,8,9,10]

t8g = get (lensFoldr plusr) ([1,2,3,4], 0)
t8p = put (lensFoldr plusr) ([1,2,3,4], 0) 16

t9g = get (lensFoldr plusl) ([1,2,3,4], 0)
t9p = put (lensFoldr plusl) ([1,2,3,4], 0) 16


----------------------
t10g = get ((lensMap uleft) `Compose` lensMaximum) [Left 2, Left 7, Left 5]
t10p = put ((lensMap uleft) `Compose` lensMaximum) [Left 2, Left 7, Left 5] 999

t11g = get (lensFoldr1MapFusion uleft lensMaxInner) [Left 2, Left 7, Left 5]
t11p = put (lensFoldr1MapFusion uleft lensMaxInner) [Left 2, Left 7, Left 5] 999


-- map, map, foldr1,
t12g = get (lensMap (lensMap uleft) `Compose` lensMap lensMaximum `Compose` lensFoldr1 lensHead_test)
           ([[Left 1, Left 3, Left 2], [Left 6, Left 4, Left 5]])
t12p = put (lensMap (lensMap uleft) `Compose` lensMap lensMaximum `Compose` lensFoldr1 lensHead_test)
           ([[Left 1, Left 3, Left 2], [Left 6, Left 4, Left 5]])  999

-- map map fusion
t13g = get ( lensMap (lensMap uleft `Compose` lensMaximum) `Compose` lensFoldr1 lensHead_test)
           ([[Left 1, Left 3, Left 2], [Left 6, Left 4, Left 5]])
t13p = put ( lensMap (lensMap uleft `Compose` lensMaximum) `Compose` lensFoldr1 lensHead_test)
           ([[Left 1, Left 3, Left 2], [Left 6, Left 4, Left 5]]) 999

-- map fold fusion
t14g = get ( lensFoldr1MapFusion (lensMap uleft `Compose` lensMaximum) lensHead_test)
           ([[Left 1, Left 3, Left 2], [Left 6, Left 4, Left 5]])
t14p = put ( lensFoldr1MapFusion (lensMap uleft `Compose` lensMaximum) lensHead_test)
           ([[Left 1, Left 3, Left 2], [Left 6, Left 4, Left 5]]) 999

lensHead_test = ($(update [p| v |] [p| (v,_) |] [d| v = Replace |]))

uleft :: BiGUL (Either a b) a
uleft = $(update [p| x |] [p| Left x |] [d| x = Replace |])

plusl :: BiGUL (Int,Int) Int
plusl = emb (\(x,y) -> x + y) (\(x,y) v -> (v-y ,y))

plusr :: BiGUL (Int,Int) Int
plusr = $(rearrS [| \(x,y) -> (y,x) |]) plusl
----------------



---------------MSS----------------
--segs = concat . map inits . tails

--test1 = lensTails `Compose` lensMap lensInits `Compose` lensMap (lensMap plusl)  `Compose` lensMap lensMaximum `Compose` lensMaximum
msstest1 = lensTails `Compose` lensMap (lensInits `Compose` lensMap plusl'' `Compose` lensMaximum) `Compose` lensMaximum
msstest2 = lensTails `Compose` lensMap lensInits `Compose` lensMap (lensMap plusl'')
-- [1,3,-3,4]
--maximum  . map maximum  . map (map sum) . map inits . tails [1,2,3,4]

msstest3 = lensTails `Compose` lensMap (lensInits `Compose` lensMap plusr'' `Compose` lensMaximum) `Compose` lensMaximum

plusl'' :: BiGUL [Int] Int
plusl'' = emb (\s -> sum s) (\s v -> v - sum (tail s) : tail s )

plusr'' :: BiGUL [Int] Int
plusr'' = emb (\s -> sum s) (\s v -> init s  ++  [(v - sum (init s))] )


----------log-----------
-- get (lensTails `Compose` lensMap lensInits `Compose` lensMap (lensMap plusl)  `Compose` lensMap lensMaximum `Compose` lensMaximum)  [1,2,-3,4]
-- Right 4

--get (lensTails `Compose` lensMap lensInits `Compose` lensMap (lensMap plusl)  `Compose` lensMap lensMaximum)  [1,2,-3,4]
--Right [4,3,1,4,0]
--put lensMaximum  [4,3,1,4,0] 5
--Right [5,3,1,4,0]

--get (lensTails `Compose` lensMap lensInits `Compose` lensMap (lensMap plusl))  [1,2,-3,4]
--Right [[0,1,3,0,4],[0,2,-1,3],[0,-3,1],[0,4],[0]]
--put (lensMap lensMaximum) [[0,1,3,0,4],[0,2,-1,3],[0,-3,1],[0,4],[0]] [5,3,1,4,0]
--Right [[0,1,3,0,5],[0,2,-1,3],[0,-3,1],[0,4],[0]]

--get (lensTails `Compose` lensMap lensInits) [1,2,-3,4]
--Right [[[],[1],[1,2],[1,2,-3],[1,2,-3,4]],[[],[2],[2,-3],[2,-3,4]],[[],[-3],[-3,4]],[[],[4]],[[]]]
--put (lensMap (lensMap plusl)) [[[],[1],[1,2],[1,2,-3],[1,2,-3,4]],   [[],[2],[2,-3],[2,-3,4]],[[],[-3],[-3,4]],[[],[4]],[[]]] [[0,1,3,0,5],   [0,2,-1,3],[0,-3,1],[0,4],[0]]
--Right [[[],[1],[1,2],[1,2,-3],[2,2,-3,4]],   [[],[2],[2,-3],[2,-3,4]],[[],[-3],[-3,4]],[[],[4]],[[]]]

-- note that [[],[1],[1,2],[1,2,-3],   [1,2,-3,4]] becomes [[[],[1],[1,2],[1,2,-3],  [2,2,-3,4]], which is not a inits !
-- because plusl changes the first element !
-- guess: plusr should works fine ? (turns out to be not)

--get lensTails [1,2,-3,4]
--Right [[1,2,-3,4],[2,-3,4],[-3,4],[4],[]]
--put (lensMap lensInits) [[1,2,-3,4],[2,-3,4],[-3,4],[4],[]] [[[],[1],[1,2],[1,2,-3],[2,2,-3,4]],[[],[2],[2,-3],[2,-3,4]],[[],[-3],[-3,4]],[[],[4]],[[]]]
--Left incompatible updates

--[ [1,2,-3,4],                        [2,-3,4],                [-3,4],          [4],      []]
--[ [[],[1],[1,2],[1,2,-3],[2,2,-3,4]],[[],[2],[2,-3],[2,-3,4]],[[],[-3],[-3,4]],[[],[4]],[[]]]
-- view is not inits!


------------------log 2 with plusr -------------
--get (lensTails `Compose` lensMap lensInits `Compose` lensMap (lensMap plusl)  `Compose` lensMap lensMaximum)  [1,2,-3,4]
--Right [4,3,1,4,0]
--put lensMaximum  [4,3,1,4,0] 5
--Right [5,3,1,4,0]

--get (lensTails `Compose` lensMap lensInits `Compose` lensMap (lensMap plusl))  [1,2,-3,4]
--Right [[0,1,3,0,4],[0,2,-1,3],[0,-3,1],[0,4],[0]]
--put (lensMap lensMaximum) [[0,1,3,0,4],[0,2,-1,3],[0,-3,1],[0,4],[0]]   [5,3,1,4,0]
--Right [[0,1,3,0,5],[0,2,-1,3],[0,-3,1],[0,4],[0]]

--get (lensTails `Compose` lensMap lensInits) [1,2,-3,4]
--Right [[[],[1],[1,2],[1,2,-3],[1,2,-3,4]], [[],[2],[2,-3],[2,-3,4]],  [[],[-3],[-3,4]],[[],[4]],[[]]]
--put (lensMap (lensMap plusr)) [ [[],[1],[1,2],[1,2,-3],[1,2,-3,4]],   [[],[2],[2,-3],[2,-3,4]], [[],[-3],[-3,4]], [[],[4]], [[]] ]
--                              [ [0,1,3,0,5],                          [0,2,-1,3],               [0,-3,1],         [0,4],    [0] ]
--Right [[[],[1],[1,2],[1,2,-3],[1,2,-3, 5 ]],[[],[2],[2,-3],[2,-3,  4 ]],  [[],[-3],[-3, 4 ]],[[],[ 4 ]],[[]]] -- 4 is not changed to 5


--get lensTails [1,2,-3,4]
--Right [[1,2,-3,4],[2,-3,4],[-3,4],[4],[]]
--put (lensMap lensInits) [[1,2,-3,4],[2,-3,4],[-3,4],[4],[]]    [[[],[1],[1,2],[1,2,-3],[1,2,-3,5]],[[],[2],[2,-3],[2,-3,4]],[[],[-3],[-3,4]],[[],[4]],[[]]]
--Right [[1,2,-3,5],  [2,-3,4],[-3,4],[4],[]]
-- note that the view is still not tails!


--put lensTails [1,2,-3,4] [[1,2,-3,5],  [2,-3,4],[-3,4],[4],[]]
--Left incompatible updates





------------------log 3 with plusr -------------
--get (lensTails `Compose` lensMap lensInits `Compose` lensMap (lensMap plusl)  `Compose` lensMap lensMaximum)  [1,-1]
--Right [1,0,0]
--put lensMaximum  [1,0,0] 5
--Right [5,0,0]

--get (lensTails `Compose` lensMap lensInits `Compose` lensMap (lensMap plusl))  [1,-1]
--Right [[0,1,0],[0,-1],[0]]
--put (lensMap lensMaximum) [[0,1,0],[0,-1],[0]]   [5,0,0]
--Right [[0,5,0],[0,-1],[0]]

--get (lensTails `Compose` lensMap lensInits) [1,-1]
--Right [[[],[1],[1,-1]],[[],[-1]],[[]]]
--put (lensMap (lensMap plusr)) [[ [], [1], [1,-1] ], [[],[-1]], [[]]]   [[0, 5, 0],[0,-1],[0]]
--Right [[[],[5],[1,-1]],[[],[-1]],[[]]]


--get lensTails [1,-1]
--Right [[1,-1],[-1],[]]
--put (lensMap lensInits) [[1,-1],[-1],[]]    [ [[],[ 5 ],[1,-1]],  [[],[-1]], [[]]]
-- Left incompatible updates
-- note that the view is not tails!

-------------end--MSS----------------



--align :: (Eq a, Eq b)
--      => (a -> Bool)
--      -> (a -> b -> Bool)
--      -> BiGUL a b
--      -> (b -> a)
--      -> (a -> Maybe a)
--      -> BiGUL [a] [b]
--align p match b create conceal =
--  Case [ $(normalSV [| null . filter p |] [p| [] |]) $
--           $(rearrV [| \ [] -> () |]) Skip
--       , $(adaptiveSV [| not . null . filter p |] [p| [] |]) $ -- conceal == delete
--           \ss _ -> catMaybes (map (\s -> if p s then conceal s else Just s) ss)

--         -- preserve the items not satisfying predicate p in the source
--       , $(normalSV [| \ss -> not (null (filter p ss)) && not (p (head ss)) |] [p| _:_ |]) $
--           $(update [p| vs |] [p| _:vs |] [d| vs = align p match b create conceal |])
--       , $(normal' [| \ss vs -> not (null (filter p ss)) && p (head ss) && match (head ss) (head vs) |]
--                   [| \ss -> not (null (filter p ss)) && p (head ss) |]) $
--           $(update [p| v : vs |] [p| v : vs |] [d| v  = b; vs = align p match b create conceal |])
--       , $(adaptiveV [p| _:_ |]) $
--           \ss (v:_) -> case find (flip match v) (filter p ss) of
--                          Nothing -> create v:ss
--                          Just s  -> s: delete s ss ]

--align1 :: BiGUL [Int] [Int]
--align1 = align (\_ -> True)
--               (\s v -> s == v)
--               Replace {- case matched -}
--               (id) {- case unmatchS -}
--               (const Nothing) {- case unmatchV -}
--  where
--    salary (n,s) = s
--    name (n,s) = n

--extract :: Ord a => BiGUL (a, [a]) [a]
--extract = Case
--  [ $(normalV [p| [] |])$
--      Fail "view cannot be empty"
--  , $(normal [| \(s, ss) vs -> not (s `elem` vs) |])$
--      Fail "view doesn't include special node"
--  , $(adaptive [| \(s, ss) (v:vs) -> null ss && v /= s |])$
--      \(s, ss) (v:vs) -> (s, [v])
--  , $(normal [| \(s, ss) (v:vs) -> not (null ss) && v < s |])$
--      $(rearrS [| \(s, (s':ss)) -> (s', (s, ss)) |])$
--        $(rearrV [| \(v:vs) -> (v, vs) |])$
--          Replace `Prod` extract
--  , $(normal [| \(s, ss) (v:vs) -> v == s |])$
--      $(rearrV [| \(v:vs) -> (v, vs) |])$
--        Replace
--  ]


--unsort :: Ord a => BiGUL [a] [a]
--unsort = Case
--  [ $(normalSV [p| [] |] [p| [] |])$
--      $(update [p| [] |] [p| _ |] [d| |])
--  , $(adaptiveSV [p| _:_ |] [p| [] |])$
--      \_ _ -> []
--  , $(adaptiveSV [p| [] |] [p| _:_ |])$
--      \_ vs -> vs
--  , $(normal [| \(s:ss) vs -> elem s vs |])$
--      $(rearrS [| \(s:ss) -> (s, ss) |])$
--        (Replace `Prod` unsort) `Compose` extract
--  , $(adaptive [| \(s:ss) vs -> not (elem s vs) |])$
--      \ss vs -> let ss' = dropWhile (not . flip elem vs) ss
--                in if null ss'
--                   then vs
--                   else ss'
--  ]
