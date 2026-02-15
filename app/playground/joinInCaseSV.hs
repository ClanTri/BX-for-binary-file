{-# LANGUAGE TypeOperators, TypeFamilies, FlexibleContexts, DeriveGeneric, ViewPatterns  #-}

import GHC.Generics
import Generics.BiGUL
import Generics.BiGUL.AST
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH
import Data.List
import Data.Maybe
import Control.Monad.Except

type BiGUL' s v = BiGUL (Either ErrorInfo) s v



forkG :: (Eq a, Eq s) => (s -> a) -> BiGUL' [s] (s,([s], [s]))
forkG f = Case [ $(normalSV [p| [_] |] [p| (_,([], [])) |]) $
                   $(rearr [| \(ts,([], [])) -> [ts] |]) Replace
               , $(normal [| \ss (c,(xs,_)) -> not (null ss) && not (null xs) && f (head ss) == f c |]) $
                   $(rearr [| \(c , (x:xs, ys)) -> (x , (c , (xs , ys))) |])
                     $(update [p| s:ss |] [d| s = Replace; ss = forkG f |])
               , $(normal [| \ss (c,(_,ys)) -> not (null ss) && not (null ys) && f (head ss) /= f c |]) $
                   $(rearr [| \(c , (xs, y:ys)) -> (y , (c , (xs , ys))) |])
                     $(update [p| s:ss |] [d| s = Replace; ss = forkG f |])
               , $(adaptiveSV [p| _ |] [p| _ |]) $
                   \_ (c , (xs, ys)) -> xs ++ ys ++ [c]
               ]
forkG' :: (Eq a, Eq s) => (s -> a) -> BiGUL' [s] ([s], [s])
forkG' f = Case [ $(normalSV [p| [] |] [p| ([], []) |]) $
                    $(rearr [| \([], []) -> () |]) Skip
                , $(normalSV [p| _ |] [p| (_:_ , _) |]) $
                    ($(rearr [| \(x:xs,ys) -> (x,(xs,ys)) |]) (forkG f))
                ]


unGroupBX :: (Eq a,Eq s) => (s -> a) -> BiGUL' [s] [[s]]
unGroupBX f = Case [  $(normalSV [p| _ |] [p| [] |]) $
                        $(rearr [| \[] -> [] |]) Replace
                    , $(normalSV [p| _ |] [p| _ |]) $
                           ((flip Compose)
                            ($(rearrAndUpdate [p| x:xs |]
                                              [p| (x,xs) |]
                                              [d| x = Replace
                                                  xs = unGroupBX f|]))
                             (forkG' f))
                    ]

groupBX :: (Eq a,Eq s) => (s -> a) -> BiGUL' [[s]] [s]
groupBX f = Emb (put (unGroupBX f) [])
                (\_ v -> get (unGroupBX f) v)

productUnGroupBX :: (Eq a,Eq s1,Eq s2) => (s1 -> a) -> (s2 -> a) -> BiGUL' ([s1],[s2]) ([[s1]],[[s2]])
productUnGroupBX f1 f2 = $(update [p| (x,y) |]
                                  [d| x = unGroupBX f1
                                      y = unGroupBX f2|])














pairAlignHelperREmpty :: (Eq s1, Eq s2 ) => BiGUL' [([s1],[s2])] ([[s1]],[[s2]])
pairAlignHelperREmpty = Case [ $(normalV [p| ([],[]) |]) $
                                 $(rearr [| \([],[]) -> [] |]) Replace
                             , $(adaptiveS [p| [] |]) $
                                   (\[] _ -> [undefined])
                             , $(normalV [p| (_,[]) |]) $
                                 $(rearr [| \((v:vs),[]) -> ((v,[]),(vs,[])) |])
                                  $(update [p| (x:xs) |] [d| x = Replace ; xs = pairAlignHelperREmpty |])
                              ]

pairAlignHelperLEmpty :: (Eq s1, Eq s2 ) => BiGUL' [([s1],[s2])] ([[s1]],[[s2]])
pairAlignHelperLEmpty = Case [ $(normalV [p| ([],[]) |]) $
                                 $(rearr [| \([],[]) -> [] |]) Replace
                             , $(adaptiveS [p| [] |]) $
                                   (\[] _ -> [undefined])
                             , $(normalV [p| ([],_) |]) $
                                 $(rearr [| \([],(v:vs)) -> (([],v),([],vs)) |])
                                  $(update [p| (x:xs) |] [d| x = Replace ; xs = pairAlignHelperLEmpty |])
                             ]



pairAlign' :: (Eq s1, Eq s2 ,Ord a) => (s1 -> a) -> (s2 -> a) -> BiGUL' [([s1],[s2])] ([[s1]],[[s2]])
pairAlign' f1 f2 = Case [      $(normalV [p| ([],_) |]) $ pairAlignHelperLEmpty
                             , $(normalV [p| (_,[]) |]) $ pairAlignHelperREmpty
                             , $(adaptiveS [p| [] |]) $
                                   (\[] _ -> [undefined])
                             , $(normalV [| pUnMatchedRight |]) $
                               $(rearr [| \(v1s,(v2:v2s)) -> (([],v2),(v1s,v2s)) |])
                                  $(update [p| (x:xs) |] [d| x = Replace ; xs = pairAlign' f1 f2 |])
                             , $(normalV [| pUnMatchedLeft|]) $
                               $(rearr [| \((v1:v1s),v2s) -> ((v1,[]),(v1s,v2s)) |])
                                  $(update [p| (x:xs) |] [d| x = Replace ; xs = pairAlign' f1 f2 |])
                             , $(normalV  [| pMatched|]) $
                               $(rearr [| \((v1:v1s),(v2:v2s)) -> ((v1,v2),(v1s,v2s)) |])
                                  $(update [p| (x:xs) |] [d| x = Replace ; xs = pairAlign' f1 f2 |])
                          ]
                  where pUnMatchedRight (v1,v2) =  f1 (head (head v1)) > f2 (head (head v2))
                        pUnMatchedLeft  (v1,v2) =  f1 (head (head v1)) < f2 (head (head v2))
                        pMatched        (v1,v2) =  f1 (head (head v1)) == f2 (head (head v2))

pairAlign :: (Eq s1, Eq s2 ,Ord a) => (s1 -> a) -> (s2 -> a) -> BiGUL' ([[s1]],[[s2]]) [([s1],[s2])]
pairAlign f1 f2 = Emb (put (pairAlign' f1 f2) [])
                      (\_ v -> get (pairAlign' f1 f2) v)


pairAlignExample :: ([[(String,Int)]],[[(Int,String)]])
pairAlignExample = ([ [("a",1),("b",1)],
                      [("a",2),("b",2)]
                    ],
                    [ [(2,"c"),(2,"d")],
                      [(3,"c")]
                    ])





subJoinDupHelper :: (Eq s1,Eq s2) => BiGUL' [(s1 , s2)] (s1,[s2])
subJoinDupHelper = Case [ $(normalSV [p| _ |] [p| (_, [v2]) |])
                          ($(rearr [| \(v1,[v2]) -> [(v1,v2)] |]) Replace)
                        , $(adaptiveSV [p| [] |] [p| _ |]) $
                          (\[] _ -> [undefined])
                        , $(normalSV [p| _ |] [p| _ |])
                          ($(rearr [| \(v1,v:vs) -> ((v1,v),(v1,vs)) |])
                            $(update [p| (x:xs) |] [d| x = Replace
                                                       xs = subJoinDupHelper |]))
                        ]


subJoinDup :: (Eq s1,Eq s2) => BiGUL' [[(s1,s2)]] ([s1],[s2])
subJoinDup = Case [ $(normalSV [p| _ |] [p| ([x], _) |])
                     ((flip Compose)
                       ($(rearr [| \([v1],v2s) -> (v1,v2s) |]) subJoinDupHelper)
                       ($(rearr [| \v -> [v] |]) Replace))
                   ,$(adaptiveSV [p| [[]] |] [p| _ |]) $
                       (\[[]] _ -> [[],[]])
                   ,$(normalSV [p| _ |] [p| _ |]) $
                       ($(rearr [| \(v:vs,v2) -> ((v,v2),(vs,v2)) |])
                          $(update [p| (x:xs) |] [d| x = subJoinDupHelper
                                                     xs = subJoinDup |]))
                  ]

subJoinHelper :: BiGUL' [((String,Int),(Int,String))] [(String,(Int,String))]
subJoinHelper = Case [ $(normalSV [p| _ |] [p| [] |])
                          ($(rearr [| \[] -> [] |]) Replace)
                     , $(adaptiveSV [p| [] |] [p| _ |]) $
                          (\[] _ -> [undefined])
                     , $(normalSV [p| _ |] [p| _ |]) $
                       $(rearr [| \(v:vs) -> (v,vs) |])
                          ($(update [p| (x:xs) |] [d| x = ($(rearr [| \(s1,i,s2) -> ((s1,i),(i,s2)) |]) Replace)
                                                      xs = subJoinHelper |]))
                        ]

subJoinHelper' :: BiGUL' [(String,(Int,String))] [((String,Int),(Int,String))]
subJoinHelper' =  Emb (\s -> put subJoinHelper [] s)
                      (\_ v -> get subJoinHelper v)

subJoin' ::  BiGUL' [(String,(Int,String))] ([(String,Int)],[(Int,String)])
subJoin' = ((flip Compose)
           (Case [ $(adaptiveSV [p| [] |] [p| _ |]) $
                     (\[] _ -> [[]])
                 , $(normalSV [p| _ |] [p| _ |]) $
                     subJoinDup])
             ((flip Compose)
               (unGroupBX (\(v1,_) -> fst v1))
                  subJoinHelper'))

subJoin :: BiGUL' ([(String,Int)],[(Int,String)]) [(String,(Int,String))]
subJoin = Emb (put subJoin' [])
              (\_ v -> get subJoin' v)

subJoinExample ::  ([(String,Int)],[(Int,String)])
subJoinExample = ([("a",1),("b",1)],
                  [(1,"c"),(1,"d"),(1,"e")])





align :: (Eq a, Eq b)
      => (a -> Bool)
      -> (a -> b -> Bool)
      -> BiGUL' a b
      -> (b -> a)
      -> (a -> Maybe a)
      -> BiGUL' [a] [b]
align p match b create conceal =
  Case [ $(normalSV [| null . filter p |] [p| [] |]) $
           $(rearr [| \ [] -> () |]) Skip
       , $(adaptiveSV [| not . null . filter p |] [p| [] |]) $
           \ss _ -> catMaybes (map (\s -> if p s then conceal s else Just s) ss)
       , $(normalSV [| \ss -> not (null (filter p ss)) && not (p (head ss)) |] [p| _:_ |]) $
           $(rearrAndUpdate [p| vs |] [p| _:vs |]
                            [d| vs = align p match b create conceal |])
       , $(normal' [| \ss vs -> not (null (filter p ss)) && p (head ss) && not (null vs) &&
                                match (head ss) (head vs) |]
                   [| \ss -> not (null (filter p ss)) && p (head ss) |]) $
           $(rearrAndUpdate [p| v : vs |]
                            [p| v : vs |]
                            [d| v  = b
                                vs = align p match b create conceal |])
       , $(adaptiveV [p| _:_ |]) $
           \ss (v:_) -> case find (flip match v) (filter p ss) of
                          Nothing -> create v:ss
                          Just s  -> s:delete s ss ]

data JoinFlag = DeleteBoth | DeleteLeft | DeleteRight

joinHelper :: JoinFlag -> BiGUL' [([(String,Int)],[(Int,String)])] [[(String,(Int,String))]]
joinHelper flag = align (\(ls,rs) -> not (null ls) && not (null rs))
                        (\(ls,_) ss -> snd (head ls) == (fst . snd) (head ss))
                        subJoin
                        (\((_,(i,_)):_) -> ([("",i)],[(i,"")]))
                        (\(ls,rs) -> case flag of
                                          DeleteBoth  -> Nothing
                                          DeleteLeft  -> Just ([],rs)
                                          DeleteRight -> Just (ls,[]))



joinBX :: JoinFlag -> BiGUL' ([(String,Int)],[(Int,String)]) [(String,(Int,String))]
joinBX flag = ((flip Compose) (groupBX (fst . snd))
               ((flip Compose) (joinHelper flag)
                  ((flip Compose) (pairAlign snd fst)
                          (productUnGroupBX snd fst))))


joinBXWithSort :: JoinFlag -> BiGUL' ([(String,Int)],[(Int,String)]) [(String,(Int,String))]
joinBXWithSort flag = Emb (\s -> get (joinBX flag) (sortS s))
                          (\s v -> put (joinBX flag) (sortS s) v)
                    where sortS (s1,s2) = (sortBy (\(_,x) (_,y) -> compare y x) s1,sortBy (\(x,_) (y,_) -> compare y x) s2)


joinExample ::  ([(String,Int)],[(Int,String)])
joinExample = ([("a",1),("b",2),("g",3),("c",2),("g",3)],
                  [(1,"c"),(1,"d"),(2,"e"),(4,"h")])

{-
*Main> get (joinBX DeleteBoth) joinExample
Right [("c",(2,"e")),("a",(1,"d")),("a",(1,"c")),("b",(2,"e"))]
*Main> get (joinBX DeleteLeft) joinExample
Right [("c",(2,"e")),("a",(1,"d")),("a",(1,"c")),("b",(2,"e"))]
*Main> get (joinBX DeleteRight) joinExample
Right [("c",(2,"e")),("a",(1,"d")),("a",(1,"c")),("b",(2,"e"))]



*Main> put (joinBX DeleteRight)  joinExample  [("a",(1,"d")),("a",(1,"c")),("b",(1,"c")),("b",(1,"d"))]
Right ([("a",1),("b",2),("c",2),("b",1)],[(1,"d"),(1,"c")])
*Main> put (joinBX DeleteBoth)  joinExample  [("a",(1,"d")),("a",(1,"c"))]
Right ([("a",1)],[(1,"c"),(1,"d")])
*Main> put (joinBX DeleteLeft)  joinExample  [("a",(1,"d")),("a",(1,"c"))]
Right ([("a",1)],[(1,"c"),(2,"e"),(1,"d")])
*Main> put (joinBX DeleteRight)  joinExample  [("a",(1,"d")),("a",(1,"c"))]
Right ([("b",2),("c",2),("a",1)],[(1,"c"),(1,"d")])
*Main> put (joinBX DeleteBoth)  joinExample  [("a",(1,"d")),("a",(1,"c")),("b",(1,"c"))]
Left (ErrorInfo "getCase: case exhaustion")
*Main> put (joinBX DeleteBoth)  joinExample  [("a",(1,"d")),("a",(1,"c")),("b",(1,"c")),("b",(1,"d"))]
Right ([("a",1),("b",1)],[(1,"d"),(1,"c")])
-}