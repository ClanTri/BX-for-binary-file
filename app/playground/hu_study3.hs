{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{- Studying notes by Zhenjiang Hu @ 23/09/2015
   This note demonstates definitions of interesting put lenses
   over pairs and lists.
-}

import GHC.Generics
import Generics.BiGUL.Interpreter
import Util
import Generics.BiGUL.AST
import Language.Haskell.TH as TH
import Generics.BiGUL.TH

-- upFst:
--  (a,b) <-> a
upFst :: Eq a => BiGUL (a,b) a
--upFst = Rearr RVar (EDir DVar `EProd` EConst ())
--                   (Update (UVar Replace `UProd` UVar Skip))

upFst = $(rearrV [| \x -> (x,()) |]) $(update [p| (x,()) |] [p| (x,_) |]  [d| x = Replace |])

{-

*Main> testGet upFst (1,2)
Right 1
*Main> testPut upFst (1,2) 100
Right (100,2)

-}

-- upSnd:
--  (a,b) <-> b
upSnd :: Eq b => BiGUL (a,b) b
--upSnd = Rearr RVar (EConst () `EProd` EDir DVar)
--                   (Update (UVar Skip `UProd` UVar Replace))

upSnd = $(rearrV [|  \x -> ((),x) |]) $(update [p| ((),x) |] [p| (_,x) |] [d| x = Replace |])

{-

*Main> testGet upSnd (1,2)
Right 2
*Main> testPut upSnd (1,2) 200
Right (1,200)

-}

upSwap :: (Eq a, Eq b) => BiGUL (a,b) (b,a)
--upSwap = Rearr (RVar `RProd` RVar) (EDir (DRight DVar) `EProd` EDir (DLeft DVar)) Replace

upSwap = $(update [p| (x,y) |] [p| (y,x) |] [d| x=Replace; y = Replace |])

{-

*Main> testGet upSwap (1,2)
Right (2,1)
*Main> testPut upSwap (1,2) (200,100)
Right (100,200)

-}

-- upHead
-- [100,2,3,4] <-> 100

-- upHead :: (Eq a, MonadError' ErrorInfo m) => BiGUL m [a] a
--upHead = CaseS [ (return . (==[]), Normal $ failMsg "upHead: the source should not be empty"),
--                 (return . (/=[]), Normal $ (Update (UElem (UVar Replace) (UVar Skip)) @@ upFst)) ]

-- upHead = CaseS [ $(normal [p| [] |]) $ failMsg "upHead: the source should not be empty",
--                  $(normal' [| (/=[]) |])  ($(update [p| x:_ |] [d| x = Replace |]) @@ upFst) ]

{-

*Main> testGet upHead [1,2,3]
Right 1
*Main> testPut upHead [1,2,3] 100
Right [100,2,3]
*Main> testPut upHead [] 100
Left (ErrorInfo "upHead: the source should not be empty")

-}

-- upTail :: (Eq a, MonadError' ErrorInfo m) => BiGUL m [a] [a]
--upTail = CaseS [ (return . (==[]), Normal $ failMsg "upTail: the source should not be empty"),
--                 (return . (/=[]), Normal $ (Update (UElem (UVar Skip) (UVar Replace)) @@ upSnd)) ]
-- upTail = CaseS [ $(normal [p| [] |]) $ failMsg "upTail: the source should not be empty",
--                  $(normal' [| (/=[]) |])  ($(update [p| _:x |] [d| x = Replace |]) @@ upSnd) ]
{-

*Main> testGet upTail [1,2,3]
Right [2,3]
*Main> testPut upTail [1,2,3] [100,200,300]
Right [1,100,200,300]

-}


-- mapU upHead:
--   [[1,2,3],[10,11,12,13],[20]] <-> [1,10,20]

-- mapUpHead :: MonadError' ErrorInfo m => BiGUL m [[Int]] [Int]
-- mapUpHead = mapU [0] upHead

{-

*Main> testGet mapUpHead [[1,2,3],[10,11,12,13],[20]]
Right [1,10,20]
*Main> testPut mapUpHead [[1,2,3],[10,11,12,13],[20]] [100,200,300]
Right [[100,2,3],[200,11,12,13],[300]]
*Main> testPut mapUpHead [[1,2,3],[10,11,12,13],[20]] [100,200,300,400]
Right [[100,2,3],[200,11,12,13],[300],[400]]
*Main> testPut mapUpHead [[1,2,3],[10,11,12,13],[20]] [100,200]
Right [[100,2,3],[200,11,12,13]]

-}



-- embedAt 2:
--  [1,2,300,4] <--> 300

-- embedAt :: (Eq a, MonadError' ErrorInfo m) => Int -> BiGUL m [a] a
-- embedAt i | i==0      = upHead
--           | otherwise = upTail @@ embedAt (i-1)

{-

*Main> testGet (embedAt 3) [1..10]
Right 4
*Main> testPut (embedAt 3) [1..10] 100
Right [1,2,3,100,5,6,7,8,9,10]


-}

-- uLefts
--  [Left 1, Right 1, Left 3, Left 3, Right 2] <-> [Left 1, Left 3, Left 3]

-- original uLefts (also workable)
--uLefts :: (Eq a) => BiGUL [Either a a] [Either a a]
--uLefts =
--  Case [ ( (\_ v -> case v of [] -> True; _ -> False),
--            Normal
--            (Case [ ( (\s _ -> all (not . isLeft) s),
--                      Normal
--                        (RearrV (PIn (PLeft (PConst ())))  (EConst ()) Skip)
--                        (\s -> all (not . isLeft) s) )
--                  , ( (\_ _ -> True),
--                      Adaptive (\s _ -> rmLefts s))
--                  ]
--            )
--            (const True)
--         )
--       ,  ( (\_ v -> case v of _:_ -> True; _ -> False),
--            Normal
--            (Case [ ( (\s _ -> s /= [] && hasLeftHead s),
--                      Normal ( RearrS (PIn (PRight $ (PLeft PVar) `PProd` PVar))
--                                      (EDir (DLeft DVar) `EProd` EDir (DRight DVar))
--                                      (RearrV (PIn (PRight $ (PLeft PVar) `PProd` PVar))
--                                              (EDir (DLeft DVar) `EProd` EDir (DRight DVar))
--                                              (Replace `Prod` uLefts))
--                             )
--                             (\s -> s /= [] && hasLeftHead s) )
--                  , ( (\s _ -> s /= [] && not (hasLeftHead s)),
--                      Normal ( RearrS (PIn $ PRight (PRight PVar `PProd` PVar))
--                                      (EDir (DRight DVar))
--                                      (RearrV (PIn $ PRight (PVar `PProd` PVar))
--                                              (EIn $ ERight $ EDir (DLeft DVar) `EProd` EDir (DRight DVar))
--                                              (uLefts))
--                             )
--                             (\s -> s /= [] && not (hasLeftHead s)) )
--                  , (\s _ -> s == [], Adaptive (\_ _-> undefined))
--                  ])
--            (const True)

--          )
--        ]

uLefts :: (Eq a) => BiGUL [Either a a] [Either a a]
uLefts = Case [ $(normalV [p| [] |]) $
                  $(rearrV [| \[] -> () |]) $
                    Case [ $(normalS [| all (not . isLeft) |]) Skip
                         , $(adaptiveS [p| _ |]) (\s _ -> rmLefts s)
                         ]
              , $(normalV [p| _ : _ |]) $
                     (Case [ $(normalS [p| Left _ : _ |]) $
                               $(update [p| Left x : xs |]
                                        [p| Left x : xs |]
                                        [d| x  = Replace; xs = uLefts |])
                           , $(normalS [p| Right _ : _ |]) $
                               $(rearrV [| \xs -> ((), xs) |]) $
                                 $(update [p| ((), xs) |] [p| _ : xs |] [d| xs = uLefts |])
                           , $(adaptiveS [p| [] |]) $
                               \_ _ -> [Left undefined]
                           ])
              ]


hasLeftHead (Left _ : _) = True
hasLeftHead _ = False
rmLefts xs = [ x | x <- xs, not (isLeft x) ]
isLeft (Left _) = True
isLeft _ = False

{-

*Main> get uLefts [Left 1, Right 1, Left 3, Left 3, Right 2]
Right [Left 1,Left 3,Left 3]
*Main> put uLefts [Left 1, Right 1, Left 3, Left 3, Right 2] []
Right [Right 1,Right 2]
*Main> put uLefts [Left 1, Right 1, Left 3, Left 3, Right 2] [Left 100]
Right [Left 100,Right 1,Right 2]
*Main> put uLefts [Left 1, Right 1, Left 3, Left 3, Right 2] [Left 100, Left 200, Left 300, Left 400]
Right [Left 100,Right 1,Left 200,Left 300,Right 2,Left 400]

-}


-- rmLeftTags
--  [Left 1, Left 2, Left 3] <-> [1,2,3]

rmLeftTags :: (Eq a, Show a) => BiGUL [Either a a] [a]
rmLeftTags = mapU uLeft

uLeft :: (Eq a) => BiGUL (Either a a) a
uLeft = Case [ $(normalS [p| Left _ |]) $ $(update [p| x |] [p| Left x |] [d| x = Replace |])
             , $(normalS [p| _      |]) $ Fail "rmLeftTags: any element in the source should be a left value."
             ]

{-

*Main> testGet (rmLeftTags 0) [Left 1, Left 2, Left 3]
Right [1,2,3]
*Main> testGet (rmLeftTags 0) [Left 1, Left 2, Left 3, Right 4]
Left (ErrorInfo "failed.")
*Main> testPut (rmLeftTags 0) [Left 1, Left 2, Left 3] [10,20,30]
Right [Left 10,Left 20,Left 30]
*Main> testPut (rmLeftTags 0) [Left 1, Left 2, Left 3] [10,20,30,40]
Right [Left 10,Left 20,Left 30,Left 40]
*Main> testPut (rmLeftTags 0) [Left 1, Left 2, Left 3] [10,20]
Right [Left 10,Left 20]
*Main> testPut (rmLeftTags 0) [Left 1, Right 2, Left 3] [10,20]
Left (ErrorInfo "rmLeftTags: all elements in the source should be a left value.")

-}

