{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


import GHC.Generics
import Generics.BiGUL
import Util
import Generics.BiGUL.AST
import Language.Haskell.TH as TH hiding (Name)
import Generics.BiGUL.TH
import Data.Set
import Control.Monad.Except

type Name = String
data NTree = Branchs [(Name,NTree)] deriving(Eq,Show)
deriveBiGULGeneric ''NTree

xsplit :: MonadError' ErrorInfo m => [(Name,NTree)] -> (Name -> m Bool) -> m ([(Name,NTree)],[(Name,NTree)])
xsplit [] _ = return ([],[])
xsplit ((x@(name,_)):xs) p = do b <- p name
                                (ls1,ls2) <- xsplit xs p
                                if b then return (x:ls1,ls2) else return (ls1,x:ls2)


xfork :: MonadError' ErrorInfo m => (Name -> m Bool) -> (Name -> m Bool) -> BiGUL m NTree NTree -> BiGUL m NTree NTree -> BiGUL m NTree NTree
xfork ps pv bigul1 bigul2 = Emb (\s -> do let (Branchs sbs) = s
                                          (s1,s2) <- xsplit sbs ps
                                          (Branchs v1) <-  get bigul1 (Branchs s1)
                                          (Branchs v2) <-  get bigul2 (Branchs s2)
                                          bs1 <- mapM (pv . fst) v1
                                          bs2 <- mapM (pv . fst) v2
                                          if and bs1 && not (or bs2)
                                          then return $ (Branchs (v1++v2))
                                          else throwError $ ErrorInfo "view is not valid int get Xfork")
                                (\s v -> do let (Branchs sbs) = s
                                                (Branchs vbs) = v
                                            (s1,s2) <- xsplit sbs ps
                                            (v1,v2) <- xsplit vbs pv
                                            (Branchs s1') <- put bigul1 (Branchs s1) (Branchs v1)
                                            (Branchs s2') <- put bigul2 (Branchs s2) (Branchs v2)
                                            return $ (Branchs (s1'++s2')))


hoist :: MonadError' ErrorInfo m => Name -> BiGUL m NTree NTree
hoist name = Rearr RVar  (EIn (EIn (ERight
                          (EProd
                           (EProd (EConst name) (EDir DVar)) (EIn (ELeft (EConst ()))))))) Replace

plunge :: MonadError' ErrorInfo m => Name -> BiGUL m NTree NTree
plunge name = Rearr (RIn (RIn (RRight (RProd (RProd (RConst name) RVar) (RIn (RLeft (RConst ())))))))
                    (EDir (DLeft (DRight DVar)))
                    Replace


rename :: MonadError' ErrorInfo m => Name -> Name -> BiGUL m NTree NTree
rename n1 n2 = xfork ps pv (Compose (hoist n1) (plunge n2)) Replace
               where ps n | n == n1   = return True
                          | otherwise = return False
                     pv n | n == n2   = return True
                          | otherwise = return False



xconst :: (Eq a,Show a,MonadError' ErrorInfo m) => a -> BiGUL m s a
xconst val = Rearr (RConst val)
                   (EConst ())
                   Skip


xfilter :: MonadError' ErrorInfo m => Set Name -> BiGUL m NTree NTree
xfilter names = xfork ps pv Replace (xconst (Branchs []))
                where ps n = return (member n names)
                      pv n = return (member n names)


xprune :: MonadError' ErrorInfo m => Name -> BiGUL m NTree NTree
xprune  name = xfork ps pv (xconst (Branchs [])) Replace
               where ps n | n == name   = return True
                          | otherwise   = return False
                     pv n | n == name   = return True
                          | otherwise   = return False


xadd :: MonadError' ErrorInfo m => Name -> NTree -> BiGUL m NTree NTree
xadd name tree = xfork ps pv (xconst (Branchs [(name,tree)])) Replace
                 where ps _  = return False
                       pv n | n == name   = return True
                            | otherwise   = return False


xfocus :: MonadError' ErrorInfo m => Name -> BiGUL m NTree NTree
xfocus name = Compose (xfilter (singleton name)) (hoist name)



hoistNonunique :: MonadError' ErrorInfo m => Name -> Set Name -> BiGUL m NTree NTree
hoistNonunique name names =  xfork ps pv (hoist name) Replace
                             where ps n | n == name   = return True
                                        | otherwise   = return False
                                   pv n = return (member n names)


xmap :: MonadError' ErrorInfo m => BiGUL m NTree NTree -> BiGUL m NTree NTree
xmap bigul = $(rearr [| \(Branchs bs) -> bs |])
             ($(update [p| Branchs bs |]
                       [d| bs = Align (\_ -> return True)
                                      (\(names,_) (namev,_) -> return (names == namev))
                                      $(update [p| (n,tree) |]
                                               [d| n = Replace ; tree = bigul|])
                                      (\_ -> return ("",(Branchs [])))
                                      (\_ -> return Nothing)|]))



wmap :: MonadError' ErrorInfo m => BiGUL m (Name,NTree) (Name,NTree) -> BiGUL m NTree NTree
wmap bigul = $(rearr [| \(Branchs bs) -> bs |])
             ($(update [p| Branchs bs |]
                       [d| bs = Align (\_ -> return True)
                                      (\(names,_) (namev,_) -> return (names == namev))
                                      bigul
                                      (\_ -> return ("",(Branchs [])))
                                      (\_ -> return Nothing)|]))


{-
testSubTreeEq :: MonadError' ErrorInfo m => Name -> Name -> NTree -> m Bool
testSubTreeEq m n ts = do  msub <- findSub m ts
                           nsub <- findSub n ts
                           return (msub == nsub)
                           where findSub name (Branchs []) = throwError $ ErrorInfo "branch name does not exist"
                                 findSub name (Branchs ((n,sub):xs)) | name == n = (return sub)
                                                                     | otherwise = findSub name (Branchs xs)

copy :: MonadError' ErrorInfo m => Name -> Name -> BiGUL m NTree NTree
copy m n = CaseV [ (testSubTreeEq m n , xfork ps pv ($(rearr [| \t -> (Branchs []) |]) Replace) Replace)
                 ]
           where ps _  = return False
                 pv na | na == n    = return True
                       | otherwise  = return False
-}

ccond :: MonadError' ErrorInfo m => (s -> m Bool) -> BiGUL m s v -> BiGUL m s v -> BiGUL m s v
ccond ps bigul1 bigul2 = CaseS [ (ps , Normal bigul1),
                                 (\s -> ps s >>= return . not , Normal bigul2)
                               ]

acond :: MonadError' ErrorInfo m => (s -> m Bool) -> (v -> m Bool) -> BiGUL m s v -> BiGUL m s v -> BiGUL m s v
acond ps pv bigul1 bigul2 = CaseS [ (ps , Normal  (CaseV [(pv , bigul1)]) ),
                                    (\s -> ps s >>= return . not , Normal (CaseV [(\v -> pv v >>= return . not , bigul2)]) )]

-- Jorge's cond , similar to acond
jcond
  :: (MonadError' ErrorInfo m)
  => (v -> m Bool)   -- ^ c1 - view condition
  -> (s -> m Bool)   -- ^ c2 - source condition
  -> (s -> v -> m s) -- ^  c1 & !c2 adaptive function
  -> (s -> v -> m s) -- ^ !c1 &  c2 adaptive function
  -> BiGUL m s v     -- ^  c1 &  c2 BiGUL program
  -> BiGUL m s v     -- ^ !c1 & !c2 BiGUL program
  -> BiGUL m s v
jcond c1 c2 f1 f2 l1 l2 =
  CaseV
    [ ( c1
      , CaseS [ (c2, Normal l1) , (return . const True, Adaptive f1) ]
      )
    , (return . const True
      , CaseS [ (\s -> not <$> c2 s, Normal l2) , (return . const True, Adaptive f2) ]
      )
    ]

-- acond with default NTree
acond' :: MonadError' ErrorInfo m => (NTree -> m Bool) -> (NTree -> m Bool) -> BiGUL m NTree NTree -> BiGUL m NTree NTree -> BiGUL m NTree NTree
acond' ps pv bigul1 bigul2 = jcond pv ps (\_ _ -> return $ Branchs []) (\_ _ -> return $ Branchs []) bigul1 bigul2


xcond ::  MonadError' ErrorInfo m =>
           (s -> m Bool)
        -> (v -> m Bool)
        -> (v -> m Bool)
        -> (s -> v -> m s)
        -> (s -> v -> m s)
        -> BiGUL m s v
        -> BiGUL m s v
        -> BiGUL m s v
xcond ps pv1 pv2 f1 f2 bigul1 bigul2 =
  CaseV [(\v -> pv1 v >>= \b1 -> pv2 v >>= \b2 -> return (b1 && b2),
         CaseS [ (ps , Normal bigul1),
                 (\s -> ps s >>= return . not , Normal bigul2) ]),
         (\v -> pv1 v >>= \b1 -> pv2 v >>= \b2 -> return (b1 && (not b2)),
         CaseS [ (ps , Normal bigul1),
                 (\s -> ps s >>= return . not , Adaptive f1) ]),
         (\v -> pv1 v >>= \b1 -> pv2 v >>= \b2 -> return ((not b1) && b2),
         CaseS [ (ps , Adaptive f2),
                 (\s -> ps s >>= return . not , Normal bigul2) ])
        ]

{-
Notes:
ccond , acond and xcond have same get semantics , so user need to figure out their put semantics to distinguish them.

but tree lens like xadd , xfocus ... obviously just provide abstraction in get semantics.

It's difficult to switch between get and put semantics when writing programs.
-}


list2NTree :: [Name] -> NTree
list2NTree []     = Branchs []
list2NTree (x:xs) = Branchs [("hd", Branchs [(x,Branchs [])]) , ("tl", list2NTree xs)]

nTree2List :: NTree -> [Name]
nTree2List (Branchs []) = []
nTree2List (Branchs [("hd", Branchs [(x,Branchs [])]),("tl", tltree)]) = x:(nTree2List tltree)
nTree2List _ = ["Invalid NTree to list"]

xhd :: MonadError' ErrorInfo m => BiGUL m NTree NTree
xhd = xfocus "hd"

xtl :: MonadError' ErrorInfo m => BiGUL m NTree NTree
xtl = xfocus "tl"

list_map :: MonadError' ErrorInfo m => BiGUL m NTree NTree -> BiGUL m NTree NTree
list_map bigul = wmap (CaseV [ $(branch [p|("hd",_)|]) $(update [p| (n,tree) |]
                                                                [d| n = Replace ; tree = bigul|]),
                               $(branch [p|("tl",_)|]) $(update [p| (n,tree) |]
                                                                [d| n = Replace ; tree = list_map bigul|])
                             ])

xrotate :: MonadError' ErrorInfo m => BiGUL m NTree NTree
xrotate = CaseS [$(normal [p| Branchs [] |]) Replace,
                 $(normal [p| Branchs [("hd",_),("tl",Branchs [])]|]) Replace,
                 $(normal [p| _ |]) (Compose (rename "hd" "tmp")
                                             (Compose (hoistNonunique "tl" (fromList ["hd","tl"]))
                                                      (xfork phd phd Replace
                                                        (Compose (rename "tmp" "hd")
                                                                 (Compose xrotate (plunge "tl"))))))]
          where phd n = return (n == "hd")

list_reverse :: MonadError' ErrorInfo m => BiGUL m NTree NTree
list_reverse =  Compose (wmap (CaseV [$(branch [p| ("hd",_) |]) Replace,
                                      $(branch [p| ("tl",_) |]) $(update [p| (tlr,tree) |] [d| tlr = Replace ; tree = list_reverse |])]))
                        xrotate




xrotateAcond :: MonadError' ErrorInfo m => BiGUL m NTree NTree
xrotateAcond =  acond p p Replace  (Compose (rename "hd" "tmp")
                                    (Compose (hoistNonunique "tl" (fromList ["hd","tl"]))
                                            (xfork phd phd Replace
                                              (Compose (rename "tmp" "hd")
                                                       (Compose xrotate (plunge "tl"))))))
                where p (Branchs []) = return True
                      p (Branchs [("hd",_),("tl",Branchs [])]) = return True
                      p _ = return False
                      phd n = return (n == "hd")

{-
*Main> fmap nTree2List $ testPut xrotateAcond   (list2NTree ["fs","grt","hsd"])  (list2NTree ["aa","bb","cc"])
Right ["cc","aa","bb"]
*Main> fmap nTree2List $ testPut xrotateAcond   (list2NTree ["fs","grt"])  (list2NTree ["aa","bb","cc"])
Left (ErrorInfo "updated source does not satisfy the condition.")
*Main> fmap nTree2List $ testPut xrotateAcond   (list2NTree ["fs","grt","hsd","ad"])  (list2NTree ["aa","bb","cc"])
Left (ErrorInfo "updated source does not satisfy the condition.")
-}

