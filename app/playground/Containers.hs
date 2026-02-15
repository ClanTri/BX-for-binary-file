{-# LANGUAGE TypeFamilies #-}
module Test.Containers where

import Generics.BiGUL
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import GHC.Generics

-- *Tree Data Type

-- |Binary Tree.
data Tree a = Nil | Node a (Tree a) (Tree a)
  deriving (Show, Eq)

instance Generic a => Generic (Tree a) where
      type Rep (Tree a) = (:+:) U1 ((:*:) (K1 R a) ((:*:) (K1 R (Tree a)) (K1 R (Tree a))))
      from Nil = L1 U1
      from (Node var_arJr var_arJs var_arJt)
        = R1 ((:*:) (K1 var_arJr) ((:*:) (K1 var_arJs) (K1 var_arJt)))
      to (L1 U1) = Nil
      to (R1 ((:*:) (K1 var_arJr) ((:*:) (K1 var_arJs) (K1 var_arJt))))
        = Node var_arJr var_arJs var_arJt

-- This is to prevent some issues with Template Haskell.
-- (workaround from here: https://ghc.haskell.org/trac/ghc/ticket/9813 )
$( return [] )

-- *List Lenses

-- |Map elements in the source at even positions to elements of the view.
mapL
  :: (MonadError' ErrorInfo m, Eq s, Eq v)
  => BiGUL m s v     -- ^BiGUL program to apply to the values
  -> BiGUL m [s] [v]
mapL u = CaseV [ ( return . null , $(rearr [| \ [] -> () |])
                     (CaseS [ (return . null, Normal  Skip),
                             (return . not . null, Adaptive (\s v -> return s))
                           ])),
                 ( return . not . null,
                   $(rearr [| \ (x : xs) -> (x, xs) |])
                     (CaseS [ (return . not . null, Normal (Update (UIn $ URight (UProd (UVar u) (UVar (mapL u)))))),
                             (return . null, Adaptive (\s v -> return [undefined]))
                           ]))
               ]

-- |Map elements in the source at even positions to elements of the view.
--
-- Another implementation of 'mapL'.
mapL'
  :: (MonadError' ErrorInfo m, Eq s, Eq v)
  => BiGUL m s v     -- ^BiGUL program to apply to the values
  -> BiGUL m [s] [v]
mapL' u = cond c1 c2 f1 f2 l1 l2
  where c1 = return . null
        c2 = return . null
        f1 = \ s _ -> return s
        f2 = \ _ _ -> return undefined
        l1 = $(rearr [| \ [] -> () |]) Skip
        l2 = $(rearr [| \ (x : xs) -> (x, xs) |]) $ Update (UIn $ URight (UProd (UVar u) (UVar (mapL' u))))

-- |Map elements in the source list at even positions to elements of the view.
mapLEven
  :: (MonadError' ErrorInfo m, Eq s, Eq v)
  => s               -- ^default value
  -> BiGUL m s v     -- ^BiGUL program to apply to the values
  -> BiGUL m [s] [v]
mapLEven s0 u = CaseV [ ( return . null ,
                          $(rearr [| \ [] -> () |])
                            (CaseS [ (return . (<2) . length, Normal Skip)
                                     , (return . not . null, Adaptive (\s v -> return $ take 1 s))
                                  ]))
                      , ( return . not . null,
                          $(rearr [| \ (x : xs) -> ((), (x, xs)) |])
                            (CaseS [ (return . (>=2) . length, Normal (Update (UIn $ URight (UProd (UVar Skip) (UIn $ URight (UProd (UVar u) (UVar (mapLEven s0 u))))))))
                                   , (return . null, Adaptive (\s v -> return [s0,undefined]))
                                   , (return . const True, Adaptive (\[s] v -> return [s,undefined]))
                                   ]))
                      ]

-- |Map elements in the source list at even positions to elements of the view.
--
-- Another implementation of 'mapLEven'.
mapLEven'
  :: (MonadError' ErrorInfo m, Eq s, Eq v)
  => s               -- ^default value
  -> BiGUL m s v     -- ^BiGUL program to apply to the values
  -> BiGUL m [s] [v]
mapLEven' s0 u = cond c1 c2 f1 f2 l1 l2
  where c1 = return . null
        c2 = return . (<2) . length
        f1 = \ s _ -> return $ take 1 s
        f2 = \ s _ -> return $ if null s then [s0,undefined] else head s : [undefined]
        l1 = $(rearr [| \ [] -> () |]) Skip
        l2 = $(rearr [| \ (x : xs) -> ((), (x, xs)) |]) $ Update (UIn $ URight (UProd (UVar Skip) (UIn $ URight (UProd (UVar u) (UVar (mapLEven' s0 u))))))

-- |Map elements in the source list at even positions to elements of the view.
--
-- The behavior of this map, in the put direction is different from the
-- 'mapLEven' one.
mapLEven2'
  :: (MonadError' ErrorInfo m, Eq s, Eq v)
  => s               -- ^default value
  -> BiGUL m s v     -- ^BiGUL program to apply to the values
  -> BiGUL m [s] [v]
mapLEven2' s0 u = cond c1 c2 f1 f2 l1 l2
  where c1 = return . null
        c2 = return . null
        f1 = \ s _ -> return  []
        f2 = \ s _ -> return [s0]
        l1 = $(rearr [| \ [] -> () |]) Skip
        l2 = $(rearr [| \ (x : xs) -> ((), (x, xs)) |]) $ Update (UIn $ URight (UProd (UVar Skip) (UIn $ URight (UProd (UVar u) (UVar (mapLEven2' s0 u))))))

-- *Tree Lenses

treeSize :: Tree a -> Int
treeSize Nil = 0
treeSize (Node _ l r) = 1 + treeSize l + treeSize r

treeIsEmpty :: Tree a -> Bool
treeIsEmpty Nil = True
treeIsEmpty _ = False

-- |Map elements of a tree to elements of another tree.
mapT
  :: (MonadError' ErrorInfo m, Generic s, Generic v, Eq s, Eq v)
  => BiGUL m s v               -- ^BiGUL program to apply to the values
  -> BiGUL m (Tree s) (Tree v)
mapT u = CaseV [ ( return . treeIsEmpty ,
                      $(rearr [| \ Nil -> () |])
                        (CaseS [ (return . treeIsEmpty, Normal Skip),
                                (return . not . treeIsEmpty, Adaptive (\s v -> return Nil))
                              ])),
                    ( return . not . treeIsEmpty,
                      $(rearr [| \ (Node x xs ys) -> (x, (xs, ys)) |])
                        (CaseS [ (return . not . treeIsEmpty, Normal (Update (UIn $ URight (UProd (UVar u) (UProd (UVar (mapT u)) (UVar (mapT u))))))),
                                (return . treeIsEmpty, Adaptive (\s v -> return (Node undefined Nil Nil)))
                              ]))
                  ]

-- |Map elements of a tree to elements of another tree.
--
-- Another implementation of 'mapT'.
mapT'
  :: (MonadError' ErrorInfo m, Generic s, Generic v, Eq s, Eq v)
  => BiGUL m s v               -- ^BiGUL program to apply to the values
  -> BiGUL m (Tree s) (Tree v)
mapT' u = cond c1 c2 f1 f2 l1 l2
  where c1 = return . treeIsEmpty
        c2 = return . treeIsEmpty
        f1 = \ s _ -> return Nil
        f2 = \ s _ -> return (Node undefined Nil Nil)
        l1 = $(rearr [| \ Nil -> () |]) Skip
        l2 = $(rearr [| \ (Node x xs ys) -> (x, (xs, ys)) |]) $ Update (UIn $ URight (UProd (UVar u) (UProd (UVar (mapT' u)) (UVar (mapT' u)))))

-- |Mirror a 'Tree'.
mirrorT
  :: (MonadError' ErrorInfo m, Generic a, Eq a)
  => BiGUL m (Tree a) (Tree a)
mirrorT = CaseV [ ( return . treeIsEmpty
                  , $(rearr [| \ Nil -> () |])
                    (CaseS [ (return . treeIsEmpty, Normal Skip)
                          , (return . const True, Adaptive (\ _ _ -> return Nil))
                          ])
                  )
                , ( return . not . treeIsEmpty
                  , $(rearr [| \ (Node x xs ys) -> (x, (ys, xs)) |])
                    (CaseS [ (return . not . treeIsEmpty, Normal $ Update (UIn $ URight (UProd (UVar Replace) (UProd (UVar mirrorT) (UVar mirrorT)))))
                           , (return . const True, Adaptive (\s v -> return (Node undefined Nil Nil)))
                           ])
                  )
                ]

-- |Mirror a 'Tree'.
--
-- Another implementation of 'mapT'.
mirrorT'
  :: (MonadError' ErrorInfo m, Generic a, Eq a)
  => BiGUL m (Tree a) (Tree a)
mirrorT' = cond c1 c2 f1 f2 l1 l2
  where c1 = return . treeIsEmpty
        c2 = return . treeIsEmpty
        f1 = (\ _ _ -> return Nil)
        f2 = (\s v -> return (Node undefined Nil Nil))
        l1 = $(rearr [| \ Nil -> () |]) Skip
        l2 = $(rearr [| \ (Node x xs ys) -> (x, (ys, xs)) |]) $ Update (UIn $ URight (UProd (UVar Replace) (UProd (UVar mirrorT') (UVar mirrorT'))))

-- *Auxiliary Functions

-- |Conditional BiGUL program.
cond
  :: (MonadError' ErrorInfo m)
  => (v -> m Bool)   -- ^ c1 - view condition
  -> (s -> m Bool)   -- ^ c2 - source condition
  -> (s -> v -> m s) -- ^  c1 & !c2 adaptive function
  -> (s -> v -> m s) -- ^ !c1 &  c2 adaptive function
  -> BiGUL m s v     -- ^  c1 &  c2 BiGUL program
  -> BiGUL m s v     -- ^ !c1 & !c2 BiGUL program
  -> BiGUL m s v
cond c1 c2 f1 f2 l1 l2 =
  CaseV
    [ ( c1
      , CaseS [ (c2, Normal l1) , (return . const True, Adaptive f1) ]
      )
    , (return . const True
      , CaseS [ (\s -> not <$> c2 s, Normal l2) , (return . const True, Adaptive f2) ]
      )
    ]
