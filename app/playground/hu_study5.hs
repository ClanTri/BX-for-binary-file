{-# LANGUAGE FlexibleContexts #-}
{- Studying notes by Zhenjiang Hu @ 25/09/2015
   Try to implement a set of alignments using CaseS and CaseV.
-}

import GHC.Generics
import Generics.BiGUL
import Util
import Generics.BiGUL.AST
import Language.Haskell.TH as TH
import Generics.BiGUL.TH

-- alignPos:
--   [("a",10),("b",20),("c",30)] <-> [10,20,30]

data Edit a = Ins Pos
            | Del Pos
type Pos = Int

-- Assume that the edition list is in the order of edition positions

uPairByList ::(Eq c, MonadError' ErrorInfo m) =>  BiGUL m a c -> BiGUL m b [c] -> BiGUL m (a,b) [c]
--uPairByList u1 u2 =  CaseV [ CaseVBranch (PConst []) $ failMsg "uPairByList: the view should not be an empty list",
--                         CaseVBranch (PElem PVar PVar) $ Update (UVar u1 `UProd` UVar u2)
--                       ]

uPairByList u1 u2 =  CaseV [ $(branch [p| [] |]) $ failMsg "uPairByList: the view should not be an empty list",
                             $(branch [p| _:_ |]) $(update [p| (x1,x2) |] [d| x1 = u1 ; x2 = u2 |])
                           ]

uListByPair :: (Eq a, MonadError' ErrorInfo m) => BiGUL m a b -> BiGUL m [a] c -> BiGUL m [a] (b,c)
--uListByPair u1 u2 =  CaseS [ (return . (==[]), Normal $ failMsg "uListByPair: the source should not be an empty list"),
--                         (return . const True, Normal $ Update (UVar u1 `UElem` UVar u2))
--                       ]

uListByPair u1 u2 =  CaseS [ $(normal [p| [] |]) $ failMsg "uListByPair: the source should not be an empty list",
                             $(normal [p| _ |]) $(update [p| x1:x2 |] [d| x1 = u1 ; x2 = u2 |])
                          ]

uProd :: BiGUL m a c -> BiGUL m b d -> BiGUL m (a,b) (c,d)
--uProd u1 u2 =  Update (UVar u1 `UProd` UVar u2)

uProd u1 u2 = $(update [p| (x1,x2) |] [d| x1 = u1 ; x2 = u2 |])

uListByList :: (Eq a, Eq b, MonadError' ErrorInfo m) => BiGUL m a b -> BiGUL m [a] [b] -> BiGUL m [a] [b]
uListByList u1 u2 = uListByPair Replace Replace @@ uProd u1 u2 @@ uPairByList Replace Replace

-- main function
-- I hope the lens "alignPos [Ins 2, Del 3] Replace" has the following behavior:
--     get: [1,2,3,4,5] -> [1,2,3,4,5]
--     put: [1,2,3,4,5] [1,2,100,3,5] -> [1,2,100,3,5]

alignPos :: (Val a, Eq a, Eq b, MonadError' ErrorInfo m) => [Edit b] -> BiGUL m a b -> BiGUL m [a] [b]
alignPos eds u = alignPos' eds 0 u

alignPos' :: (Val a, Eq a, Eq b, MonadError' ErrorInfo m) => [Edit b] -> Pos -> BiGUL m a b -> BiGUL m [a] [b]
{-
alignPos' [] _ u = mapU defaultV u
alignPos' (ed:eds) pos0 u
  | getPos ed > pos0  = uListByList u (alignPos' (ed:eds) (pos0+1) u)
  | getPos ed == pos0 = case ed of
                          Ins pos -> CaseS [ (return . isNewHead,
                                                Normal $ uListByList u (alignPos' eds pos0 u)),
                                             (return . not . isNewHead,
                                                Adaptive (\s v -> return (newV : s)))
                                           ]
                          Del pos -> CaseS [ (return . (==[]),
                                                Normal $ failMsg "alignPos': no corresponding source element for deletion"),
                                             (return . const True,
                                                Adaptive (\s v -> return (tail s)))
                                           ]
  | otherwise = error "The positions in the edition list should be in an increasing order."
  where
    getPos (Ins pos) = pos
    getPos (Del pos) = pos
    isNewHead (x:xs) = isNewV x
    isNewHead _ = False
-}

alignPos' [] _ u = mapU defaultV u
alignPos' (ed:eds) pos0 u
  | getPos ed > pos0  = uListByList u (alignPos' (ed:eds) (pos0+1) u)
  | getPos ed == pos0 = case ed of
                          Ins pos -> CaseS [ $(normal' [| isNewHead |]) $ uListByList u (alignPos' eds pos0 u),
                                             $(adaptive' [| not . isNewHead |]) (\s v -> return (newV : s))
                                           ]
                          Del pos -> CaseS [ $(normal [p| [] |]) $ failMsg "alignPos': no corresponding source element for deletion",
                                             $(adaptive [p| _ |]) (\s v -> return (tail s))
                                           ]
  | otherwise = error "The positions in the edition list should be in an increasing order."
  where
    getPos (Ins pos) = pos
    getPos (Del pos) = pos
    isNewHead (x:xs) = isNewV x
    isNewHead _ = False

class Eq a => Val a where
  newV :: a
  defaultV :: a
  isNewV :: a -> Bool
  isNewV v = v == newV


-- for testing

instance Val Int where
  newV = -1000
  defaultV = 0

u1 :: MonadError' ErrorInfo m => BiGUL m [Int] [Int]
u1 = alignPos [] Replace

u2 :: MonadError' ErrorInfo m => BiGUL m [Int] [Int]
u2 = alignPos [Ins 2, Del 3] Replace
