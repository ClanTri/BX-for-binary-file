{-# Language TemplateHaskell #-}
module THAST where

import Generics.BiGUL
import Generics.BiGUL.TH
import Language.Haskell.TH as TH
import Data.Map (Map)
import Data.List as List
import Data.Maybe
import qualified Data.Map as Map

rearrPatTH :: TH.Pat -> Q (Exp, Map Name Exp)
rearrPatTH (VarP name)   = do
  Just conRVar <- lookupValueName "RVar"
  Just conDVar <- lookupValueName "DVar"
  return (ConE conRVar, Map.singleton name (ConE conDVar))
rearrPatTH (ConP name ps) = do
  (_, [nRConst, nRIn, nRLeft, nRRight, nRProd, nDLeft, nDRight]) <-
    lookupNames [] [ "Generics.BiGUL.AST." ++ s | s <- ["RConst", "RIn", "RLeft", "RRight", "RProd", "DLeft", "DRight"] ] "cannot find data constructors from Generic.BiGUL.AST"
  ConE nUnit <- [| () |]
  ems <- mapM rearrPatTH ps
  lrs <- lookupLRs name
  let con = foldl (.) (AppE (ConE nRIn))
              (map (AppE . ConE . contag nRLeft nRRight) lrs)
  let prodE = case ps of
                [] -> ConE nRConst `AppE` ConE nUnit
                _  -> foldr1 (\e0 e1 -> (ConE nRProd `AppE` e0) `AppE` e1)
                        (map fst ems)
  let envs = zipWith (Map.map . foldr (.) id . map (AppE . ConE . contag nDLeft nDRight))
                     (constructLRs (length ps)) (map snd ems)
  return (con prodE, Map.unions envs)
rearrPatTH (TupP [p])    = rearrPatTH p
rearrPatTH (TupP (p:ps)) = do
  (lexp, lenv) <- rearrPatTH p
  (rexp, renv) <- rearrPatTH (TupP ps)
  Just conRProd <- lookupValueName "RProd"
  Just conDLeft <- lookupValueName "DLeft"
  Just conDRight <- lookupValueName "DRight"
  return ((ConE conRProd `AppE` lexp) `AppE` rexp,
          Map.map (ConE conDLeft `AppE`) lenv `Map.union`
          Map.map (ConE conDRight `AppE`) renv)


rearrExprTH :: Exp -> Map Name Exp -> Q Exp
rearrExprTH (VarE name)   env =
  case Map.lookup name env of
    Just path -> do Just conEDir <- lookupValueName "EDir"
                    return (ConE conEDir `AppE` path)
rearrExprTH (ConE name)   env = do Just conEConst <- lookupValueName "EConst"
                                   return (ConE conEConst `AppE` ConE name)
rearrExprTH (TupE [e])    env = rearrExprTH e env
rearrExprTH (TupE (e:es)) env = do e0 <- rearrExprTH e env
                                   e1 <- rearrExprTH (TupE es) env
                                   Just conEProd <- lookupValueName "EProd"
                                   return ((ConE conEProd `AppE` e0) `AppE` e1)

rearrExprTH (InfixE (Just l) (ConE mustbecons) (Just r)) env = do
  e0 <- rearrExprTH l env
  e1 <- rearrExprTH r env
  ConE cons <- [|(:)|]
  if cons == mustbecons
    then [| EIn (ERight (EProd $(return e0) $(return e1))) |] -- dangerous. please check whether these constructor exists
    else fail "the infix operator should be ListCons (:)"


rearrTH :: Exp -> Q Exp
rearrTH (LamE [p] e) = do (pat, env) <- rearrPatTH p
                          expr <- rearrExprTH e env
                          Just conRearr <- lookupValueName "Rearr"
                          return ((ConE conRearr `AppE` pat) `AppE` expr)

rearr :: Q Exp -> Q Exp
rearr = (rearrTH =<<)
