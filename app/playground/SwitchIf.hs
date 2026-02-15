{-# LANGUAGE TypeFamilies #-}

module SwitchIf where

import GHC.Generics
import Generics.BiGUL.AST
import Generics.BiGUL.TH
import Generics.BiGUL.Interpreter

type Var = String

data StmtS = SwitchS Var [(Int, StmtS)]
           | IfS Var Int StmtS StmtS
           | SkipS
           deriving (Show, Eq)

deriveBiGULGeneric ''StmtS

data StmtV = IfV Var Int StmtV StmtV
           | SkipV
           deriving Show

deriveBiGULGeneric ''StmtV

sync :: BiGUL StmtS StmtV
sync = Case
  [ $(normalSV [p| SkipS |] [p| SkipV |])$
      $(update [p| SkipV |] [p| _ |] [d| |])
  , $(normalSV [p| IfS _ _ _ _ |] [p| IfV _ _ _ _ |])$
      $(update [p| IfV var constant tStmt eStmt |] [p| IfS var constant tStmt eStmt |]
               [d| var = Replace; constant = Replace; tStmt = sync; eStmt = sync |])
  , $(normalSV [p| SwitchS _ [] |] [p| SkipV |])$
      $(update [p| SkipV |] [p| _ |] [d| |])
  , $(adaptiveV [p| SkipV |])$
      \_ _ -> SkipS
  , $(normalSV [p| SwitchS _ (_:_) |] [p| IfV _ _ _ _ |])$
      $(rearrS [| \(SwitchS var bs) -> (var, bs) |])$
        syncBranches
  , $(adaptiveV [p| IfV _ _ _ _ |])$
      \_ (IfV var constant _ _) -> IfS var constant SkipS SkipS
  ]

syncBranches :: BiGUL (Var, [(Int, StmtS)]) StmtV
syncBranches = Case
  [ $(normalSV [p| (_, []) |] [p| SkipV |])$  -- variable name allowed?
      $(rearrV [| \SkipV -> () |])$ Skip
  , $(adaptiveV [p| SkipV |])$
      \(sVar, _) _ -> (sVar, [])
  , $(normal' [| \s v -> case (s, v) of
                           ((sVar, (sConst, _):_), IfV vVar vConst _ _) -> sVar == vVar && sConst == vConst
                           _ -> False |]
              [p| (_, _:_) |])$
      $(rearrS [| \(sVar, (sConst, stmt):stmts) -> ((sVar, sConst), stmt, (sVar, stmts)) |])$
        $(rearrV [| \(IfV vVar vConst tStmt eStmt) -> ((vVar, vConst), tStmt, eStmt) |])$
          Replace `Prod` sync `Prod` syncBranches
  , $(adaptive [| \_ _ -> True |])$
      \(sVar, bs) (IfV _ vConst _ _) -> (sVar, dropWhile ((/= vConst) . fst) bs)
  ]

testS :: StmtS
testS = SwitchS "x" [(0, IfS "y" 0 SkipS SkipS), (1, SkipS), (2, SkipS)]

testV :: StmtV
testV = IfV "x" 0 SkipV (IfV "x" 2 (IfV "y" 0 SkipV SkipV)SkipV)
