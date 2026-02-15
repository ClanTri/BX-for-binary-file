module BiYaccDef where

import GHC.Generics
--import Generics.BiGUL hiding (Expr, Pat)
import Generics.BiGUL.AST hiding (Expr, Pat)
import Generics.BiGUL.TH
import Language.Haskell.TH

data Expr = EAdd Expr Term
          | ESub Expr Term
          | ETerm Term
          | ENull
  deriving (Show, Eq)

data Term = TMul Term Factor
          | TDiv Term Factor
          | TFactor Factor
          | TNull
  deriving (Show, Eq)

data Factor = FNum Int
            | FNeg Factor
            | FExpr Expr
            | FNull
  deriving (Show, Eq)

data Arith = Num Int
           | Add Arith Arith
           | Sub Arith Arith
           | Mul Arith Arith
           | Div Arith Arith
  deriving (Eq, Show)

deriveBiGULGeneric ''Expr
deriveBiGULGeneric ''Term
deriveBiGULGeneric ''Factor
deriveBiGULGeneric ''Arith


--rule :: Q Pat -> Q Pat -> Q Exp -> Q Exp -> Q Exp
--rule vPat sPat upd next =
--  [| CaseS [ $(normal sPat)
--               (CaseV [ $(branch vPat) $(upd),
--                        $(branch [p| _ |]) $(next)
--                      ]),
--             $(normal [p| _ |]) $(next)
--           ]
--  |]

-- rule :: Q Pat -> Q Pat -> Q Exp -> Q Exp -> Q Exp
-- rule vPat sPat upd next =
--   [| CaseV [ $(branch vPat)
--                   (CaseS [ $(normal sPat) $(upd),
--                            $(normal [p| _ |]) $(next)
--                          ]),
--                 $(branch [p| _ |]) $(next)
--               ]
--   |]

--gen vpat cons parcount =
--  if vpat == [| _ |]
--     then [| _ |]
--     else gen' cons parcount
--  where
--    gen' cons 0 = [| \a -> a |]
--    gen' cons 1 = [| \( cons x)  -> x |]
--    gen' cons 2 = [| \( cons x y)  -> (x,y) |]


-- adaptation :: Q Pat -> Q Pat -> Q Exp -> Q Exp -> Q Exp
-- adaptation vPat sPat upd new =
--   [| $(branch vPat)
--        (CaseS [ $(normal sPat) $(upd),
--                 $(adaptive [p| _ |]) (\_ _ -> return $(new))
--               ])
--   |]
