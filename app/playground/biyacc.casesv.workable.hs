import GHC.Generics
--import Generics.BiGUL hiding (Expr, Pat)
import Generics.BiGUL.AST hiding (Expr, Pat)
import Generics.BiGUL.Interpreter hiding (Expr, Pat)
import Generics.BiGUL.TH
import Generics.BiGUL.Error
import Language.Haskell.TH
import BiYaccDef

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad


tc1 = (ETerm (TFactor (FExpr (ETerm (TFactor (FExpr (EAdd (ETerm (TFactor (FNum 3))) (TFactor (FNum 3)) )  ))))))

testPut :: BiGUL s v -> s -> v -> s
testPut u s v = either (error . show) id (put u s v)

testGet :: BiGUL s v -> s -> v
testGet u s = either (error . show) id (get u s)

ruleExprArith :: BiGUL Expr Arith
ruleExprArith =
  Case  [ $(normalSV [p| EAdd _ _ |] [p| Add _ _ |] )
            $(update [p| Add l r |] [p| EAdd l r |]
                             [d| l = ruleExprArith; r = ruleTermArith |])
        , $(adaptiveSV [p| ETerm (TFactor (FExpr (ETerm (TFactor (FExpr (EAdd _ _)))))) |]
                       [p| Add _ _ |] )
                       (\s _ -> case s of ETerm (TFactor (FExpr a)) ->  a)
        , $(normalSV [p| ESub _ _ |] [p| Sub _ _ |] )
            $(update [p| Sub l r |] [p| ESub l r |]
                             [d| l = ruleExprArith; r = ruleTermArith |])
        , $(adaptiveSV [p| ETerm (TFactor (FExpr (ETerm (TFactor (FExpr (ESub _ _)))))) |]
                       [p| Sub _ _ |] )
                       (\s _ -> case s of ETerm (TFactor (FExpr a)) ->  a)
        , $(normalSV [p| ETerm _ |] [p|  _ |] )
            $(update [p| a  |] [p| ETerm a |]
                             [d| a = ruleTermArith |])
        , $(adaptiveV [p| Add _ _|]) (\_ _ -> EAdd ENull TNull)
        , $(adaptiveV [p| Sub _ _|]) (\_ _ -> ESub ENull TNull)
        , $(adaptiveV [p| _ |])      (\_ _ -> ETerm TNull)
       ]

--
ruleTermArith :: BiGUL Term Arith
ruleTermArith =
  Case [ $(normalSV [p| TMul _ _ |] [p| Mul _ _ |] )
           $(update [p| Mul l r |] [p| TMul l r |]
                            [d| l = ruleTermArith; r = ruleFactorArith |])
       , $(normalSV [p| TDiv _ _ |] [p| Div _ _ |] )
           $(update [p| Div l r |] [p| TDiv l r |]
                            [d| l = ruleTermArith; r = ruleFactorArith |])
       , $(normalSV [p| TFactor _ |] [p|  _ |] )
           $(update [p| a  |] [p| TFactor a |]
                            [d| a = ruleFactorArith |])
       , $(adaptiveV [p| Mul _ _|]) (\_ _ -> TMul TNull FNull )
       , $(adaptiveV [p| Div _ _|]) (\_ _ -> TDiv TNull FNull )
       , $(adaptiveV [p| _ |])      (\_ _ -> TFactor FNull)
       ]

ruleFactorArith :: BiGUL Factor Arith
ruleFactorArith =
  Case [ $(normalSV [p| FNeg _ |] [p| Sub (Num 0) _ |] )
           $(update [p| Sub (Num 0) n |] [p| FNeg n |]
                            [d| n = ruleFactorArith |])
       , $(normalSV [p| FNum _ |] [p| Num _ |] )
           $(update [p| Num n |] [p| FNum n |]
                            [d| n = Replace |])
       , $(normalSV [p| FExpr _ |] [p|  _ |] )
           $(update [p| a  |] [p| FExpr a |]
                            [d| a = ruleExprArith |])
       , $(adaptiveV [p| Sub (Num 0) _ |]) (\_ _ -> FNeg FNull)
       , $(adaptiveV [p| Num _|])          (\_ _ -> FNum 0 )
       , $(adaptiveV [p| _ |])             (\_ _ -> FExpr ENull)
       ]

-------------- quick check ---------------
-- quick check


-- putget bigul s v = do
--   s' <- testPut bigul s v
--   v' <- testGet bigul s'
--   return $ if v == v' then True else False


-- prop_GetPut :: (BiGUL Expr Arith) -> Expr -> Property
prop_GetPut s =
  let v  = testGet ruleExprArith s
      s' = testPut ruleExprArith s v
  in  s == s'
  where types = s :: Expr
-- prop_GetPut s = monadic runBigulM $ do
--   v <- run $ testGet ruleExprArith0 s
--   s' <- run $ testPut ruleExprArith0 s v
--   assert $ s == s'
--   where types = s :: Expr


prop_PutGet s v =
  let s' = testPut ruleExprArith s v
      v' = testGet ruleExprArith s'
  in  v == v'
  where types = (s :: Expr, v :: Arith)
-- prop_PutGet s v = monadic runBigulM $ do
--   s' <- run $ testPut ruleExprArith0 s v
--   v' <- run $ testGet ruleExprArith0 s'
--   assert $ v == v'
--   where types = (s :: Expr, v :: Arith)


-- runBigulM ma =
--   case ma of
--     Left _ -> error "cuole"
--     Right a -> a


-----------
cexprg = sized cexprg'

cexprg' 0 = liftM ETerm (ctermg' 0)
cexprg' n | n > 0 =
  oneof [liftM2 EAdd subcexprg subctermg
        ,liftM2 ESub subcexprg subctermg
        ,liftM ETerm subctermg]
  where
    subcexprg = cexprg' (n `div` 2)
    subctermg = ctermg' (n `div` 2)

ctermg = sized ctermg'

ctermg' 0 = liftM TFactor (cfactorg' 0)
ctermg' n | n > 0 =
  oneof [liftM2 TMul subctermg subcfactorg
        ,liftM2 TDiv subctermg subcfactorg
        ,liftM TFactor subcfactorg]
  where
    subctermg = ctermg' (n `div` 2)
    subcfactorg = cfactorg' (n `div` 2)

cfactorg = sized cfactorg'

cfactorg' 0 = liftM FNum arbitrary
cfactorg' n | n>0 =
  oneof [liftM FNeg subcfactorg
        ,liftM FExpr subcexprg
        ,liftM FNum arbitrary]
  where
    subcfactorg = cfactorg' (n `div` 2)
    subcexprg = cexprg' (n `div` 2)

----

arithg = sized arithg'

arithg' 0 = liftM Num arbitrary
arithg' n | n > 0 =
  oneof [liftM2 Add subArithg subArithg
        ,liftM2 Sub subArithg subArithg
        ,liftM2 Mul subArithg subArithg
        ,liftM2 Div subArithg subArithg
        ,liftM Num arbitrary]
  where
    subArithg = arithg' (n `div` 2)

instance Arbitrary Expr where
  arbitrary = cexprg

instance Arbitrary Arith where
  arbitrary = arithg


-----------------


--
testArgs :: Args
testArgs = stdArgs {maxSuccess=100}

fire1 = verboseCheckWith testArgs prop_GetPut
fire2 = verboseCheckWith testArgs prop_PutGet


