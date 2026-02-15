-- | This module implements the rearrangement operations, which are based on pattern matching.

module Generics.BiGUL.PatternMatching where

import Generics.BiGUL
import Generics.BiGUL.Error

import GHC.InOut

import Control.Monad.Except


modifyError :: (e -> e) -> Either e a -> Either e a
modifyError f = either (Left . f) Right

deconstruct :: Pat a env con -> a -> Either PatError env
deconstruct PVar          x         = return (Var x)
deconstruct PVar'         x         = return (Var x)
deconstruct (PConst c)    x         = if c == x then return () else throwError PEConstantMismatch
deconstruct (l `PProd` r) (x, y)    = liftM2 (,) (modifyError PEProdL (deconstruct l x))
                                                 (modifyError PEProdR (deconstruct r y))
deconstruct (PLeft  p)    (Left  x) = modifyError PELeft  (deconstruct p x)
deconstruct (PLeft  _)    _         = throwError PELeftMismatch
deconstruct (PRight p)    (Right x) = modifyError PERight (deconstruct p x)
deconstruct (PRight _)    _         = throwError PERightMismatch
deconstruct (PIn p)       x         = modifyError PEIn    (deconstruct p (out x))

construct :: Pat a env con -> env -> a
construct PVar          (Var x) = x
construct PVar'         (Var x) = x
construct (PConst c)    _       = c
construct (l `PProd` r) (x, y)  = (construct l x, construct r y)
construct (PLeft  p)    x       = Left  (construct p x)
construct (PRight p)    x       = Right (construct p x)
construct (PIn p)       x       = inn (construct p x)

retrieve :: Direction env a -> env -> a
retrieve  DVar      (Var x ) = x
retrieve (DLeft  d) (env, _) = retrieve d env
retrieve (DRight d) (_, env) = retrieve d env

eval :: Expr env a -> env -> a
eval (EDir d)      env = retrieve d env
eval (EConst c)    env = c
eval (l `EProd` r) env = (eval l env, eval r env)
eval (ELeft  e)    env = Left  (eval e env)
eval (ERight e)    env = Right (eval e env)
eval (EIn e)       env = inn (eval e env)

uneval :: Pat a env con -> Expr env b -> b -> con -> Either PatError con
uneval p (EDir d)     x         con = unevalDir p d x con
uneval p (EConst c)   x         con = if c == x then return con else throwError PEConstantMismatch
uneval p (EProd l r)  (x, y)    con = modifyError PEProdL (uneval p l x con) >>= modifyError PEProdR . uneval p r y
uneval p (ELeft  e)   (Left  x) con = modifyError PELeft  (uneval p e x con)
uneval p (ELeft  _)   x         con = throwError PELeftMismatch
uneval p (ERight e)   (Right x) con = modifyError PERight (uneval p e x con)
uneval p (ERight _)   x         con = throwError PERightMismatch
uneval p (EIn e)      x         con = modifyError PEIn    (uneval p e (out x) con)

unevalDir :: Pat a env con -> Direction env b -> b -> con -> Either PatError con
unevalDir PVar          DVar       x (Just y)     = if x == y
                                                    then return (Just x)
                                                    else throwError PEIncompatibleUpdates
unevalDir PVar          DVar       x Nothing      = return (Just x)
unevalDir PVar'         DVar       x (Just y)     = throwError PEMultipleUpdates
unevalDir PVar'         DVar       x Nothing      = return (Just x)
unevalDir (PConst c)    _          x con          = return con
unevalDir (l `PProd` r) (DLeft  d) x (conl, conr) = liftM (, conr) (modifyError PEProdL (unevalDir l d x conl))
unevalDir (l `PProd` r) (DRight d) x (conl, conr) = liftM (conl ,) (modifyError PEProdR (unevalDir r d x conr))
unevalDir (PLeft  p)    d          x con          = modifyError PELeft  (unevalDir p d x con)
unevalDir (PRight p)    d          x con          = modifyError PERight (unevalDir p d x con)
unevalDir (PIn p)       d          x con          = modifyError PEIn    (unevalDir p d x con)

fromContainerV :: Pat v env con -> con -> Either PatError env
fromContainerV PVar          Nothing      = throwError PEValueUnrecoverable
fromContainerV PVar          (Just v)     = return (Var v)
fromContainerV PVar'         Nothing      = throwError PEValueUnrecoverable
fromContainerV PVar'         (Just v)     = return (Var v)
fromContainerV (PConst c)    con          = return ()
fromContainerV (l `PProd` r) (conl, conr) = liftM2 (,) (modifyError PEProdL (fromContainerV l conl))
                                                       (modifyError PEProdR (fromContainerV r conr))
fromContainerV (PLeft  p)    con          = modifyError PELeft  (fromContainerV p con)
fromContainerV (PRight p)    con          = modifyError PERight (fromContainerV p con)
fromContainerV (PIn p)       con          = modifyError PEIn    (fromContainerV p con)

fromContainerS :: Pat s env con -> env -> con -> env
fromContainerS PVar          (Var s)      Nothing      = (Var s )
fromContainerS PVar          (Var s)      (Just s')    = (Var s')
fromContainerS PVar'         (Var s)      Nothing      = (Var s )
fromContainerS PVar'         (Var s)      (Just s')    = (Var s')
fromContainerS (PConst c)    _            _            = ()
fromContainerS (l `PProd` r) (envl, envr) (conl, conr) = (fromContainerS l envl conl, fromContainerS r envr conr)
fromContainerS (PLeft  p)    env          con          = fromContainerS p env con
fromContainerS (PRight p)    env          con          = fromContainerS p env con
fromContainerS (PIn p)       env          con          = fromContainerS p env con

emptyContainer :: Pat v env con -> con
emptyContainer PVar          = Nothing
emptyContainer PVar'         = Nothing
emptyContainer (PConst c)    = ()
emptyContainer (l `PProd` r) = (emptyContainer l, emptyContainer r)
emptyContainer (PLeft  p)    = emptyContainer p
emptyContainer (PRight p)    = emptyContainer p
emptyContainer (PIn p)       = emptyContainer p
