-- | The standard interpreters, which perform all dynamic checks to ensure well-behavedness
--   and produce trace information when execution fails.
--   Currently, tracing is designed for debugging, and only the traces leading to failure
--   can be expected to contain a complete log of the steps executed.
--   In other words, traces leading to success usually contain only partial tracing information.
--   Also, when a program loops, there is no guarantee that the trace is computed productively.
--   Finally, /note that branch numbering starts from 0./

module Generics.BiGUL.Interpreter (put, putTrace, get, getTrace) where

import Generics.BiGUL
import Generics.BiGUL.Error
import Generics.BiGUL.PatternMatching

import Control.Monad


-- | A 'Maybe' monad with/modulo trace information —
--   the monad laws hold only when the trace component is /not/ considered.
newtype BiGULResult a = BiGULResult { runBiGULResult :: (Maybe a, BiGULTrace) }

instance Functor BiGULResult where
    fmap = liftM

instance Applicative BiGULResult where
  pure x = BiGULResult (Just x, BTSuccess)
  (<*>)  = ap

instance Monad BiGULResult where
  return = pure
  BiGULResult (Just x , t) >>= f = f x
  BiGULResult (Nothing, t) >>= f = BiGULResult (Nothing, t)

-- Auxiliary functions for 'BiGULResult'.

catchBind :: BiGULResult a -> (a -> BiGULResult b) -> (BiGULTrace -> BiGULResult b) -> BiGULResult b
catchBind (BiGULResult (Just x , _)) f g = f x
catchBind (BiGULResult (Nothing, t)) f g = g t

errorResult :: BiGULError -> BiGULResult a
errorResult e = BiGULResult (Nothing, BTError e)

modifyTrace :: (BiGULTrace -> BiGULTrace) -> BiGULResult a -> BiGULResult a
modifyTrace f (BiGULResult ~(mx, t)) = BiGULResult (mx, f t)

embedEither :: (e -> BiGULError) -> Either e a -> BiGULResult a
embedEither f = either (errorResult . f) return

incrBranchNo :: BiGULTrace -> BiGULTrace
incrBranchNo (BTBranch i t) = BTBranch (i+1) t
incrBranchNo t              = t

addCurrentBranchTrace :: BiGULTrace -> BiGULTrace -> BiGULTrace
addCurrentBranchTrace t (BTBranches ts) = BTBranches (t:ts)
addCurrentBranchTrace t u               = u

-- | The putback semantics of a 'Generics.BiGUL.BiGUL' program.
put :: BiGUL s v -> s -> v -> Maybe s
put b s v = fst (runBiGULResult (putWithTrace b s v))

-- | The execution trace of a 'put' computation.
putTrace :: BiGUL s v -> s -> v -> BiGULTrace
putTrace b s v = snd (runBiGULResult (putWithTrace b s v))

putWithTrace :: BiGUL s v -> s -> v -> BiGULResult s
putWithTrace (Fail str)         s       v       = errorResult (BEFail str)
putWithTrace (Skip f)           s       v       = if f s == v then return s else errorResult BESkipMismatch
putWithTrace  Replace           s       v       = return v
putWithTrace (l `Prod` r)       (s, s') (v, v') =
  liftM2 (,) (modifyTrace (BTNextSV "on the left of Prod"  s  v ) (putWithTrace l s  v ))
             (modifyTrace (BTNextSV "on the right of Prod" s' v') (putWithTrace r s' v'))
putWithTrace (RearrS p e b)     s       v       = do
  env <- embedEither BESourcePatternMismatch (deconstruct p s)
  let m = eval e env
  s'  <- modifyTrace (BTNextSV "inside RearrS" m v) (putWithTrace b m v)
  con <- embedEither BEInvRearrFailed (uneval p e s' (emptyContainer p))
  return (construct p (fromContainerS p env con))
putWithTrace (RearrV p e b)     s       v       = do
  v' <- embedEither BEViewPatternMismatch (deconstruct p v)
  let m = eval e v'
  modifyTrace (BTNextSV "inside RearrV" s m) (putWithTrace b s m)
putWithTrace (Dep f b)          s       (v, v') =
  if f v == v' then modifyTrace (BTNextSV "inside Dep" s v) (putWithTrace b s v)
               else errorResult BEDependencyMismatch
putWithTrace (Case bs)          s       v       = putCase bs s v
putWithTrace (l `Compose` r)    s       v       = do
  m  <- modifyTrace (BTNextS "computing intermediate source" s) (getWithTrace l s)
  m' <- modifyTrace (BTNextSV "on the right of Compose" m v) (putWithTrace r m v)
  modifyTrace (BTNextSV "on the left of Compose" s m') (putWithTrace l s m')
putWithTrace (Checkpoint str b) s    v          =
  modifyTrace (BTNextSV ("checkpoint: " ++ str) s v) (putWithTrace b s v)

getCaseBranch :: (s -> v -> Bool, CaseBranch s v) -> s -> BiGULResult v
getCaseBranch (p, Normal b q) s =
  if q s
  then do v <- getWithTrace b s
          if p s v
          then return v
          else errorResult BEPostVerificationFailed
  else errorResult BEBranchUnmatched
getCaseBranch (p, Adaptive f) s = errorResult BEAdaptiveBranchMatched

putCaseCheckDiversion :: [(s -> v -> Bool, CaseBranch s v)] -> s -> v -> BiGULResult ()
putCaseCheckDiversion []             s v = return ()
putCaseCheckDiversion (pb@(p, b):bs) s v =
  if not (p s v)
  then catchBind (getCaseBranch pb s) (const (errorResult BEPreviousBranchMatched))
                                      (const (putCaseCheckDiversion bs s v))
  else errorResult BEPreviousBranchMatched

putCaseWithAdaptation :: [(s -> v -> Bool, CaseBranch s v)] -> [(s -> v -> Bool, CaseBranch s v)] ->
                         s -> v -> (s -> BiGULResult s) -> BiGULResult s
putCaseWithAdaptation []             bs' s v cont = errorResult BECaseExhausted
putCaseWithAdaptation (pb@(p, b):bs) bs' s v cont =
  if p s v
  then modifyTrace (BTBranch 0) $
       case b of
         Normal b q -> do
           s' <- putWithTrace b s v
           if p s' v
           then if q s'
                then putCaseCheckDiversion bs' s' v >> return s'
                else errorResult BEExitConditionFailed
           else errorResult BEPostVerificationFailed
         Adaptive f -> cont (f s v)
  else modifyTrace incrBranchNo (putCaseWithAdaptation bs (pb:bs') s v cont)

putCase :: [(s -> v -> Bool, CaseBranch s v)] -> s -> v -> BiGULResult s
putCase bs s v = putCaseWithAdaptation bs [] s v
                   (\s' -> putCaseWithAdaptation bs [] s' v
                             (const (errorResult BEAdaptiveBranchRevisited)))

-- | The get semantics of a 'Generics.BiGUL.BiGUL' program.
get :: BiGUL s v -> s -> Maybe v
get b s = fst (runBiGULResult (getWithTrace b s))

-- | The execution trace of a 'get' computation.
getTrace :: BiGUL s v -> s -> BiGULTrace
getTrace b s = snd (runBiGULResult (getWithTrace b s))

getWithTrace :: BiGUL s v -> s -> BiGULResult v
getWithTrace (Fail str)         s       = errorResult (BEFail str)
getWithTrace (Skip f)           s       = return (f s)
getWithTrace  Replace           s       = return s
getWithTrace (l `Prod` r)       (s, s') =
  liftM2 (,) (modifyTrace (BTNextS "on the left of Prod"  s ) (getWithTrace l s ))
             (modifyTrace (BTNextS "on the right of Prod" s') (getWithTrace r s'))
getWithTrace (RearrS p e b)     s       = do
  env <- embedEither BESourcePatternMismatch (deconstruct p s)
  let m = eval e env
  modifyTrace (BTNextS "inside RearrS" m) (getWithTrace b m)
getWithTrace (RearrV p e b)     s       = do
  v'  <- modifyTrace (BTNextS "inside RearrV" s) (getWithTrace b s)
  con <- embedEither BEInvRearrFailed (uneval p e v' (emptyContainer p))
  env <- embedEither BEViewRecoveringFailed (fromContainerV p con)
  return (construct p env)
getWithTrace (Dep f b)          s       = do
  v <- modifyTrace (BTNextS "inside Dep" s) (getWithTrace b s)
  return (v, f v)
getWithTrace (Case bs)          s       = getCase bs s
getWithTrace (l `Compose` r)    s       = do
  m <- modifyTrace (BTNextS "on the left of Compose" s) (getWithTrace l s)
  modifyTrace (BTNextS "on the right of Compose" m) (getWithTrace r m)
getWithTrace (Checkpoint str b) s       =
  modifyTrace (BTNextS ("checkpoint: " ++ str) s) (getWithTrace b s)

getCase :: [(s -> v -> Bool, CaseBranch s v)] -> s -> BiGULResult v
getCase []             s =  BiGULResult (Nothing, BTBranches [])
getCase (pb@(p, b):bs) s =
  catchBind (getCaseBranch pb s) return
            (\t -> do v <- modifyTrace (addCurrentBranchTrace t) (getCase bs s)
                      if not (p s v)
                      then return v
                      else modifyTrace (BTBranch 0) (errorResult BEPreviousBranchMatched))
