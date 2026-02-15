-- | Datatypes for error traces, produced by the standard interpreters in "Generics.BiGUL.Interpreter".

module Generics.BiGUL.Error where

import Generics.BiGUL

import Control.Monad
import Data.List


-- | Execution traces, which log the operations executed, intermediate sources and views, and reasons of eventual failure.
data BiGULTrace = BTSuccess
                    -- ^ Execution successfully produces a result.
                | BTError BiGULError
                    -- ^ Execution fails to produce a result.
                | forall s v. (Show s, Show v) => BTNextSV String s v BiGULTrace
                    -- ^ An intermediate step with the current source and view.
                | forall s. (Show s) => BTNextS String s BiGULTrace
                    -- ^ An intermediate step with the current source.
                | BTBranch Int BiGULTrace
                    -- ^ Inside a branch. /Notes that branch numbering starts from 0./
                | BTBranches [BiGULTrace]
                    -- ^ All branches fail.

instance Show BiGULTrace where
  show  BTSuccess           = "success"
  show (BTError e)          = show e
  show (BTNextSV str s v t) = str ++ "\n" ++
                              "  source = " ++ show s ++ "\n" ++
                              "  view   = " ++ show v ++ "\n" ++ show t
  show (BTNextS  str s   t) = str ++ "\n" ++
                              "  source = " ++ show s ++ "\n" ++ show t
  show (BTBranch i t)       = "inside branch " ++ show i ++ "\n" ++ show t
  show (BTBranches ts)      = "all cases fail\n" ++
                              concat (intersperse "\n" (concat
                                (zipWith (\i t -> ("  branch " ++ show i ++ ":") :
                                                  map ("    " ++) (lines (show t))) [0..] ts)))

-- | Datatype of atomic errors.
data BiGULError = BEFail String
                    -- ^ 'Generics.BiGUL.Fail' is executed.
                | BESkipMismatch
                    -- ^ When executing @Skip f@ on a source @s@, the view is not @f s@.
                | BESourcePatternMismatch PatError
                    -- ^ The source does not match with a pattern.
                | BEViewPatternMismatch PatError
                    -- ^ The view does not match with a pattern.
                | BEInvRearrFailed PatError
                    -- ^ The inverse rearrangement fails.
                | BEViewRecoveringFailed PatError
                    -- ^ The view cannot be reconstructed.
                | BEDependencyMismatch
                    -- ^ When executing @Dep f _@ on a view pair @(v, v')@, the second component @v'@ is not @f v@.
                | BECaseExhausted
                    -- ^ No branch in a 'Generics.BiGUL.Case' statement is applicable.
                | BEAdaptiveBranchRevisited
                    -- ^ After adaptation, execution still goes into an adaptative branch.
                | BEPreviousBranchMatched
                    -- ^ After execution of a branch, the computed source and view
                    --   satisfy the main condition of a previous branch.
                | BEExitConditionFailed
                    -- ^ After execution of a branch, the updated source fails to satisfy the exit condition.
                | BEPostVerificationFailed
                    -- ^ After execution of a branch, the computed source and view do not satisfy the main condition.
                | BEBranchUnmatched
                    -- ^ The main condition is not satisfied by the source and view (and hence the branch is not chosen).
                | BEAdaptiveBranchMatched
                    -- ^ The branch is adaptive and hence ignored.

indent :: String -> String
indent = unlines . map ("  "++) . lines

instance Show BiGULError where
  show (BEFail str)                = "fail statement executed\n" ++ indent str
  show  BESkipMismatch             = "view not determined by the source"
  show (BESourcePatternMismatch e) = "source pattern mismatch\n" ++ indent (show e)
  show (BEViewPatternMismatch e)   = "view pattern mismatch\n" ++ indent (show e)
  show (BEInvRearrFailed e)        = "inverse rearrangement failed\n" ++ indent (show e)
  show (BEViewRecoveringFailed e)  = "view recovering failed\n" ++ indent (show e)
  show  BEDependencyMismatch       = "second view component not determined by the first"
  show  BECaseExhausted            = "case exhausted"
  show  BEAdaptiveBranchRevisited  = "adaptive branch revisited"
  show  BEPreviousBranchMatched    = "previous branch matched after branch execution"
  show  BEExitConditionFailed      = "exit condition not satisfied after branch execution"
  show  BEPostVerificationFailed   = "main condition not satisfied after branch execution"
  show  BEBranchUnmatched          = "main condition not satisfied"
  show  BEAdaptiveBranchMatched    = "adaptive branch ignored"

-- | Pattern matching errors, which also explain where the matching fails.
data PatError = PEConstantMismatch
                  -- ^ A constant pattern/expression is matched with a different value.
              | PELeftMismatch
                  -- ^ A 'Left' pattern/expression is matched with a 'Right' value.
              | PERightMismatch
                  -- ^ A 'Right' pattern/expression is matched with a 'Left' value.
              | PEIncompatibleUpdates
                  -- ^ Occurrences of the same variable in an expression are matched with different values.
              | PEMultipleUpdates
                  -- ^ A variable that should appear at most once in an expression is however used multiple times.
              | PEValueUnrecoverable
                  -- ^ A variable that should appear at least once in an expression is however absent.
              | PEProdL PatError
                  -- ^ The error is on the left of a product pattern/expression.
              | PEProdR PatError
                  -- ^ The error is on the right of a product pattern/expression.
              | PELeft  PatError
                  -- ^ The error is inside a 'Left' pattern/expression.
              | PERight PatError
                  -- ^ The error is inside a 'Right' pattern/expression.
              | PEIn    PatError
                  -- ^ The error is inside a constructor pattern/expression.

instance Show PatError where
  show  PEConstantMismatch    = "matching a constant pattern/expression with a different value"
  show  PELeftMismatch        = "matching a Left pattern/expression with a Right value"
  show  PERightMismatch       = "matching a Right pattern/expression with a Left value"
  show  PEIncompatibleUpdates = "matching occurrences of the same variable with different values"
  show  PEMultipleUpdates     = "multiple occurrences of a variable that can only be used at most once"
  show  PEValueUnrecoverable  = "no occurrences of a variable that should be used at least once"
  show (PEProdL e)            = "on the left of a product pattern/expression\n" ++ show e
  show (PEProdR e)            = "on the right of a product pattern/expression\n" ++ show e
  show (PELeft  e)            = "inside a Left pattern/expression\n" ++ show e
  show (PERight e)            = "inside a Right pattern/expression\n" ++ show e
  show (PEIn    e)            = "inside a constructor pattern/expression\n" ++ show e
