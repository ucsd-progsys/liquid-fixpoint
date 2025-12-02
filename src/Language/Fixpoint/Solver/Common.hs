{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.Solver.Common (askSMT, toSMT) where

import Control.Monad.State
import Language.Fixpoint.Types.Config (Config, solver, solverFlags)
import Language.Fixpoint.Smt.Interface (Context(..), checkValidWithContext)
import Language.Fixpoint.Smt.Types (SmtM)
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Visitor (kvarsExpr)
import Language.Fixpoint.Defunctionalize (defuncAny)
import Language.Fixpoint.SortCheck (ElabParam(..), elaborate)
import GHC.Stack (HasCallStack)

mytracepp :: (PPrint a) => String -> a -> a
mytracepp = notracepp

askSMT
  :: HasCallStack
  => Config
  -> [(Symbol, Sort)] -- ^ symbols already declared in the SMT solver
  -> [(Symbol, Sort)] -- ^ symbols to declare in the SMT solver
  -> Expr
  -> SmtM Bool
askSMT cfg bsInSMT xs e
  | isTautoPred  e     = return True
  | null (kvarsExpr e) =
      do ctx <- get
         let e' = toSMT "askSMT" cfg ctx (xs ++ bsInSMT) e
         checkValidWithContext xs PTrue e'
  | otherwise          = return False

toSMT :: HasCallStack => String -> Config -> Context -> [(Symbol, Sort)] -> Expr -> Pred
toSMT msg cfg ctx xs e =
    defuncAny cfg symenv .
        elaborate (ElabParam (solverFlags $ solver cfg) (dummyLoc msg) (elabEnv xs)) .
            mytracepp ("toSMT from " ++ msg ++ " > " ++ showpp e) $
                e
  where
    elabEnv = insertsSymEnv symenv
    symenv  = ctxSymEnv ctx
