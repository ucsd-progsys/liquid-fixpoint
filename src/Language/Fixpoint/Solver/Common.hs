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

mytracepp :: (PPrint a) => String -> a -> a
mytracepp = notracepp

askSMT :: Config -> [(Symbol, Sort)] -> Expr -> SmtM Bool
askSMT cfg xs e
--   | isContraPred e  = return False
  | isTautoPred  e     = return True
  | null (kvarsExpr e) =
      do ctx <- get
         let e' = toSMT "askSMT" cfg ctx xs e
         checkValidWithContext xs PTrue e'
  | otherwise          = return False

toSMT :: String -> Config -> Context -> [(Symbol, Sort)] -> Expr -> Pred
toSMT msg cfg ctx xs e =
    defuncAny cfg symenv .
        elaborate (ElabParam (solverFlags $ solver cfg) (dummyLoc msg) (elabEnv xs)) .
            mytracepp ("toSMT from " ++ msg ++ " > " ++ showpp e) $
                e
  where
    elabEnv = insertsSymEnv symenv
    symenv  = ctxSymEnv ctx
