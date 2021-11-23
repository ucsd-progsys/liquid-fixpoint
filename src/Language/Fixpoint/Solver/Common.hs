{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.Solver.Common (askSMT, toSMT) where

import Language.Fixpoint.Types.Config (Config)
import Language.Fixpoint.Smt.Interface (Context(..), checkValidWithContext)
import Language.Fixpoint.Types
import Language.Fixpoint.Types.Visitor (kvarsExpr)
import Language.Fixpoint.Defunctionalize (defuncAny)
import Language.Fixpoint.SortCheck (elaborate)

mytracepp :: (PPrint a) => String -> a -> a
mytracepp = notracepp

askSMT :: Config -> Context -> [(Symbol, Sort)] -> Expr -> IO Bool
askSMT cfg ctx bs e
--   | isContraPred e  = return False
  | isTautoPred  e     = return True
  | null (kvarsExpr e) = checkValidWithContext ctx [] PTrue e'
  | otherwise          = return False
  where
    e' = toSMT "askSMT" cfg ctx bs e

toSMT :: String -> Config -> Context -> [(Symbol, Sort)] -> Expr -> Pred
toSMT msg cfg ctx bs e =
    defuncAny cfg senv .
        elaborate "makeKnowledge" (elabEnv bs) .
            mytracepp ("toSMT from " ++ msg ++ showpp e) $
                e
  where
    elabEnv = insertsSymEnv senv
    senv    = ctxSymEnv ctx
