{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.Counterexample
  ( tryCounterExample
  ) where

import Language.Fixpoint.Types hiding (Counterexample)
import Language.Fixpoint.Counterexample.Types
import Language.Fixpoint.Counterexample.Build
import Language.Fixpoint.Counterexample.Check
import Language.Fixpoint.Types.Config (Config, counterExample)

import qualified Data.HashMap.Strict as Map

import Control.Monad.IO.Class

-- TODO: Remove variables from the counter example that got mapped to
-- the "wrong" type in smt format (e.g. to an int while not being one).

-- TODO: Ideally `Result` would not have `SubcId` in its generic. Instead, this
-- should just always be contained in a `Result`. Right now, our counterexample
-- will contain a bunch of meaningless `SubcId` as we need to read it from
-- the result.

-- | Try to get a counter example for the given unsafe clauses (if any).
tryCounterExample
  :: (MonadIO m, Fixpoint info)
  => Config
  -> SInfo info
  -> Result (SubcId, info)
  -> m (Result (SubcId, info))
tryCounterExample cfg si res@Result
  { resStatus = Unsafe _ cids'
  , resCounterexamples = cexs'
  } | counterExample cfg = do
    -- Build program from constraints
    prog <- hornToProg cfg si

    -- Check the constraints, returning a substitution map
    let cids = fst <$> cids'
    smtcex <- checkProg cfg si prog cids

    -- Map the symbols in this substitution to their respective bind id
    let cexs = Map.mapWithKey (toFullCex si) smtcex
    return res { resCounterexamples = cexs <> cexs' }
tryCounterExample _ _ res = return res

-- | Extend an SMT counterexample to a full counterexample.
toFullCex :: SInfo info -> SubcId -> SMTCounterexample -> FullCounterexample (SubcId, info)
toFullCex si subcid (Counterexample env trace) = Counterexample
  { cexEnv = substToCexEnv si subcid env
  , cexFrames = Map.mapWithKey (toFullCex si . snd) trace
  }

-- | Extend an SMT counterexample environment (i.e. the substitution map) to a
-- full counterexample environment. With this, the variables are indexed by
-- `BindId` and they contain also their refinement type and user info.
substToCexEnv :: SInfo info -> SubcId -> Subst -> CexEnv (SubcId, info)
substToCexEnv si subcid (Su sub) = benv { beBinds = binds }
  where
    benv = bs si
    ibenv = senv $ cm si Map.! subcid 

    -- Filter out all bind ids that are not in the constraint. Then map the
    -- symbols back to the bind ids.
    binds = Map.mapMaybe trans
          . Map.filterWithKey (\bid _ -> memberIBindEnv bid ibenv)
          . beBinds
          $ benv

    -- Extend the symbol with a its type and user info if possible.
    trans (sym, sreft, info) = extend <$> Map.lookup sym sub
      where
        extend ex = (sym, sreft, (ex, (subcid, info)))
