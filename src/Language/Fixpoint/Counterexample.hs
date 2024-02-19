{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.Counterexample
  ( tryCounterExample
  ) where

import Language.Fixpoint.Types
import Language.Fixpoint.Counterexample.Types
import Language.Fixpoint.Counterexample.Build
import Language.Fixpoint.Counterexample.Check
import Language.Fixpoint.Types.Config (Config, counterExample)

import qualified Data.HashMap.Strict as Map

import Control.Monad.IO.Class
import Control.Monad (void)

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
    let cids = map fst cids'
    smtcex <- checkProg cfg si prog cids

    -- Map the symbols in this substitution to their respective bind id
    let cexs = toFullCex si <$> smtcex
    dbg $ fmap (fmap (fmap (fmap void))) <$> cexs
    return res { resCounterexamples = cexs <> cexs' }
tryCounterExample _ _ res = return res

-- | Extend an SMT counterexample to a full counterexample.

-- With this, the variables are indexed by
-- `BindId` and they contain also their refinement type and user info.
toFullCex :: SInfo info -> SMTCounterexample -> Counterexample (SubcId, info)
toFullCex si = fmap $ substToCexEnv si

-- | Extend an SMT counterexample environment (i.e. the substitution map) to a
-- full counterexample environment. With this, the variables are indexed by
-- `BindId` and they contain also their refinement type and user info.
substToCexEnv :: SInfo info -> Subst -> CexEnv (SubcId, info)
substToCexEnv si (Su sub) = benv { beBinds = binds }
  where
    benv = bs si

    binds = Map.mapMaybe trans $ beBinds benv

    trans (sym, sreft, info) = extend <$> Map.lookup sym sub
      where
        -- We fake a SubCId here. It really shouldn't be here, but it is
        -- an artifact of a SubcId needing to be embedded in the generic of
        -- `Result`! Ideally, we would have the CexEnv contain just the same
        -- generic as SInfo. This would require us to change the structure of
        -- `Result` to contain the SubcId always.
        extend ex = (sym, sreft, (ex, (0, info)))

-- TODO: The bindings don't completely match yet. Try out
-- tests/neg/duplicate-names3.fq
--
-- There you can see that we get 2 bindings for z, while both k instances only
-- should get 1!
--
-- We should use the IBindEnv of every separate horn clause (look at Build.hs
-- on how to get the IBindEnv). With this local bind set, we can get a correct
-- environment!

-- TODO: Make the `Counterexample` structure a tree instead of a hashmap with
-- lists.

