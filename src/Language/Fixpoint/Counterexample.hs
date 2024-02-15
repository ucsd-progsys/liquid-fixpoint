{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fixpoint.Counterexample
  ( tryCounterExample
  ) where

import Language.Fixpoint.Types
import Language.Fixpoint.Counterexample.Types
import Language.Fixpoint.Counterexample.Build
import Language.Fixpoint.Counterexample.Check
import Language.Fixpoint.Types.Config (Config, counterExample)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Control.Monad.IO.Class

-- TODO: Remove variables from the counter example that got mapped to
-- the "wrong" type in smt format (e.g. to an int while not being one).

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
    subs <- checkProg cfg si prog cids

    -- Map the symbols in this substitution to their respective bind id
    let cexs = toFullCex si <$> subs
    return res { resCounterexamples = cexs <> cexs' }
tryCounterExample _ _ res = return res

-- | Map a counter example to use the BindId instead of the
-- variable name as the key.
--
-- In other words, we go from a mapping of Symbol |-> Expr to
-- BindId |-> Expr
toFullCex :: forall info. SInfo info -> SMTCounterexample -> Counterexample (SubcId, info)
toFullCex si smtcex = extendSubst <$> pathcex
  where
    -- Get the bind environment
    benv = bs si

    -- Remove all of the map except the bind id.
    bindings :: BindMap Symbol
    bindings = (\(bid, _, _) -> bid) <$> beBinds benv

    -- Reverses the direction of a hashmap
    reverseMap = Map.fromList . fmap (\(a, b) -> (b, a)) . Map.toList

    -- Reverse the bindings mapping, so we can map our symbols to bind ids.
    symIds :: HashMap Symbol BindId
    symIds = reverseMap bindings

    -- A counterexample where we transformed the path to be [BindId].
    -- I.e. this is a partial translation from SMT Counterexample to a full one.
    pathcex :: HashMap [BindId] Subst
    pathcex = Map.mapKeys (map $ (Map.!) symIds) smtcex

    -- Maps an smt subst to a full counterexample environment (i.e. the
    -- concrete instances for a given scope).
    extendSubst :: Subst -> CexEnv (SubcId, info)
    extendSubst (Su sub) = benv { beBinds = binds }
      where
        binds = Map.mapMaybe trans $ beBinds benv

        trans (sym, sreft, info) = extend <$> Map.lookup sym sub
          where
            -- We fake a SubCId here. It really shouldn't be here, but it is an
            -- artifact of this being embedded in the generic of `Result`
            -- instead of always being there!
            extend ex = (sym, sreft, (ex, (0, info)))

