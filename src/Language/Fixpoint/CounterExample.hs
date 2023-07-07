{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.CounterExample
  ( tryCounterExample
  ) where

import Language.Fixpoint.Types
import Language.Fixpoint.CounterExample.Types
import Language.Fixpoint.CounterExample.Build
import Language.Fixpoint.CounterExample.Check
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
  , resCntExs = cexs'
  } | counterExample cfg = do
    let cids = map fst cids'
    prog <- hornToProg cfg si
    subs <- checkProg cfg si prog cids
    let cexs = cexBindIds si <$> subs
    return res { resCntExs = cexs <> cexs' }
tryCounterExample _ _ res = return res

-- | Map a counter example to use the BindId instead of the
-- variable name as the key.
--
-- In other words, we go from a mapping of Symbol |-> Expr to
-- BindId |-> Expr
cexBindIds :: SInfo info -> CounterExample -> HashMap [BindId] (BindMap Expr)
cexBindIds si cex = Map.mapKeys (map $ (Map.!) symIds) inner
  where
    -- Inner mappings are changed, but the traces aren't yet
    inner :: HashMap [Symbol] (BindMap Expr)
    inner = (\(Su sub) -> Map.compose sub bindings) <$> cex

    -- Fetch a map of all the available bindings
    bindings :: HashMap BindId Symbol
    bindings = fst' <$> beBinds (bs si)
    fst' (sym, _, _) = sym

    -- Reverse the bindings mapping, so we can map our symbols to bind ids.
    symIds :: HashMap Symbol BindId
    symIds = Map.fromList $ (\(sym, bid) -> (bid, sym)) <$> Map.toList bindings