module SimplifyPLE (simplify') where

import qualified Data.HashMap.Strict as SM
import qualified Data.HashSet as S
import qualified Data.Map as M
import Language.Fixpoint.Solver.PLE (FuelCount (..), ICtx (..), Knowledge (..))
import qualified Language.Fixpoint.Solver.PLE as PLE
import Language.Fixpoint.Types.Refinements (Expr)

simplify' :: Expr -> Expr
simplify' = PLE.simplify emptyKnowledge emptyICtx
  where
    emptyKnowledge :: PLE.Knowledge
    emptyKnowledge =
      -- @PLE.simplify@ does not actually use all these fields, so we can get
      -- away with leaving some of them @undefined@.
      KN
        { knSims = M.empty, -- :: Map Symbol [Rewrite]
          knAms = M.empty, -- :: Map Symbol Equation
          knContext = undefined, -- :: SMT.Context
          knPreds = undefined, -- :: SMT.Context -> [(Symbol, Sort)] -> Expr -> IO Bool
          knLams = [], -- :: ![(Symbol, Sort)]
          knSummary = [], -- :: ![(Symbol, Int)]
          knDCs = S.empty, -- :: !(S.HashSet Symbol)
          knSels = [], -- :: !SelectorMap
          knConsts = [], -- :: !ConstDCMap
          knAutoRWs = SM.empty, -- :: M.HashMap SubcId [AutoRewrite]
          knRWTerminationOpts = undefined -- :: RWTerminationOpts
        }

    emptyICtx :: PLE.ICtx
    emptyICtx =
      ICtx
        { icAssms = S.empty, -- S.HashSet Pred
          icCands = S.empty, -- :: S.HashSet Expr
          icEquals = S.empty, -- :: EvAccum
          icSolved = S.empty, -- :: S.HashSet Expr
          icSimpl = SM.empty, -- :: !ConstMap
          icSubcId = Nothing, -- :: Maybe SubcId
          icFuel = emptyFuelCount, -- :: !FuelCount
          icANFs = S.empty -- :: S.HashSet Pred
        }

    emptyFuelCount :: PLE.FuelCount
    emptyFuelCount =
      FC
        { fcMap = SM.empty, -- :: M.HashMap Symbol Int
          fcMax = Nothing -- :: Maybe Int
        }
