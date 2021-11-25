module SimplifyInterpreter (simplify') where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Language.Fixpoint.Solver.Interpreter (ICtx (..), Knowledge (..))
import qualified Language.Fixpoint.Solver.Interpreter as Interpreter
import Language.Fixpoint.Types.Refinements (Expr)

simplify' :: Expr -> Expr
simplify' = Interpreter.simplify emptyKnowledge emptyICtx
  where
    emptyKnowledge :: Interpreter.Knowledge
    emptyKnowledge =
      Interpreter.KN
        { knSims = M.empty, -- :: M.HashMap (Symbol, Symbol) Rewrite
          knAms = M.empty, -- :: M.HashMap Symbol Equation
          knLams = [], -- :: ![(Symbol, Sort)]
          knSummary = [], -- :: ![(Symbol, Int)]
          knDCs = S.empty, -- :: !(S.HashSet Symbol)
          knAllDCs = S.empty, -- :: !(S.HashSet Symbol)
          knSels = M.empty, -- :: !SelectorMap
          knConsts = M.empty -- :: !ConstDCMap
        }

    emptyICtx :: Interpreter.ICtx
    emptyICtx =
      Interpreter.ICtx
        { icCands = S.empty, -- :: S.HashSet Expr
          icEquals = S.empty, -- :: EvAccum
          icSolved = S.empty, -- :: S.HashSet Expr
          icSimpl = M.empty, -- :: !ConstMap
          icSubcId = Nothing -- :: Maybe SubcId
        }
