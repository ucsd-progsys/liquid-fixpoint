{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE BangPatterns         #-}

module Language.Fixpoint.Solver.Eliminate
       (eliminateAll) where

import           Language.Fixpoint.Types
import           Language.Fixpoint.Types.Visitor   (kvars)
import           Language.Fixpoint.Solver.Deps     (depNonCuts, deps)
import           Language.Fixpoint.Misc            (errorstar)
import           Language.Fixpoint.Solver.Solution (Solution, mkJVar)

import qualified Data.HashMap.Strict as M
import           Data.List           (foldl')
import           Control.Arrow       (first)
import           Control.DeepSeq     (($!!))


--------------------------------------------------------------
eliminateAll :: SInfo a -> (Solution, SInfo a)
eliminateAll !fi = {-# SCC "eliminateAll" #-} foldl' eliminate (M.empty, fi) nonCuts
  where
    nonCuts = depNonCuts $ deps fi
--------------------------------------------------------------

eliminate :: (Solution, SInfo a) -> KVar -> (Solution, SInfo a)
eliminate (!s, !fi) k = (M.insert k (mkJVar orPred) s, fi { cm = remainingCs , ws = M.delete k $ ws fi })
  where
    relevantCs  = M.filter (   elem k . kvars . crhs) (cm fi)
    remainingCs = M.filter (notElem k . kvars . crhs) (cm fi)
    kvWfC = ws fi M.! k
    be = bs fi
    kDom = domain be kvWfC
    orPred = {-# SCC "orPred" #-} POr $!! extractPred kDom be <$> M.elems relevantCs

extractPred :: [Var] -> BindEnv -> SimpC a -> Expr
extractPred kDom be sc = renameQuantified (subcId sc) kSol
  where
    env = clhs be sc
    binds = env
    nonFuncBinds = (\(v,_) -> (vname v, vsort v)) <$> filter (nonFunction be . fst) binds
    lhsPreds     = bindPred <$> env
    suPreds      = substPreds kDom $ crhs sc
    kSol         = PExist nonFuncBinds $ PAnd (lhsPreds ++ suPreds)

-- x:{v:int|v=10} -> (x=10)
bindPred :: (Var, Reft) -> Expr
bindPred (sym, sr) = subst1 (reftPred sr) sub
  where
    sub = (reftBind sr, EVar sym)

-- k0[v:=e1][x:=e2] -> [v = e1, x = e2]
substPreds :: [Var] -> Expr -> [Expr]
substPreds dom (PKVar _ (Su subs)) = [PAtom Eq (EVar sym) e | (sym, e) <- M.toList subs , sym `elem` dom]
substPreds _ _ = errorstar "Eliminate.substPreds called on bad input"

nonFunction :: BindEnv -> Var -> Bool
nonFunction be sym = sym `notElem` funcs
  where
    funcs = [sym | (_, sym, sr) <- bindEnvToList be, isFunctionReft sr]

domain :: BindEnv -> WfC a -> [Var]
domain be wfc = (fst $ wrft wfc) : map fst (envCs be $ wenv wfc)

renameQuantified :: Integer -> Expr -> Expr
renameQuantified i (PExist bs p) = PExist bs' p'
  where
    su  = substFromQBinds i bs
    bs' = (first (`existSymbol` i))  <$> bs
    p'  = subst su p
renameQuantified _ _ = errorstar "Eliminate.renameQuantified called on bad input"

-- NV TODO: change existentials to variables... 
substFromQBinds :: Integer -> [(Symbol, Sort)] -> Subst
substFromQBinds i bs = Su $ M.fromList [(makeVar s t, EVar $ makeVar (existSymbol s i) t) | (s,t) <- bs]
