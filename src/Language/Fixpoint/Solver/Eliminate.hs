{-# LANGUAGE FlexibleContexts #-}
module Language.Fixpoint.Solver.Eliminate
       (eliminateAll) where

import           Language.Fixpoint.Types
import qualified Language.Fixpoint.Solver.Deps as D
import           Language.Fixpoint.Visitor (kvars, mapKVars)
import           Language.Fixpoint.Names   (existSymbol)
import           Language.Fixpoint.Misc    (errorstar)

import qualified Data.HashMap.Strict           as M
import           Data.List (partition, (\\))
import           Data.Foldable (foldlM)
import           Control.Monad.State (get, put, runState, evalState, State)


--------------------------------------------------------------
eliminateAll :: FInfo a -> FInfo a
eliminateAll fi = evalState (foldlM eliminate fi (D.depNonCuts ds)) 0
  where
    ds = D.deps fi
--------------------------------------------------------------


class Elimable a where
  elimKVar :: KVar -> Pred -> a -> a

instance Elimable (SubC a) where
  -- we don't bother editing srhs since if kv is on the rhs then the entire constraint should get eliminated
  elimKVar kv pr x = x { slhs = elimKVar kv pr (slhs x) }

instance Elimable SortedReft where
  elimKVar kv pr x = x { sr_reft = elimKVar kv pr (sr_reft x) }

instance Elimable Reft where
  elimKVar kv pr = mapKVars go
    where
      go k = if kv == k then Just pr else Nothing

instance Elimable (FInfo a) where
  elimKVar kv pr x = x { cm = M.map (elimKVar kv pr) (cm x)
                       , bs = elimKVar kv pr (bs x)
                       }

instance Elimable BindEnv where
  elimKVar kv pr = mapBindEnv (\(sym, sr) -> (sym, elimKVar kv pr sr))


eliminate :: FInfo a -> KVar -> State Integer (FInfo a)
eliminate fInfo kv = do
  n <- get
  let relevantSubCs  = M.filter (   elem kv . D.rhsKVars) (cm fInfo)
  let remainingSubCs = M.filter (notElem kv . D.rhsKVars) (cm fInfo)
  let (kvWfC, remainingWs) = findWfC kv (ws fInfo)
  let (bindingsList, (n', orPred)) = runState (mapM (extractPred kvWfC (bs fInfo)) (M.elems relevantSubCs)) (n, POr [])
  let bindings = concat bindingsList

  let be = bs fInfo
  let (ids, be') = insertsBindEnv [(sym, trueSortedReft srt) | (sym, srt) <- bindings] be
  let newSubCs = M.map (\s -> s { senv = insertsIBindEnv ids (senv s)}) remainingSubCs
  put n'
  return $ elimKVar kv orPred (fInfo { cm = newSubCs , ws = remainingWs , bs = be' })

insertsBindEnv :: [(Symbol, SortedReft)] -> BindEnv -> ([BindId], BindEnv)
insertsBindEnv bs = runState (mapM go bs)
  where
    go (sym, srft) = do be <- get
                        let (id, be') = insertBindEnv sym srft be
                        put be'
                        return id

findWfC :: KVar -> [WfC a] -> (WfC a, [WfC a])
findWfC kv ws = (w', ws')
  where
    (w, ws') = partition (elem kv . kvars . sr_reft . wrft) ws
    w' | [x] <- w  = x
       | otherwise = errorstar $ (show kv) ++ " needs exactly one wf constraint"

extractPred :: WfC a -> BindEnv -> SubC a -> State (Integer, Pred) [(Symbol, Sort)]
extractPred wfc be subC = do (n, (POr preds)) <- get
                             let (bs, (n', pr')) = runState (mapM renameVar vars) (n, PAnd $ pr : [(blah (kVarVV, slhs subC))] ++ suPreds')
                             put (n', POr $ pr' : preds)
                             return bs
  where
    wfcIBinds  = elemsIBindEnv $ wenv wfc
    subcIBinds = elemsIBindEnv $ senv subC
    unmatchedIBinds | wfcIBinds `subset` subcIBinds = subcIBinds \\ wfcIBinds
                    | otherwise = errorstar $ "kVar is not well formed (missing bindings)" ++ "kVar: " ++ (showFix wfc) ++ "constraint: " ++ (showFix subC)
    unmatchedIBindEnv = insertsIBindEnv unmatchedIBinds emptyIBindEnv
    unmatchedBindings = envCs be unmatchedIBindEnv

    kvSreft = wrft wfc
    kVarVV = reftBind $ sr_reft kvSreft

    (vars, pr) = baz unmatchedBindings

    reft = sr_reft $ srhs subC
    suPreds = substPreds $ reftPred reft
    sub = ((reftBind reft), (eVar kVarVV))
    suPreds' = [subst1 p sub | p <- suPreds]

-- on rhs, k0[v:=z] -> [v = z]
substPreds :: Pred -> [Pred]
substPreds (PKVar _ (Su subs)) = map (\(sym, expr) -> PAtom Eq (eVar sym) expr) subs

renameVar :: (Symbol, Sort) -> State (Integer, Pred) (Symbol, Sort)
renameVar (sym, srt) = do (n, pr) <- get
                          let sym' = existSymbol sym n
                          put ((n+1), subst1 pr (sym, eVar sym'))
                          return (sym', srt)

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = (xs \\ ys) == []

-- [ x:{v:int|v=10} , y:{v:int|v=20} ] -> [x:int, y:int], (x=10) /\ (y=20)
baz :: [(Symbol, SortedReft)] -> ([(Symbol,Sort)],Pred)
baz bindings = (bs, PAnd $ map blah bindings)
  where
    bs = map (\(sym, sreft) -> (sym, sr_sort sreft)) bindings

-- [ x:{v:int|v=10} ] -> (x=10)
blah :: (Symbol, SortedReft) -> Pred
blah (sym, sr) = subst1 (reftPred reft) sub
  where
    reft = sr_reft sr
    sub = ((reftBind reft), (eVar sym))
