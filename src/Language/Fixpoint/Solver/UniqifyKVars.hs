{-
This module creates new bindings for each argument of each kvar.
It also makes sure that all arguments to each kvar are explicit.
For example,

'''
bind 0 x
bind 1 v
constraint:
  env [0; 1]
  rhs $k_42 // implicit substitutions [x:=x], [v:=v]
wf:
  env [0]
  reft {v : int | [$k_42]}
'''

becomes

'''
bind 0 x
bind 1 v
bind 2 lq_karg$x$k_42
constraint:
  env [0; 1]
  rhs $k_42[lq_karg$x$k_42:=x][lq_karg$v$k_42:=v]

wf:
  env [2]
  reft {lq_karg$v$k_42 : int | [$k_42]}
'''
-}

module Language.Fixpoint.Solver.UniqifyKVars (wfcUniqify) where

import           Language.Fixpoint.Types
import           Language.Fixpoint.Types.Visitor (mapKVarSubsts)
-- import           Language.Fixpoint.Solver.Validate (getDomain)
import qualified Data.HashMap.Strict as M
import           Data.Foldable       (foldl')

--------------------------------------------------------------
wfcUniqify    :: SInfo a -> SInfo a
wfcUniqify fi = updateWfcs $ remakeSubsts fi
--------------------------------------------------------------

--------------------------------------------------------------
remakeSubsts :: SInfo a -> SInfo a
--------------------------------------------------------------
remakeSubsts fi = mapKVarSubsts (remakeSubst fi) fi

remakeSubst :: SInfo a -> KVar -> Subst -> Subst
remakeSubst fi k su = foldl' (updateSubst k) su kDom
  where
    kDom = kvarDomain fi k

updateSubst :: KVar -> Subst -> Symbol -> Subst
updateSubst k (Su su) sym
  | sym `M.member` su = Su $ M.delete sym $ M.insert (kArgSymbol sym kx) (su M.! sym) su
  | otherwise         = Su $ M.insert (kArgSymbol sym kx) (eVar sym) su
  where
    kx                = kv k

--------------------------------------------------------------
updateWfcs :: SInfo a -> SInfo a
--------------------------------------------------------------
updateWfcs fi = M.foldl' updateWfc fi (ws fi)

updateWfc :: SInfo a -> WfC a -> SInfo a
updateWfc fi w    = fi' { ws = M.insert k w' (ws fi) }
  where
    (v, t, k)     = wrft w
    (fi', newIds) = insertNewBinds w fi k
    env'          = insertsIBindEnv newIds emptyIBindEnv
    w'            = w { wenv = env', wrft = (kArgSymbol v (kv k), t, k) }

insertNewBinds :: WfC a -> SInfo a -> KVar -> (SInfo a, [BindId])
insertNewBinds w fi k = foldl' (accumBindsIfValid k) (fi, []) (elemsIBindEnv $ wenv w)

accumBindsIfValid :: KVar -> (SInfo a, [BindId]) -> BindId -> (SInfo a, [BindId])
accumBindsIfValid k (fi, ids) i = if renamable then accumBinds k (fi, ids) i else (fi, i : ids)
  where
    (_, sr) = lookupBindEnv i (bs fi)
    renamable = isValidInRefinements $ sr_sort sr

accumBinds :: KVar -> (SInfo a, [BindId]) -> BindId -> (SInfo a, [BindId])
accumBinds k (fi, ids) i = (fi {bs = be'}, i' : ids)
  where
    --TODO: could we ignore the old SortedReft? what would it mean if it were non-trivial in a wf environment?
    (oldSym, sr) = lookupBindEnv i (bs fi)
    newSym       = {- tracepp "kArgSymbol" $ -}  kArgSymbol oldSym (kv k)
    (i', be')    = insertBindEnv newSym sr (bs fi)
--------------------------------------------------------------

isValidInRefinements :: Sort -> Bool
isValidInRefinements FInt        = True
isValidInRefinements FReal       = True
isValidInRefinements FNum        = False
isValidInRefinements FFrac       = False
isValidInRefinements (FObj _)    = True
isValidInRefinements (FVar _)    = True
isValidInRefinements (FFunc _ _) = False
isValidInRefinements (FAbs  _ t) = isValidInRefinements t
isValidInRefinements (FTC _)     = True --TODO is this true? seems to be required for e.g. ResolvePred.hs
isValidInRefinements (FApp _ _)  = True
