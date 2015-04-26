module Language.Fixpoint.Solver.Eliminate
       (eliminateAll, solve) where

import           Language.Fixpoint.Types
import qualified Language.Fixpoint.Solver.Deps as D
import           Language.Fixpoint.Visitor (kvars)
import           Language.Fixpoint.Names   (nonSymbol)
import           Language.Fixpoint.Misc    (errorstar)

import qualified Data.HashMap.Strict           as M
import           Data.List (partition)


--------------------------------------------------------------
-- | Dummy just for debugging --------------------------------
--------------------------------------------------------------
import qualified Text.PrettyPrint.HughesPJ as Debug
import           Language.Fixpoint.Config
solve :: Config -> FInfo a -> IO (FixResult a)
--------------------------------------------------------------
solve cfg fi = do
  let d = D.deps fi
  let blah = toFixpoint (eliminateAll fi d)
  putStr (Debug.render blah)
  return Safe


class Elimable a where
  elimKVar :: KVar -> Pred -> a -> a

instance Elimable Pred where
  elimKVar kv pr p@(PKVar k su) | kv == k   = subst su pr
                                | otherwise = p
  elimKVar kv pr (PAnd ps)      = PAnd $ map (elimKVar kv pr) ps
  elimKVar kv pr (POr ps)       = POr  $ map (elimKVar kv pr) ps
  elimKVar kv pr (PNot p)       = PNot (elimKVar kv pr p)
  elimKVar kv pr (PImp p q)     = PImp (elimKVar kv pr p) (elimKVar kv pr q)
  elimKVar kv pr (PIff p q)     = PIff (elimKVar kv pr p) (elimKVar kv pr q)
  elimKVar kv pr (PAll bs p)    = PAll   bs (elimKVar kv pr p)
  elimKVar kv pr (PExist bs p)  = PExist bs (elimKVar kv pr p)
  elimKVar _ _ p                = p

instance Elimable (SubC a) where
  elimKVar kv pr x = x { sgrd = elimKVar kv pr (sgrd x)
                       , slhs = elimKVar kv pr (slhs x)
                       --, srhs = elimKVar kv pr (srhs x)
                       }

instance Elimable SortedReft where
  elimKVar kv pr x = x { sr_reft = elimKVar kv pr (sr_reft x) }

instance Elimable Reft where
  elimKVar kv pr (Reft (s, refa)) = Reft (s, (elimKVar kv pr refa))

instance Elimable Refa where
  elimKVar kv pr x = x { raPred = elimKVar kv pr (raPred x) }

instance Elimable (FInfo a) where
  elimKVar kv pr x = x { cm = M.map (elimKVar kv pr) (cm x)
                       , bs = elimKVar kv pr (bs x)
                       }

instance Elimable BindEnv where
  elimKVar kv pr = mapBindEnv (\(sym, sr) -> (sym, (elimKVar kv pr sr)))


eliminateAll :: FInfo a -> D.Deps -> FInfo a
eliminateAll fInfo ds = foldl eliminate fInfo (D.depNonCuts ds)

eliminate :: FInfo a -> KVar -> FInfo a
eliminate fInfo kv = elimKVar kv orPred (fInfo { cm = remainingSubCs , ws = remainingWs})
  where
    relevantSubCs  = M.filter (      (elem kv) . D.rhsKVars) (cm fInfo)
    remainingSubCs = M.filter (not . (elem kv) . D.rhsKVars) (cm fInfo)
    (kVarSReft, remainingWs) = bar kv (ws fInfo)
    orPred = POr (map (foo kVarSReft fInfo) (M.elems relevantSubCs))

--TODO: ignores the WfC's env
bar :: KVar -> [WfC a] -> (SortedReft, [WfC a])
bar kv ws = (wrft w', ws')
  where
    (w, ws') = partition (elem kv . kvars . sr_reft . wrft) ws
    w' | [x] <- w  = x
       | otherwise = errorstar $ (show kv) ++ " needs exactly one wf constraint"

--TODO: ignores a constraint's sgrd, stag, and sinfo
foo :: SortedReft -> FInfo a -> SubC a -> Pred
foo kvSreft fInfo subC = pr'
  where
    bindings = envCs (bs fInfo) (senv subC)
    kVarVV = reftBind $ sr_reft kvSreft
    pr = baz $ (zoink kVarVV (slhs subC)) : bindings
    pr' = projectNonWFVars [(kVarVV, sr_sort kvSreft)] pr

projectNonWFVars :: [(Symbol,Sort)] -> ([(Symbol,Sort)],Pred) -> Pred
projectNonWFVars wfVars (vars, pr) = PExist [v | v <- vars, not (elem v wfVars)] pr

zoink :: Symbol -> SortedReft -> (Symbol, SortedReft)
zoink sym lhs = (sym, subst1 lhs (oldV, eVar sym))
  where
    oldV = reftBind $ sr_reft lhs

-- [ x:{v:int|v=10} , y:{v:int|v=20} ] -> [x:int, y:int], (x=10) /\ (y=20)
baz :: [(Symbol, SortedReft)] -> ([(Symbol,Sort)],Pred)
baz bindings = (bs, PAnd $ map blah bindings)
  where
    bs = map (\(sym, sreft) -> (sym, sr_sort sreft)) bindings

blah :: (Symbol, SortedReft) -> Pred
blah (sym, sr) = subst1 (reftPred reft) sub
  where
    reft = sr_reft sr
    sub = ((reftBind reft), (eVar sym))
