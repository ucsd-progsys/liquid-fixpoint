{-# LANGUAGE DeriveGeneric #-}

module Language.Fixpoint.Solver.TrivialSort (nontrivsorts) where

import           GHC.Generics        (Generic)
import           Control.Arrow       (second)
import           Language.Fixpoint.Visitor
import           Language.Fixpoint.Config
import           Language.Fixpoint.Types hiding (simplify)
import           Language.Fixpoint.Files
import           Language.Fixpoint.Misc
import qualified Data.HashSet            as S
import           Data.Hashable
import qualified Data.HashMap.Strict     as M
import           Data.List (foldl')
import qualified Data.Graph              as G
import           Data.Maybe
import           Data.Monoid
import           Text.Printf
import           Debug.Trace
import           Language.Fixpoint.PrettyPrint

-------------------------------------------------------------------------
nontrivsorts :: (Fixpoint a) => Config -> FInfo a -> IO (Result SubC a)
-------------------------------------------------------------------------
nontrivsorts cfg fi = do
  let fi' = simplify' cfg fi
  writeFInfo cfg fi' $ extFileName Out (inFile cfg)
  return mempty

simplify' :: Config -> FInfo a -> FInfo a
simplify' _ fi = simplifyFInfo (mkNonTrivSorts fi) fi

--------------------------------------------------------------------
-- | The main data types
--------------------------------------------------------------------
type NonTrivSorts = S.HashSet Sort
type KVarMap      = M.HashMap KVar [Sort]
data Polarity     = Lhs | Rhs
type TrivInfo     = (NonTrivSorts, KVarMap)
--------------------------------------------------------------------

--------------------------------------------------------------------
mkNonTrivSorts :: FInfo a -> NonTrivSorts
--------------------------------------------------------------------
mkNonTrivSorts = tracepp "mkNonTrivSorts: "
               . nonTrivSorts . trivInfo

--------------------------------------------------------------------
nonTrivSorts :: TrivInfo -> NonTrivSorts
--------------------------------------------------------------------
nonTrivSorts ti = S.fromList [s | S s <- ntvs]
  where
    ntvs        = [fst3 (f v) | v <- G.reachable g root]
    (g, f, fv)  = G.graphFromEdges $ ntGraph ti
    root        = fromMaybe err    $ fv NTV
    err         = errorstar "nonTrivSorts: cannot find root!"

ntGraph :: TrivInfo -> NTG
ntGraph ti = [(v,v,vs) | (v, vs) <- groupList $ ntEdges ti]

ntEdges :: TrivInfo -> [(NTV, NTV)]
ntEdges (nts, kvm) = es ++ [(v, u) | (u, v) <- es]
  where
    es = [(NTV, S s) | s       <- S.toList nts         ]
      ++ [(K k, S s) | (k, ss) <- M.toList kvm, s <- ss]

type NTG = [(NTV, NTV, [NTV])]

data NTV = NTV
         | K KVar
         | S Sort
         deriving (Eq, Ord, Show, Generic)

instance Hashable NTV

--------------------------------------------------------------------
trivInfo :: FInfo a -> TrivInfo
--------------------------------------------------------------------
trivInfo fi = updTISubCs (M.elems $ cm fi)
            . updTIBinds (bs fi)
            $ (S.empty, M.empty)

updTISubCs :: [SubC a] -> TrivInfo -> TrivInfo
updTISubCs cs ti = foldl' (flip updTISubC) ti cs

updTISubC :: SubC a -> TrivInfo -> TrivInfo
updTISubC c = updTI Lhs (slhs c) . updTI Rhs (srhs c)

updTIBinds :: BindEnv -> TrivInfo -> TrivInfo
updTIBinds be ti = foldl' (flip (updTI Lhs)) ti ts
  where
    ts           = [t | (_,_,t) <- bindEnvToList be]

--------------------------------------------------------------------
updTI :: Polarity -> SortedReft -> TrivInfo -> TrivInfo
--------------------------------------------------------------------
updTI p (RR t r) = addKVs t (kvars r) . addNTS p r t

addNTS :: Polarity -> Reft -> Sort -> TrivInfo -> TrivInfo
addNTS p r t ti
  | isNTR p r = addSort t ti
  | otherwise = ti

addKVs :: Sort -> [KVar] -> TrivInfo -> TrivInfo
addKVs t ks ti     = foldl' addK ti ks
  where
    addK (ts, m) k = (ts, inserts k t m)

addSort :: Sort -> TrivInfo -> TrivInfo
addSort t (ts, m) = (S.insert t ts, m)

--------------------------------------------------------------------
isNTR :: Polarity -> Reft -> Bool
--------------------------------------------------------------------
isNTR Rhs = not . trivR
isNTR Lhs = not . trivOrSingR

trivR :: Reft -> Bool
trivR = all trivP . conjuncts . reftPred

trivOrSingR :: Reft -> Bool
trivOrSingR (Reft (v, ra)) = all trivOrSingP $ conjuncts $ raPred ra
  where
    trivOrSingP p          = trivP p || singP v p

trivP :: Pred -> Bool
trivP (PKVar {}) = True
trivP p          = isTautoPred p

singP :: Symbol -> Pred -> Bool
singP v (PAtom Eq (EVar x) _)
  | v == x                    = True
singP v (PAtom Eq _ (EVar x))
  | v == x                    = True
singP _ _                     = False

-------------------------------------------------------------------------
simplifyFInfo :: NonTrivSorts -> FInfo a -> FInfo a
-------------------------------------------------------------------------
simplifyFInfo tm fi = fi {
     cm   = simplifySubCs   tm $ cm fi
   , ws   = simplifyWfCs    tm $ ws fi
   , bs   = simplifyBindEnv tm $ bs fi
   , gs   = simplifyFEnv    tm $ gs fi
}

simplifyBindEnv :: NonTrivSorts -> BindEnv -> BindEnv
simplifyBindEnv = mapBindEnv . second . simplifySortedReft

simplifyFEnv :: NonTrivSorts -> FEnv -> FEnv
simplifyFEnv = fmap . simplifySortedReft

simplifyWfCs :: NonTrivSorts -> [WfC a] -> [WfC a]
simplifyWfCs tm = filter (isNonTrivialSort tm . sr_sort . wrft)

simplifySubCs ti cm = trace msg cm'
  where
    cm' = tx cm
    tx  = M.fromList . mapMaybe (simplifySubC ti) . M.toList
    msg = printf "simplifySUBC: before = %d, after = %d \n" n n'
    n   = M.size cm
    n'  = M.size cm'

simplifySubC :: NonTrivSorts -> (b, SubC a) -> Maybe (b, SubC a)
simplifySubC tm (i, c)
 | isNonTrivial srhs' = Just (i, c { slhs = slhs' , srhs = srhs' })
 | otherwise          = Nothing
  where
    slhs'             = simplifySortedReft tm (slhs c)
    srhs'             = simplifySortedReft tm (srhs c)

simplifySortedReft :: NonTrivSorts -> SortedReft -> SortedReft
simplifySortedReft tm sr
  | nonTrivial = sr
  | otherwise  = sr { sr_reft = mempty }
  where
    nonTrivial = isNonTrivialSort tm (sr_sort sr)

isNonTrivialSort :: NonTrivSorts -> Sort -> Bool
isNonTrivialSort tm t = S.member t tm
