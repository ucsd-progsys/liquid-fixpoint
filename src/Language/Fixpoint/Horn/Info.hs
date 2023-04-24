{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}

module Language.Fixpoint.Horn.Info (
    hornFInfo
  ) where

import           Control.Monad (forM)
import qualified Data.HashMap.Strict            as M
import qualified Data.List                      as L
import qualified Data.Tuple                     as Tuple
import           Data.Either                    (partitionEithers)
import           GHC.Generics                   (Generic)
import qualified Language.Fixpoint.Misc         as Misc
import qualified Language.Fixpoint.Types        as F
import qualified Language.Fixpoint.Types.Config as F
import qualified Language.Fixpoint.Horn.Types   as H
import           Data.Maybe (catMaybes, fromMaybe)

hornFInfo :: F.Config -> H.Query a -> F.FInfo a
hornFInfo cfg q = mempty
  { F.cm        = cs
  , F.bs        = be2
  , F.ebinds    = ebs
  , F.ws        = kvEnvWfCs kve
  , F.quals     = H.qQuals q ++ scrapeCstr hCst
  , F.gLits     = F.fromMapSEnv $ H.qCon q
  , F.dLits     = F.fromMapSEnv $ H.qDis q
  , F.ae        = axEnv cfg q cs
  , F.ddecls    = H.qData q
  }
  where
    be0         = F.emptyBindEnv
    (be1, kve)  = hornWfs   be0     (H.qVars q)
    (be2, ebs, cs) = hornSubCs be1 kve hCst
    hCst           = H.qCstr q

axEnv :: F.Config -> H.Query a -> M.HashMap F.SubcId b -> F.AxiomEnv
axEnv cfg q cs = mempty
  { F.aenvEqs    = H.qEqns q
  , F.aenvSimpl  = H.qMats q
  , F.aenvExpand = if F.rewriteAxioms cfg then True <$ cs else mempty
  }

----------------------------------------------------------------------------------
hornSubCs :: F.BindEnv a -> KVEnv a -> H.Cstr a
          -> (F.BindEnv a, [F.BindId], M.HashMap F.SubcId (F.SubC a))
----------------------------------------------------------------------------------
hornSubCs be kve c = (be', ebs, M.fromList (F.addIds cs))
  where
    (be', ebs, cs) = goS kve F.emptyIBindEnv be c
    -- lhs0           = bindSortedReft kve H.dummyBind

-- | @goS@ recursively traverses the NNF constraint to build up a list
--   of the vanilla @SubC@ constraints.

goS :: KVEnv a -> F.IBindEnv ->  F.BindEnv a -> H.Cstr a
    -> (F.BindEnv a, [F.BindId], [F.SubC a])

goS kve env be c = (be', mEbs, subcs)
  where
    (be', ecs) = goS' kve env Nothing be c
    (mEbs, subcs) = partitionEithers ecs

goS' :: KVEnv a -> F.IBindEnv -> Maybe F.SortedReft -> F.BindEnv a -> H.Cstr a
    -> (F.BindEnv a, [Either F.BindId (F.SubC a)])
goS' kve env lhs be (H.Head p l) = (be, [Right subc])
  where
    subc                        = myMkSubC env lhs rhs Nothing [] l
    rhs                         = updSortedReft kve lhs p

goS' kve env lhs be (H.CAnd cs)  = (be', concat subcss)
  where
    (be', subcss)               = L.mapAccumL (goS' kve env lhs) be cs

goS' kve env _   be (H.All b c)  = (be'', subcs)
  where
    (be'', subcs)               = goS' kve env' (Just bSR) be' c
    (bId, be')                  = F.insertBindEnv (H.bSym b) bSR (H.bMeta b) be
    bSR                         = bindSortedReft kve b
    env'                        = F.insertsIBindEnv [bId] env

goS' kve env _   be (H.Any b c)  = (be'', Left bId : subcs)
  where
    (be'', subcs)               = goS' kve env' (Just bSR) be' c
    (bId, be')                  = F.insertBindEnv (H.bSym b) bSR (H.bMeta b) be
    bSR                         = bindSortedReft kve b
    env'                        = F.insertsIBindEnv [bId] env

myMkSubC :: F.IBindEnv -> Maybe F.SortedReft -> F.SortedReft -> Maybe Integer -> F.Tag -> a -> F.SubC a
myMkSubC be lhsMb rhs x y z = F.mkSubC be lhs rhs x y z
  where
    lhs = fromMaybe def lhsMb
    def = F.trueSortedReft (F.sr_sort rhs)

bindSortedReft :: KVEnv a -> H.Bind a -> F.SortedReft
bindSortedReft kve (H.Bind x t p _) = F.RR t (F.Reft (x, predExpr kve p))

updSortedReft :: KVEnv a -> Maybe F.SortedReft -> H.Pred -> F.SortedReft
updSortedReft kve lhs p = F.RR s (F.Reft (v, predExpr kve p))
   where
      (s, v) = case lhs of
                 Just (F.RR ss (F.Reft (vv, _))) -> (ss, vv)
                 Nothing                       -> (F.intSort, F.dummySymbol)

predExpr :: KVEnv a -> H.Pred -> F.Expr
predExpr kve        = go
  where
    go (H.Reft  e ) = e
    go (H.Var k ys) = kvApp kve k ys
    go (H.PAnd  ps) = F.PAnd (go <$> ps)

kvApp :: KVEnv a -> F.Symbol -> [F.Symbol] -> F.Expr
kvApp kve k ys = F.PKVar (F.KV k) su
  where
    su         = F.mkSubst (zip params (F.eVar <$> ys))
    params     = maybe err1 kvParams (M.lookup k kve)
    err1       = F.panic ("Unknown Horn variable: " ++ F.showpp k)

----------------------------------------------------------------------------------
hornWfs :: F.BindEnv a -> [H.Var a] -> (F.BindEnv a, KVEnv a)
----------------------------------------------------------------------------------
hornWfs be vars = (be', kve)
  where
    kve         = M.fromList [ (kname i, i) | i <- is ]
    (be', is)   = L.mapAccumL kvInfo be vars
    kname       = H.hvName . kvVar

kvInfo :: F.BindEnv a -> H.Var a -> (F.BindEnv a, KVInfo a)
kvInfo be k       = (be', KVInfo k (Misc.fst3 <$> xts) wfc)
  where
    -- make the WfC
    wfc           = F.WfC wenv wrft  (H.hvMeta k)
    wenv          = F.fromListIBindEnv ids
    wrft          = (x, t, F.KV (H.hvName k))
    -- add the binders
    (be', ids)    = L.mapAccumL insertBE be xts'
    ((x,t,_), xts') = Misc.safeUncons "Horn var with no args" xts
    -- make the parameters
    xts           = [ (hvarArg k i, t', a) | (t', i) <- zip (H.hvArgs k) [0..] ]
    a             = H.hvMeta k

insertBE :: F.BindEnv a -> (F.Symbol, F.Sort, a) -> (F.BindEnv a, F.BindId)
insertBE be (x, t, a) = Tuple.swap $ F.insertBindEnv x (F.trueSortedReft t) a be

----------------------------------------------------------------------------------
-- | Data types and helpers for manipulating information about KVars
----------------------------------------------------------------------------------
type KVEnv a  = M.HashMap F.Symbol (KVInfo a)

data KVInfo a = KVInfo
  { kvVar    :: !(H.Var a)
  , kvParams :: ![F.Symbol]
  , kvWfC    :: !(F.WfC a)
  }
  deriving (Generic, Functor)

kvEnvWfCs :: KVEnv a -> M.HashMap F.KVar (F.WfC a)
kvEnvWfCs kve = M.fromList [ (F.KV k, kvWfC info) | (k, info) <- M.toList kve ]

hvarArg :: H.Var a -> Int -> F.Symbol
hvarArg k i = F.intSymbol (F.suffixSymbol hvarPrefix (H.hvName k)) i

hvarPrefix :: F.Symbol
hvarPrefix = F.symbol "nnf_arg"

-------------------------------------------------------------------------------
-- | Automatically scrape qualifiers from all predicates in a constraint
-------------------------------------------------------------------------------

scrapeCstr :: H.Cstr a -> [F.Qualifier]
scrapeCstr = go emptyBindEnv
  where
    go senv (H.Head p _) = scrapePred senv p
    go senv (H.CAnd cs)  = concatMap (go senv) cs
    go senv (H.All b c)  = scrapeBind senv' b <> go senv' c where senv' = insertBindEnv b senv
    go senv (H.Any b c)  = scrapeBind senv' b <> go senv' c where senv' = insertBindEnv b senv

scrapeBind :: BindEnv -> H.Bind a -> [F.Qualifier]
scrapeBind senv b = scrapePred senv (H.bPred b)

scrapePred :: BindEnv -> H.Pred -> [F.Qualifier]
scrapePred _    (H.Var _ _) = []
scrapePred senv (H.PAnd ps) = concatMap (scrapePred senv) ps
scrapePred senv (H.Reft e)  = catMaybes (mkQual senv <$> F.concConjuncts e)

mkQual :: BindEnv -> F.Expr -> Maybe F.Qualifier
mkQual env e = do
  ixts       <- qualParams env e
  let qParams = [ F.QP {F.qpSym = x, F.qpPat = F.PatNone, F.qpSort = t} | (_,x,t) <- ixts ]
  return F.Q { F.qName = F.symbol "AUTO", F.qParams = qParams, F.qBody = e, F.qPos = F.dummyPos "" }

  {-
    xs   = free-vars e
    ixts = reverse . sort $ [ (i, x, t) | x <- xs, (i, t) <- lookupBindEnv x senv ]
   -}

qualParams :: BindEnv -> F.Expr -> Maybe [(Int, F.Symbol, F.Sort)]
qualParams env e = do
    let xs = Misc.nubOrd (F.syms e)
    ixts <- forM xs $ \x -> do
              (t, i) <- lookupBindEnv x env
              return (i, x, t)
    return (reverse . L.sort $ ixts)

    -- ixts = [ (i, x, t) | x <- xs, (i, t) <- lookupBindEnv x env ]

-------------------------------------------------------------------------------

-- | `BindEnv` maps each symbol to (sort, depth) pair, where shorter depths
--    means bound "earlier" i.e. in (forall (x1:...) (forall (x2:...) ...)
--    the depth of x1 is less than the depth of x2.
--    We use the heuristic that the symbol with the "largest" depth is the
--    "value-variable" in the qualifier.

data BindEnv = BindEnv
  { bSize  :: !Int
  , bBinds :: M.HashMap F.Symbol (F.Sort, Int)
  }

emptyBindEnv :: BindEnv
emptyBindEnv = BindEnv { bSize = 0, bBinds = mempty }

insertBindEnv :: H.Bind a -> BindEnv -> BindEnv
insertBindEnv b senv = BindEnv { bSize = n + 1, bBinds = M.insert x (t, n) (bBinds senv) }
  where
    n = bSize senv
    x = H.bSym b
    t = H.bSort b

lookupBindEnv :: F.Symbol -> BindEnv -> Maybe (F.Sort, Int)
lookupBindEnv x env = M.lookup x (bBinds env)
