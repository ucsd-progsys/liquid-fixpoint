{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Fixpoint.Types.Visitor (
  -- * Visitor
     Visitor (..)

  -- * Extracting Symbolic Constants (String Literals)
  ,  SymConsts (..)

  -- * Default Visitor
  , defaultVisitor

  -- * Transformers
  , trans

  -- * Accumulators
  , fold

  -- * Clients
  , kvars
  , envKVars
  , rhsKVars
  , mapKVars, mapKVars', mapKVarSubsts

  -- * Predicates on Constraints
  , isConcC , isKvarC

  -- * Sorts
  , foldSort, mapSort


  ) where

import           Control.Monad.Trans.State (State, modify, runState)
import qualified Data.HashSet        as S
import qualified Data.HashMap.Strict as M
import qualified Data.List           as L
import           Language.Fixpoint.Utils.Misc (sortNub)
import           Language.Fixpoint.Types

data Visitor acc ctx = Visitor {
 -- | Context @ctx@ is built in a "top-down" fashion; not "across" siblings
    ctxExpr :: ctx -> Expr -> ctx
  , ctxPred :: ctx -> Pred -> ctx

  -- | Transforms can access current @ctx@
  , txExpr  :: ctx -> Expr -> Expr
  , txPred  :: ctx -> Pred -> Pred

  -- | Accumulations can access current @ctx@; @acc@ value is monoidal
  , accExpr :: ctx -> Expr -> acc
  , accPred :: ctx -> Pred -> acc
  }

---------------------------------------------------------------------------------
defaultVisitor :: Monoid acc => Visitor acc ctx
---------------------------------------------------------------------------------
defaultVisitor = Visitor {
    ctxExpr    = const -- \c _ -> c
  , ctxPred    = const -- \c _ -> c
  , txExpr     = \_ x -> x
  , txPred     = \_ x -> x
  , accExpr    = \_ _ -> mempty
  , accPred    = \_ _ -> mempty
  }

------------------------------------------------------------------------

fold         :: (Visitable t, Monoid a) => Visitor a ctx -> ctx -> a -> t -> a
fold v c a t = snd $ execVisitM v c a visit t

trans        :: (Visitable t, Monoid a) => Visitor a ctx -> ctx -> a -> t -> t
trans v c _ z = fst $ execVisitM v c mempty visit z

execVisitM :: Visitor a ctx -> ctx -> a -> (Visitor a ctx -> ctx -> t -> State a t) -> t -> (t, a)
execVisitM v c a f x = runState (f v c x) a

type VisitM acc = State acc

accum :: (Monoid a) => a -> VisitM a ()
accum = modify . mappend

(<$$>) ::  (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
f <$$> x = traverse f x

------------------------------------------------------------------------------
class Visitable t where
  visit :: (Monoid a) => Visitor a c -> c -> t -> VisitM a t

instance Visitable Expr where
  visit = visitExpr

instance Visitable Pred where
  visit = visitPred

instance Visitable Reft where
  visit v c (Reft (x, ra)) = (Reft . (x, )) <$> visit v c ra

instance Visitable SortedReft where
  visit v c (RR t r) = RR t <$> visit v c r

instance Visitable (Symbol, SortedReft) where
  visit v c (sym, sr) = (sym, ) <$> visit v c sr

instance Visitable BindEnv where
  visit v c = mapM (visit v c)

---------------------------------------------------------------------------------
-- Warning: these instances were written for mapKVars over SInfos only;
--  check that they behave as expected before using with other clients.
instance Visitable (SimpC a) where
  visit v c x = do
    rhs' <- visit v c (_crhs x)
    return x { _crhs = rhs' }

instance Visitable (SInfo a) where
  visit v c x = do
    cm' <- mapM (visit v c) (cm x)
    bs' <- visit v c (bs x)
    return x { cm = cm', bs = bs' }
---------------------------------------------------------------------------------

visitMany :: (Monoid a, Visitable t) => Visitor a ctx -> ctx -> [t] -> VisitM a [t]
visitMany v c xs = visit v c <$$> xs

visitExpr :: (Monoid a) => Visitor a ctx -> ctx -> Expr -> VisitM a Expr
visitExpr v = vE
  where
    vP     = visitPred v
    vE c e = accum acc >> step c' e' where c'  = ctxExpr v c e
                                           e'  = txExpr v c' e
                                           acc = accExpr v c' e
    step _ e@EBot         = return e
    step _ e@(ESym _)     = return e
    step _ e@(ECon _)     = return e
    step _ e@(EVar _)     = return e
    step c (EApp f es)    = EApp f     <$> (vE c <$$> es)
    step c (ENeg e)       = ENeg       <$> vE c e
    step c (EBin o e1 e2) = EBin o     <$> vE c e1 <*> vE c e2
    step c (EIte p e1 e2) = EIte       <$> vP c p  <*> vE c e1 <*> vE c e2
    step c (ECst e t)     = (`ECst` t) <$> vE c e

visitPred :: (Monoid a) => Visitor a ctx -> ctx -> Pred -> VisitM a Pred
visitPred v = vP
  where
    -- vS1 c (x, e)  = (x,) <$> vE c e
    -- vS c (Su xes) = Su <$> vS1 c <$$> xes
    vE      = visitExpr v
    vP c p  = accum acc >> step c' p' where c'   = ctxPred v c p
                                            p'   = txPred v c' p
                                            acc  = accPred v c' p
    step c (PAnd  ps)      = PAnd     <$> (vP c <$$> ps)
    step c (POr  ps)       = POr      <$> (vP c <$$> ps)
    step c (PNot p)        = PNot     <$> vP c p
    step c (PImp p1 p2)    = PImp     <$> vP c p1 <*> vP c p2
    step c (PIff p1 p2)    = PIff     <$> vP c p1 <*> vP c p2
    step c (PBexp  e)      = PBexp    <$> vE c e
    step c (PAtom r e1 e2) = PAtom r  <$> vE c e1 <*> vE c e2
    step c (PAll xts p)    = PAll   xts <$> vP c p
    step c (PExist xts p)  = PExist xts <$> vP c p
    step _ p@(PKVar _ _)   = return p -- PAtom r  <$> vE c e1 <*> vE c e2
    step _ p@PTrue         = return p
    step _ p@PFalse        = return p
    step _ p@PTop          = return p


mapKVars :: Visitable t => (KVar -> Maybe Pred) -> t -> t
mapKVars f = mapKVars' f'
  where
    f' (kv', _) = f kv'

mapKVars' :: Visitable t => ((KVar, Subst) -> Maybe Pred) -> t -> t
mapKVars' f            = trans kvVis () []
  where
    kvVis              = defaultVisitor { txPred = txK }
    txK _ (PKVar k su)
      | Just p' <- f (k, su) = subst su p'
    txK _ p            = p

mapKVarSubsts :: Visitable t => (KVar -> Subst -> Subst) -> t -> t
mapKVarSubsts f        = trans kvVis () []
  where
    kvVis              = defaultVisitor { txPred = txK }
    txK _ (PKVar k su) = PKVar k $ f k su
    txK _ p            = p

kvars :: Visitable t => t -> [KVar]
kvars                = fold kvVis () []
  where
    kvVis            = defaultVisitor { accPred = kv' }
    kv' _ (PKVar k _) = [k]
    kv' _ _           = []

envKVars :: (TaggedC c a) => BindEnv -> c a -> [KVar]
envKVars be c = squish [ kvs sr |  (_, sr) <- clhs be c]
  where
    squish    = S.toList  . S.fromList . concat
    kvs       = kvars . sr_reft

-- lhsKVars :: BindEnv -> SubC a -> [KVar]
-- lhsKVars binds c = envKVs ++ lhsKVs
  -- where
    -- envKVs       = envKVars binds         c
    -- lhsKVs       = kvars          $ lhsCs c

rhsKVars :: (TaggedC c a) => c a -> [KVar]
rhsKVars = kvars . crhs -- rhsCs

isKvarC :: (TaggedC c a) => c a -> Bool
isKvarC = all isKvar . conjuncts . crhs

isConcC :: (TaggedC c a) => c a -> Bool
isConcC = all isConc . conjuncts . crhs

isKvar :: Pred -> Bool
isKvar (PKVar {}) = True
isKvar _          = False

isConc :: Pred -> Bool
isConc = null . kvars

---------------------------------------------------------------------------------
-- | Visitors over @Sort@
---------------------------------------------------------------------------------
foldSort :: (a -> Sort -> a) -> a -> Sort -> a
foldSort f = step
  where
    step b t          = go (f b t) t
    go b (FFunc _ ts) = L.foldl' step b ts
    go b (FApp t1 t2) = L.foldl' step b [t1, t2]
    go b _            = b

mapSort :: (Sort -> Sort) -> Sort -> Sort
mapSort f = step
  where
    step            = go . f
    go (FFunc n ts) = FFunc n $ step <$> ts
    go (FApp t1 t2) = FApp (step t1) (step t2)
    go t            = t

---------------------------------------------------------------
-- | String Constants -----------------------------------------
---------------------------------------------------------------

-- symConstLits    :: FInfo a -> [(Symbol, Sort)]
-- symConstLits fi = [(symbol c, strSort) | c <- symConsts fi]

class SymConsts a where
  symConsts :: a -> [SymConst]

instance  SymConsts (FInfo a) where
  symConsts fi = sortNub $ csLits ++ bsLits ++ qsLits
    where
      csLits   = concatMap symConsts $ M.elems  $  cm    fi
      bsLits   = symConsts           $ bs                fi
      qsLits   = concatMap symConsts $ q_body  <$> quals fi

instance SymConsts BindEnv where
  symConsts    = concatMap (symConsts . snd) . M.elems . beBinds

instance SymConsts (SubC a) where
  symConsts c  = symConsts (slhs c) ++
                 symConsts (srhs c)

instance SymConsts SortedReft where
  symConsts = symConsts . sr_reft

instance SymConsts Reft where
  symConsts (Reft (_, ra)) = getSymConsts ra

instance SymConsts Pred where
  symConsts = getSymConsts

getSymConsts :: Visitable t => t -> [SymConst]
getSymConsts         = fold scVis () []
  where
    scVis            = defaultVisitor { accExpr = sc }
    sc _ (ESym c)    = [c]
    sc _ _           = []


{-

instance SymConsts (SimpC a) where
  symConsts c  = symConsts (crhs c)

-}