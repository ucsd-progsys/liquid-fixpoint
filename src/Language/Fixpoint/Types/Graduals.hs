{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TupleSections              #-}

{-# OPTIONS_GHC -Wno-name-shadowing     #-}

-- | This module contains the top-level SOLUTION data types,
--   including various indices used for solving.

module Language.Fixpoint.Types.Graduals (
  uniquify,

  makeSolutions,

  GSol,

  Gradual (..)
  ) where

import Language.Fixpoint.Types.Refinements
import Language.Fixpoint.Types.Constraints
import Language.Fixpoint.Types.Config
import Language.Fixpoint.Types.PrettyPrint
import Language.Fixpoint.Types.Environments
import Language.Fixpoint.Types.Substitutions
import Language.Fixpoint.Types.Visitor
import Language.Fixpoint.Types.Spans
import Language.Fixpoint.Types.Theories
import Language.Fixpoint.Types.Names        (gradIntSymbol, tidySymbol)
import Language.Fixpoint.Misc               (allCombinations, errorstar)

import Control.DeepSeq

import qualified Data.HashMap.Strict       as M
import qualified Data.List                 as L

import Control.Monad.State.Lazy
import Data.Maybe (fromMaybe)
import qualified Language.Fixpoint.SortCheck       as So
import Language.Fixpoint.Solver.Sanitize (symbolEnv)


data GSol = GSol !SymEnv !(M.HashMap KVar (Expr, GradInfo))

instance Semigroup GSol where
  (GSol e1 m1) <> (GSol e2 m2) = GSol (e1 <> e2) (m1 <> m2)

instance Monoid GSol where
  mempty = GSol mempty mempty

instance Show GSol where
  show (GSol _ m) = "GSOL = \n" ++ unlines ((\(k,(e, i)) -> showpp k ++ showInfo i ++  " |-> " ++ showpp (tx e)) <$> M.toList m)
    where
      tx e = subst (mkSubst $ [(x, EVar $ tidySymbol x) | x <- syms e]) e
      showInfo i = show i


makeSolutions :: (NFData a, Fixpoint a, Show a)
              => Config -> SInfo a
              -> [(KVar, (GWInfo, [[Expr]]))]
              -> Maybe [GSol]

makeSolutions _ _ []
  = Nothing
makeSolutions cfg fi kes
  = Just $ map (GSol env . M.fromList) (allCombinations (go  <$> kes))
  where
    go (k, (i, es)) = [(k, (pAnd (gexpr i:e'), ginfo i)) | e' <- es]
    env = symbolEnv cfg fi


-------------------------------------------------------------------------------
-- |  Make each gradual appearence unique -------------------------------------
-------------------------------------------------------------------------------
uniquify :: (NFData a, Fixpoint a, Loc a) => SInfo a -> SInfo a

uniquify fi = fi{cm = cm', ws = ws', bs = bs'}
  where
  (cm', km, bs') = uniquifyCS (bs fi) (cm fi)
  ws'            = expandWF km (ws fi)

uniquifyCS :: (NFData a, Fixpoint a, Loc a)
           => BindEnv a
           -> M.HashMap SubcId (SimpC a)
           -> (M.HashMap SubcId (SimpC a), M.HashMap KVar [(KVar, Maybe SrcSpan)], BindEnv a)
uniquifyCS bs cs
  = (x, km, benv st)
  where
    (x, st) = runState (uniq cs) (initUniqueST bs)
    km      = kmap st


class Unique ann a where
   uniq :: a -> UniqueM ann a

instance Unique ann a => Unique ann (M.HashMap SubcId a) where
  uniq m = M.fromList <$> mapM (\(i,x) -> (i,) <$> uniq x) (M.toList m)

instance Loc a => Unique a (SimpC a) where
  uniq cs = do
    updateLoc $ srcSpan $ _cinfo cs
    rhs <- uniq (_crhs cs)
    env <- uniq (_cenv cs)
    return cs{_crhs = rhs, _cenv = env}

instance Unique ann IBindEnv where
  uniq env = withCache (fromListIBindEnv <$> mapM uniq (elemsIBindEnv env))

instance Unique ann BindId where
  uniq i = do
    bs <- benv <$> get
    let (x, t, ann) = lookupBindEnv i bs
    resetChange
    t' <- uniq t
    hasChanged <- change <$> get
    if hasChanged
      then do let (i', bs') = insertBindEnv x t' ann bs
              updateBEnv i bs'
              return i'
      else return i

instance Unique ann SortedReft where
  uniq (RR s r) = RR s <$> uniq r

instance Unique ann Reft where
  uniq (Reft (x,e)) = Reft . (x,) <$> uniq e

instance Unique ann Expr where
  uniq = mapMExpr go
   where
    go (PGrad k su i e) = do
      k'  <- freshK k
      src <- uloc <$> get
      return $ PGrad k' su (i{gused = src}) e
    go e              = return e

-------------------------------------------------------------------------------
-- | The Unique Monad ---------------------------------------------------------
-------------------------------------------------------------------------------

type UniqueM ann = State (UniqueST ann)
data UniqueST a
  = UniqueST { freshId :: Integer
             , kmap    :: M.HashMap KVar [(KVar, Maybe SrcSpan)]
             , change  :: Bool
             , cache   :: M.HashMap KVar KVar
             , uloc    :: Maybe SrcSpan
             , ubs     :: [BindId]
             , benv    :: BindEnv a
             }

updateLoc :: SrcSpan -> UniqueM ann ()
updateLoc x = modify $ \s -> s{uloc = Just x}

withCache :: UniqueM ann a -> UniqueM ann a
withCache act = do
  emptyCache
  a <- act
  emptyCache
  return a

emptyCache :: UniqueM ann ()
emptyCache = modify $ \s -> s{cache = mempty}

addCache :: KVar -> KVar -> UniqueM ann ()
addCache k k' = modify $ \s -> s{cache = M.insert k k' (cache s)}

updateBEnv :: BindId -> BindEnv a -> UniqueM a ()
updateBEnv i bs = modify $ \s -> s{benv = bs, ubs = i : ubs s}

setChange :: UniqueM ann ()
setChange = modify $ \s -> s{change = True}

resetChange :: UniqueM ann ()
resetChange = modify $ \s -> s{change = False}

initUniqueST :: BindEnv a ->  UniqueST a
initUniqueST = UniqueST 0 mempty False mempty Nothing mempty

freshK, freshK' :: KVar -> UniqueM ann KVar
freshK k  = do
  setChange
  cached <- cache <$> get
  case M.lookup k cached of
    {- OPTIMIZATION: Only create one fresh occurence of ? per constraint environment. -}
    Just k' -> return  k'
    Nothing -> freshK' k

freshK' k = do
  i <- freshId <$> get
  modify (\s -> s{freshId = i + 1})
  let k' = KV $ gradIntSymbol i
  addK k k'
  addCache k k'
  return k'

addK :: KVar -> KVar -> UniqueM ann ()
addK key val =
  modify (\s -> s{kmap = M.insertWith (++) key [(val, uloc s)] (kmap s)})

-------------------------------------------------------------------------------
-- | expandWF -----------------------------------------------------------------
-------------------------------------------------------------------------------

expandWF :: (NFData a, Fixpoint a)
         => M.HashMap KVar [(KVar, Maybe SrcSpan)]
         -> M.HashMap KVar (WfC a)
         -> M.HashMap KVar (WfC a)
expandWF km ws
  = M.fromList
       ([(k, updateKVar k src w) | (i, w) <- gws, (kw, ks) <- km', kw == i, (k, src) <- ks]
        ++ kws)
  where
    (gws, kws)       = L.partition (isGWfc . snd) $ M.toList ws
    km'              = M.toList km

    updateKVar k src wfc = let wrft' = (\(v,s,_) -> (v,s,k)) $ wrft wfc in
      case wfc of
        GWfC{} -> wfc { wrft = wrft', wloc = (wloc wfc){gused = src} }
        WfC{}  -> wfc { wrft = wrft' }

-------------------------------------------------------------------------------
-- |  Substitute Gradual Solution ---------------------------------------------
-------------------------------------------------------------------------------

class Gradual a where
  gsubst :: GSol -> a -> a

instance Gradual Expr where
  gsubst (GSol env m) e   = mapGVars' (\(k, _) -> Just (fromMaybe (err k) (mknew k))) e
    where
      mknew k = So.elaborate "initBGind.mkPred" env $ fst <$> M.lookup k m
      err   k = errorstar ("gradual substitution: Cannot find " ++ showpp k)

instance Gradual Reft where
  gsubst su (Reft (x, e)) = Reft (x, gsubst su e)

instance Gradual SortedReft where
  gsubst su r = r {sr_reft = gsubst su (sr_reft r)}

instance Gradual (SimpC a) where
  gsubst su c = c {_crhs = gsubst su (_crhs c)}

instance Gradual (BindEnv a) where
  gsubst su = mapBindEnv (\_ (x, r, l) -> (x, gsubst su r, l))

instance Gradual v => Gradual (M.HashMap k v) where
  gsubst su = M.map (gsubst su)

instance Gradual (SInfo a) where
  gsubst su fi = fi { bs = gsubst su (bs fi)
                    , cm = gsubst su (cm fi)
                    }
