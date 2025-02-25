{-# LANGUAGE CPP                   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE RankNTypes            #-}

-- | This module has the functions that perform sort-checking, and related
-- operations on Fixpoint expressions and predicates.

module Language.Fixpoint.SortCheck  (
  -- * Sort Substitutions
    TVSubst
  , Env
  , mkSearchEnv

  -- * Checking Well-Formedness
  , checkSorted
  , checkSortedReft
  , checkSortedReftFull
  , checkSortFull
  , pruneUnsortedReft

  -- * Sort inference
  , sortExpr
  , checkSortExpr
  , exprSort
  , exprSortMaybe

  -- * Unify
  , unifyFast
  , unifySorts
  , unifyTo1
  , unifys

  -- * Apply Substitution
  , apply
  , defuncEApp

  -- * Exported Sorts
  , boolSort
  , strSort

  -- * Sort-Directed Transformations
  , ElabM
  , ElabParam (..)
  , Elaborate (..)
  , applySorts
  , elabApply
  , elabExpr
  , elabNumeric
  , unApply
  , unElab
  , unElabSortedReft
  , unApplySortedReft
  , unApplyAt
  , toInt

  -- * Predicates on Sorts
  , isFirstOrder
  , isMono

--  , runCM0
  ) where

--  import           Control.DeepSeq
import           Control.Exception (Exception, catch, try, throwIO)
import           Control.Monad
import           Control.Monad.Reader

import           Data.Bifunctor (first)
import qualified Data.IntMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.IORef
import qualified Data.List                 as L
import           Data.Maybe                (mapMaybe, fromMaybe, isJust)

import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Types hiding   (subst, GInfo(..), senv)
import qualified Language.Fixpoint.Types.Config as Cfg
import qualified Language.Fixpoint.Types.Visitor  as Vis
import qualified Language.Fixpoint.Smt.Theories   as Thy
import           Text.PrettyPrint.HughesPJ.Compat
import           Text.Printf

import           GHC.Stack
import qualified Language.Fixpoint.Types as F
import           System.IO.Unsafe (unsafePerformIO)

--import Debug.Trace as Debug

-- If set to 'True', enable precise logging via CallStacks.
debugLogs :: Bool
debugLogs = False

traced :: HasCallStack => (HasCallStack => String) -> String
traced str =
  if debugLogs
    then let prettified = prettyCallStack (popCallStack callStack)
         in str <> " (at " <> prettified <> ")"
    else str

--------------------------------------------------------------------------------
-- | Predicates on Sorts -------------------------------------------------------
--------------------------------------------------------------------------------
isMono :: Sort -> Bool
--------------------------------------------------------------------------------
isMono             = null . Vis.foldSort fv []
  where
    fv vs (FVar i) = i : vs
    fv vs _        = vs


--------------------------------------------------------------------------------
-- | Elaborate: make polymorphic instantiation explicit via casts,
--   make applications monomorphic for SMTLIB. This deals with
--   polymorphism by `elaborate`-ing all refinements except for
--   KVars. THIS IS NOW MANDATORY as sort-variables can be
--   instantiated to `int` and `bool`.
--------------------------------------------------------------------------------

type ElabM = Reader Cfg.ElabFlags

data ElabParam = ElabParam
  { epFlags :: Cfg.ElabFlags
  , epMsg   :: Located String
  , epEnv   :: SymEnv
  }

class Elaborate a where
  elaborate :: ElabParam -> a -> a


instance (Loc a) => Elaborate (SInfo a) where
  elaborate ep si = si
    { F.cm      = elaborate ep <$> F.cm      si
    , F.bs      = elaborate ep  $  F.bs      si
    , F.asserts = elaborate ep <$> F.asserts si
    }


instance (Elaborate e) => (Elaborate (Triggered e)) where
  elaborate ep t = elaborate ep <$> t

instance (Elaborate a) => (Elaborate (Maybe a)) where
  elaborate ep t = elaborate ep <$> t

instance Elaborate Sort where
  elaborate _ = go
   where
      go s | isString s = strSort
      go (FAbs i s)    = FAbs i  (go s)
      go (FFunc s1 s2) = funSort (go s1) (go s2)
      go (FApp s1 s2)  = FApp    (go s1) (go s2)
      go s             = s
      funSort :: Sort -> Sort -> Sort
      funSort = FApp . FApp funcSort

instance Elaborate AxiomEnv where
  elaborate ep ae = ae
    { aenvEqs   = elaborate ep (aenvEqs ae)
    -- MISSING SORTS OOPS, aenvSimpl = elaborate msg env (aenvSimpl ae)
    }

instance Elaborate Rewrite where
  elaborate ep rw = rw { smBody = skipElabExpr ep' (smBody rw) }
    where
      ep' = ep { epEnv = insertsSymEnv (epEnv ep) undefined }

instance Elaborate Equation where
  elaborate ep eq = eq { eqBody = skipElabExpr ep' (eqBody eq) }
    where
      ep' = ep { epEnv = insertsSymEnv (epEnv ep) (eqArgs eq) }

instance Elaborate Expr where
  elaborate (ElabParam ef msg env) =
    elabNumeric . elabApply env' . elabExpr (ElabParam ef msg env') . elabFMap . (if Cfg.elabSetBag ef then elabFSetBagZ3 else id)
      where
        env' = coerceEnv ef env

skipElabExpr :: ElabParam -> Expr -> Expr
skipElabExpr ep e = case elabExprE ep e of
  Left _   -> e
  Right e' -> elabNumeric . elabApply (epEnv ep) $ e'

instance Elaborate (Symbol, Sort) where
  elaborate ep (x, s) = (x, elaborate ep s)

instance Elaborate a => Elaborate [a]  where
  elaborate ep xs = elaborate ep <$> xs

elabNumeric :: Expr -> Expr
elabNumeric = Vis.mapExprOnExpr go
  where
    go (ETimes e1 e2)
      | exprSort "txn1" e1 == FReal
      , exprSort "txn2" e2 == FReal
      = ERTimes e1 e2
    go (EDiv   e1 e2)
      | exprSort ("txn3: " ++ showpp e1) e1 == FReal
      , exprSort "txn4" e2 == FReal
      = ERDiv   e1 e2
    go e
      = e

instance Elaborate SortedReft where
  elaborate ep (RR s (Reft (v, e))) = RR s (Reft (v, e'))
    where
      e'   = elaborate ep' e
      ep' = ep { epEnv = insertSymEnv v s (epEnv ep) }

instance (Loc a) => Elaborate (BindEnv a) where
  elaborate ep = mapBindEnv (\i (x, sr, l) -> (x, elaborate (ep { epMsg = msg' l i x sr }) sr, l))
    where
      msg' l i x sr = atLoc l (val (epMsg ep) ++ unwords [" elabBE", show i, show x, show sr])

instance (Loc a) => Elaborate (SimpC a) where
  elaborate ep c = c {_crhs = elaborate ep' (_crhs c) }
    where
      ep' = ep { epMsg = atLoc c (val $ epMsg ep) }

-----------------------------------------------------------------------------------
-- | Replace all finset/finmap/finbag theory operations with array-based encodings.
-----------------------------------------------------------------------------------

-- TODO abstract into a visitor for EApp?

-- TODO there's no actual elaboration happening here, just symbol renaming
elabFMap :: Expr -> Expr
elabFMap (EApp h@(EVar f) e)
  | f == Thy.mapDef        = EApp (EVar Thy.arrConstM) (elabFMap e)
  | otherwise              = EApp (elabFMap h) (elabFMap e)
elabFMap (EApp (EApp h@(EVar f) e1) e2)
  | f == Thy.mapSel        = EApp (EApp (EVar Thy.arrSelectM) (elabFMap e1)) (elabFMap e2)
  | otherwise              = EApp (EApp (elabFMap h) (elabFMap e1)) (elabFMap e2)
elabFMap (EApp (EApp (EApp h@(EVar f) e1) e2) e3)
  | f == Thy.mapSto        = EApp (EApp (EApp (EVar Thy.arrStoreM) (elabFMap e1)) (elabFMap e2)) (elabFMap e3)
  | otherwise              = EApp (EApp (EApp (elabFMap h) (elabFMap e1)) (elabFMap e2)) (elabFMap e3)
elabFMap (EApp e1 e2)      = EApp (elabFMap e1) (elabFMap e2)
elabFMap (ENeg e)          = ENeg (elabFMap e)
elabFMap (EBin b e1 e2)    = EBin b (elabFMap e1) (elabFMap e2)
elabFMap (EIte e1 e2 e3)   = EIte (elabFMap e1) (elabFMap e2) (elabFMap e3)
elabFMap (ECst e t)        = ECst (elabFMap e) t
elabFMap (ELam b e)        = ELam b (elabFMap e)
elabFMap (ETApp e t)       = ETApp (elabFMap e) t
elabFMap (ETAbs e t)       = ETAbs (elabFMap e) t
elabFMap (PAnd es)         = PAnd (elabFMap <$> es)
elabFMap (POr es)          = POr (elabFMap <$> es)
elabFMap (PNot e)          = PNot (elabFMap e)
elabFMap (PImp e1 e2)      = PImp (elabFMap e1) (elabFMap e2)
elabFMap (PIff e1 e2)      = PIff (elabFMap e1) (elabFMap e2)
elabFMap (PAtom r e1 e2)   = PAtom r (elabFMap e1) (elabFMap e2)
elabFMap (PAll   bs e)     = PAll bs (elabFMap e)
elabFMap (PExist bs e)     = PExist bs (elabFMap e)
elabFMap (PGrad  k su i e) = PGrad k su i (elabFMap e)
elabFMap (ECoerc a t e)    = ECoerc a t (elabFMap e)
elabFMap e                 = e

elabFSetBagZ3 :: Expr -> Expr
elabFSetBagZ3 (EApp h@(EVar f) e)
  | f == Thy.setEmpty         = EApp (EVar Thy.arrConstS) PFalse
  | f == Thy.setEmp           = PAtom Eq (EApp (EVar Thy.arrConstS) PFalse) (elabFSetBagZ3 e)
  | f == Thy.setSng           = EApp (EApp (EApp (EVar Thy.arrStoreS) (EApp (EVar Thy.arrConstS) PFalse)) (elabFSetBagZ3 e)) PTrue
  | f == Thy.setCom           = EApp (EVar Thy.arrMapNotS) (elabFSetBagZ3 e)
  | f == Thy.bagEmpty         = EApp (EVar Thy.arrConstB) (ECon (I 0))
  | otherwise                 = EApp (elabFSetBagZ3 h) (elabFSetBagZ3 e)
elabFSetBagZ3 (EApp (EApp h@(EVar f) e1) e2)
  | f == Thy.setMem           = EApp (EApp (EVar Thy.arrSelectS) (elabFSetBagZ3 e2)) (elabFSetBagZ3 e1)
  | f == Thy.setCup           = EApp (EApp (EVar Thy.arrMapOrS) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2)
  | f == Thy.setCap           = EApp (EApp (EVar Thy.arrMapAndS) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2)
  | f == Thy.setAdd           = EApp (EApp (EApp (EVar Thy.arrStoreS) (elabFSetBagZ3 e2)) (elabFSetBagZ3 e1)) PTrue
  -- A \ B == A /\ ~B == ~(A => B)
  | f == Thy.setDif           = EApp (EApp (EVar Thy.arrMapAndS) (elabFSetBagZ3 e1)) (EApp (EVar Thy.arrMapNotS) (elabFSetBagZ3 e2))
  | f == Thy.setSub           = PAtom Eq (EApp (EVar Thy.arrConstS) PTrue) (EApp (EApp (EVar Thy.arrMapImpS) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2))
  | f == Thy.bagCount         = EApp (EApp (EVar Thy.arrSelectB) (elabFSetBagZ3 e2)) (elabFSetBagZ3 e1)
  | f == Thy.bagSng           = EApp (EApp (EApp (EVar Thy.arrStoreB) (EApp (EVar Thy.arrConstB) (ECon (I 0)))) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2)
  | f == Thy.bagCup           = EApp (EApp (EVar Thy.arrMapPlusB) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2)
  | f == Thy.bagSub           = PAtom Eq (EApp (EVar Thy.arrConstS) PTrue) (EApp (EApp (EVar Thy.arrMapLeB) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2))
  | f == Thy.bagMax           = EApp (EApp (EApp (EVar Thy.arrMapIteB) (EApp (EApp (EVar Thy.arrMapGtB) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2))) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2)
  | f == Thy.bagMin           = EApp (EApp (EApp (EVar Thy.arrMapIteB) (EApp (EApp (EVar Thy.arrMapLeB) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2))) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2)
  | otherwise                 = EApp (EApp (elabFSetBagZ3 h) (elabFSetBagZ3 e1)) (elabFSetBagZ3 e2)
elabFSetBagZ3 (EApp e1 e2)      = EApp (elabFSetBagZ3 e1) (elabFSetBagZ3 e2)
elabFSetBagZ3 (ENeg e)          = ENeg (elabFSetBagZ3 e)
elabFSetBagZ3 (EBin b e1 e2)    = EBin b (elabFSetBagZ3 e1) (elabFSetBagZ3 e2)
elabFSetBagZ3 (EIte e1 e2 e3)   = EIte (elabFSetBagZ3 e1) (elabFSetBagZ3 e2) (elabFSetBagZ3 e3)
elabFSetBagZ3 (ECst e t)        = ECst (elabFSetBagZ3 e) t
elabFSetBagZ3 (ELam b e)        = ELam b (elabFSetBagZ3 e)
elabFSetBagZ3 (ETApp e t)       = ETApp (elabFSetBagZ3 e) t
elabFSetBagZ3 (ETAbs e t)       = ETAbs (elabFSetBagZ3 e) t
elabFSetBagZ3 (PAnd es)         = PAnd (elabFSetBagZ3 <$> es)
elabFSetBagZ3 (POr es)          = POr (elabFSetBagZ3 <$> es)
elabFSetBagZ3 (PNot e)          = PNot (elabFSetBagZ3 e)
elabFSetBagZ3 (PImp e1 e2)      = PImp (elabFSetBagZ3 e1) (elabFSetBagZ3 e2)
elabFSetBagZ3 (PIff e1 e2)      = PIff (elabFSetBagZ3 e1) (elabFSetBagZ3 e2)
elabFSetBagZ3 (PAtom r e1 e2)   = PAtom r (elabFSetBagZ3 e1) (elabFSetBagZ3 e2)
elabFSetBagZ3 (PAll   bs e)     = PAll bs (elabFSetBagZ3 e)
elabFSetBagZ3 (PExist bs e)     = PExist bs (elabFSetBagZ3 e)
elabFSetBagZ3 (PGrad  k su i e) = PGrad k su i (elabFSetBagZ3 e)
elabFSetBagZ3 (ECoerc a t e)    = ECoerc a t (elabFSetBagZ3 e)
elabFSetBagZ3 e                 = e

--------------------------------------------------------------------------------
-- | 'elabExpr' adds "casts" to decorate polymorphic instantiation sites.
--------------------------------------------------------------------------------
elabExpr :: ElabParam -> Expr -> Expr
elabExpr ep e = case elabExprE ep e of
  Left ex  -> die ex
  Right e' -> F.notracepp ("elabExp " ++ showpp e) e'

elabExprE :: ElabParam -> Expr -> Either Error Expr
elabExprE (ElabParam ef msg env) e =
  case runCM0 (srcSpan msg) (Just ef) $ do
    (!e', _) <- elab (env, envLookup) e
    finalThetaRef <- asks chTVSubst
    finalTheta <- liftIO $ readIORef finalThetaRef
    return (applyExpr finalTheta e') of
    Left (ChError f') ->
      let e' = f' ()
       in Left $ err (srcSpan e') (d (val e'))
    Right s  -> Right s
  where
    sEnv = seSort env
    envLookup = (`lookupSEnvWithDistance` sEnv)
    d m  = vcat [ "elaborate" <+> text (val msg) <+> "failed on:"
                , nest 4 (pprint e)
                , "with error"
                , nest 4 (text m)
                , "in environment"
                , nest 4 (pprint $ subEnv sEnv e)
                ]

--------------------------------------------------------------------------------
-- | 'elabApply' replaces all direct function calls indirect calls via `apply`
--------------------------------------------------------------------------------
elabApply :: SymEnv -> Expr -> Expr
elabApply env = go
  where
    go e                  = case splitArgs e of
                             (e', []) -> step e'
                             (f , es) -> defuncEApp env (go f) (first go <$> es)
    step (PAnd [])        = PTrue
    step (POr [])         = PFalse
    step (ENeg e)         = ENeg (go  e)
    step (EBin o e1 e2)   = EBin o (go e1) (go e2)
    step (EIte e1 e2 e3)  = EIte (go e1) (go e2) (go e3)
    step (ECst e t)       = ECst (go e) t
    step (PAnd ps)        = PAnd (go <$> ps)
    step (POr ps)         = POr  (go <$> ps)
    step (PNot p)         = PNot (go p)
    step (PImp p q)       = PImp (go p) (go q)
    step (PIff p q)       = PIff (go p) (go q)
    step (PExist bs p)    = PExist bs (go p)
    step (PAll   bs p)    = PAll   bs (go p)
    step (PAtom r e1 e2)  = PAtom r (go e1) (go e2)
    step e@EApp {}        = go e
    step (ELam b e)       = ELam b       (go e)
    step (ECoerc a t e)   = ECoerc a t   (go e)
    step (PGrad k su i e) = PGrad k su i (go e)
    step e@PKVar{}        = e
    step e@ESym{}         = e
    step e@ECon{}         = e
    step e@EVar{}         = e
    -- ETApp, ETAbs, PAll, PExist
    step e                = error $ "TODO elabApply: " ++ showpp e

--------------------------------------------------------------------------------
-- | Sort Inference ------------------------------------------------------------
--------------------------------------------------------------------------------
sortExpr :: SrcSpan -> SEnv Sort -> Expr -> Sort
sortExpr l γ e = case runCM0 l Nothing (checkExpr f e) of
    Left (ChError f') -> die $ err l (d (val (f' ())))
    Right s -> s
  where
    f   = (`lookupSEnvWithDistance` γ)
    d m = vcat [ "sortExpr failed on expression:"
               , nest 4 (pprint e)
               , "with error:"
               , nest 4 (text m)
               , "in environment"
               , nest 4 (pprint γ)
               ]

checkSortExpr :: SrcSpan -> SEnv Sort -> Expr -> Maybe Sort
checkSortExpr sp γ e = case runCM0 sp Nothing (checkExpr f e) of
    Left _   -> Nothing
    Right s  -> Just s
  where
    f x  = case lookupSEnv x γ of
            Just z  -> Found z
            Nothing -> Alts []

subEnv :: (Subable e) => SEnv a -> e -> SEnv a
subEnv g e = intersectWithSEnv const g g'
  where
    g' = fromListSEnv $ (, ()) <$> syms e


--------------------------------------------------------------------------------
-- | Checking Refinements ------------------------------------------------------
--------------------------------------------------------------------------------

-- | Types used throughout checker
type CheckM = ReaderT ChState IO

-- We guard errors with a lambda to prevent accidental eager
-- evaluation of the payload. This module is using -XStrict.
-- See also Note [Lazy error messages].
newtype ChError  = ChError (() -> Located String)

instance Show ChError where
  show (ChError f) = show (f ())
instance Exception ChError where

data ChState = ChS { chCount :: IORef Int
                   , chSpan  :: SrcSpan
                   , chElabF :: Cfg.ElabFlags
                   , chTVSubst :: IORef (Maybe TVSubst)
                   }

type Env      = Symbol -> SESearch Sort
type ElabEnv  = (SymEnv, Env)


--------------------------------------------------------------------------------
mkSearchEnv :: SEnv a -> Symbol -> SESearch a
--------------------------------------------------------------------------------
mkSearchEnv env x = lookupSEnvWithDistance x env

-- withError :: CheckM a -> ChError -> CheckM a
-- act `withError` e' = act `catchError` (\e -> throwError (atLoc e (val e ++ "\n  because\n" ++ val e')))

withError :: HasCallStack => CheckM a -> String -> CheckM a
act `withError` msg = do
  r <- ask
  liftIO $ runReaderT act r `catch`
    (\(ChError f) ->
      throwIO $ ChError $ \_ ->
        let e = f ()
         in atLoc e (val e ++ "\n  because\n" ++ msg)
    )

-- XXX: Why start at 42?
{-# NOINLINE varCounterRef #-}
varCounterRef :: IORef Int
varCounterRef = unsafePerformIO $ newIORef 42

-- XXX: Since 'varCounterRef' was made global, this
-- function is not referentially transparent.
-- Each evaluation of the function starts with a different
-- value of counter.
runCM0 :: SrcSpan -> Maybe Cfg.ElabFlags -> CheckM a -> Either ChError a
runCM0 sp mef act = unsafePerformIO $ do
  ref <- newIORef Nothing
  try (runReaderT act (ChS varCounterRef sp (fromMaybe (Cfg.ElabFlags False) mef) ref))

fresh :: CheckM Int
fresh = do
  rn <- asks chCount
  liftIO $ atomicModifyIORef' rn $ \n -> (n+1, n)

--------------------------------------------------------------------------------
-- | Checking Refinements ------------------------------------------------------
--------------------------------------------------------------------------------
checkSortedReft :: SEnv SortedReft -> [Symbol] -> SortedReft -> Maybe Doc
checkSortedReft env xs sr = applyNonNull Nothing oops unknowns
  where
    oops                  = Just . (text "Unknown symbols:" <+>) . toFix
    unknowns              = [ x | x <- syms sr, x `notElem` v : xs, not (x `memberSEnv` env)]
    Reft (v,_)            = sr_reft sr

checkSortedReftFull :: Checkable a => SrcSpan -> SEnv SortedReft -> a -> ElabM (Maybe Doc)
checkSortedReftFull sp γ t =
  do ef <- ask
     pure $ case runCM0 sp (Just ef) (check γ' t) of
              Left (ChError f)  -> Just (text (val (f ())))
              Right _ -> Nothing
  where
    γ' = sr_sort <$> γ

checkSortFull :: Checkable a => SrcSpan -> SEnv SortedReft -> Sort -> a -> ElabM (Maybe Doc)
checkSortFull sp γ s t =
  do ef <- ask
     pure $ case runCM0 sp (Just ef) (checkSort γ' s t) of
              Left (ChError f)  -> Just (text (val (f ())))
              Right _ -> Nothing
  where
      γ' = sr_sort <$> γ

checkSorted :: Checkable a => SrcSpan -> SEnv Sort -> a -> ElabM (Maybe Doc)
checkSorted sp γ t =
  do ef <- ask
     pure $ case runCM0 sp (Just ef) (check γ t) of
              Left (ChError f) -> Just (text (val (f ())))
              Right _  -> Nothing

pruneUnsortedReft :: SEnv Sort -> Templates -> SortedReft -> SortedReft
pruneUnsortedReft _ t r
  | isEmptyTemplates t
  = r
pruneUnsortedReft γ t (RR s (Reft (v, p)))
  | isAnyTemplates t
  -- this is the old code that checks everything
  = RR s (Reft (v, tx filterAny p))
  | otherwise
  = RR s (Reft (v, tx (filter filterWithTemplate) p))
  where
    filterAny = mapMaybe (checkPred' f)
    filterWithTemplate e =  not (matchesTemplates t e) || isJust (checkPred' f e)
    tx f' = pAnd . f' . conjuncts
    f    = (`lookupSEnvWithDistance` γ')
    γ'   = insertSEnv v s γ
    -- wmsg t r = "WARNING: prune unsorted reft:\n" ++ showFix r ++ "\n" ++ t

checkPred' :: Env -> Expr -> Maybe Expr
checkPred' f p = res -- traceFix ("checkPred: p = " ++ showFix p) $ res
  where
    res        = case runCM0 dummySpan Nothing (checkPred f p) of
                   Left _err -> notracepp ("Removing" ++ showpp p) Nothing
                   Right _   -> Just p

class Checkable a where
  check     :: SEnv Sort -> a -> CheckM ()
  checkSort :: SEnv Sort -> Sort -> a -> CheckM ()

  checkSort γ _ = check γ

instance Checkable Expr where
  check γ e =
    do ef <- asks chElabF
       _ <- checkExpr (`lookupSEnvWithDistance` coerceSortEnv ef γ) e
       pure ()

  checkSort γ s e =
    do ef <- asks chElabF
       _ <- checkExpr (`lookupSEnvWithDistance` coerceSortEnv ef γ)
                      (ECst e (if Cfg.elabSetBag ef then coerceSetBagToArray s' else s'))
       pure ()
   where
      s' = coerceMapToArray s

instance Checkable SortedReft where
  check γ (RR s (Reft (v, ra))) = check γ' ra
   where
     γ' = insertSEnv v s γ

--------------------------------------------------------------------------------
-- | Checking Expressions ------------------------------------------------------
--------------------------------------------------------------------------------
checkExpr                   :: Env -> Expr -> CheckM Sort
checkExpr _ (ESym _)        = return strSort
checkExpr _ (ECon (I _))    = return FInt
checkExpr _ (ECon (R _))    = return FReal
checkExpr _ (ECon (L _ s))  = return s
checkExpr f (EVar x)        = checkSym f x
checkExpr f (ENeg e)        = checkNeg f e
checkExpr f (EBin o e1 e2)  = checkOp f e1 o e2
checkExpr f (EIte p e1 e2)  = checkIte f p e1 e2
checkExpr f (ECst e t)      = checkCst f t e
checkExpr f (EApp g e)      = checkApp f Nothing g e
checkExpr f (PNot p)        = checkPred f p >> return boolSort
checkExpr f (PImp p p')     = mapM_ (checkPred f) [p, p'] >> return boolSort
checkExpr f (PIff p p')     = mapM_ (checkPred f) [p, p'] >> return boolSort
checkExpr f (PAnd ps)       = mapM_ (checkPred f) ps >> return boolSort
checkExpr f (POr ps)        = mapM_ (checkPred f) ps >> return boolSort
checkExpr f (PAtom r e e')  = checkRel f r e e' >> return boolSort
checkExpr _ PKVar{}         = return boolSort
checkExpr f (PGrad _ _ _ e) = checkPred f e >> return boolSort

checkExpr f (PAll  bs e )   = checkExpr (addEnv f bs) e
checkExpr f (PExist bs e)   = checkExpr (addEnv f bs) e
checkExpr f (ELam (x,t) e)  = FFunc t <$> checkExpr (addEnv f [(x,t)]) e
checkExpr f (ECoerc s t e)  = checkExpr f (ECst e s) >> return t
checkExpr _ (ETApp _ _)     = error "SortCheck.checkExpr: TODO: implement ETApp"
checkExpr _ (ETAbs _ _)     = error "SortCheck.checkExpr: TODO: implement ETAbs"

addEnv :: Eq a => (a -> SESearch b) -> [(a, b)] -> a -> SESearch b
addEnv f bs x
  = case L.lookup x bs of
      Just s  -> Found s
      Nothing -> f x

--------------------------------------------------------------------------------
-- | Elaborate expressions with types to make polymorphic instantiation explicit.
--------------------------------------------------------------------------------
{-# SCC elab #-}
elab :: ElabEnv -> Expr -> CheckM (Expr, Sort)
--------------------------------------------------------------------------------
elab f@(!_, !g) e@(EBin !o !e1 !e2) = do
  (!e1', !s1) <- elab f e1
  (!e2', !s2) <- elab f e2
  !s <- checkOpTy g e s1 s2
  let !result = EBin o (eCst e1' s1) (eCst e2' s2)
  return (result, s)

elab !f (EApp !e1 !e2) = do
  (!e1', !s1, !e2', !s2, !s) <- elabEApp f e1 e2
  let !e = eAppC s (eCst e1' s1) (eCst e2' s2)
  return (e, s)

elab !_ e@(ESym _) =
  return (e, strSort)

elab !_ e@(ECon (I _)) =
  return (e, FInt)

elab !_ e@(ECon (R _)) =
  return (e, FReal)

elab !_ e@(ECon (L _ !s)) =
  return (e, s)

elab !_ e@(PKVar _ _) =
  return (e, boolSort)

elab !f (PGrad !k !su !i !e) = do
  (!e', !_) <- elab f e
  return (PGrad k su i e', boolSort)

elab (!_, !f) e@(EVar !x) = do
  !cs <- checkSym f x
  return (e, cs)

elab !f (ENeg !e) = do
  (!e', !s) <- elab f e
  return (ENeg e', s)

elab f@(!_,!g) (ECst (EIte !p !e1 !e2) !t) = do
  (!p', !_)   <- elab f p
  (!e1', !s1) <- elab f (eCst e1 t)
  (!e2', !s2) <- elab f (eCst e2 t)
  !s          <- checkIteTy g p e1' e2' s1 s2
  return (EIte p' (eCst e1' s) (eCst e2' s), t)

elab f@(!_,!g) (EIte !p !e1 !e2) = do
  !t <- getIte g e1 e2
  (!p', !_)   <- elab f p
  (!e1', !s1) <- elab f (eCst e1 t)
  (!e2', !s2) <- elab f (eCst e2 t)
  !s          <- checkIteTy g p e1' e2' s1 s2
  return (EIte p' (eCst e1' s) (eCst e2' s), s)

elab !f (ECst !e !t) = do
  (!e', !_) <- elab f e
  return (eCst e' t, t)

elab !f (PNot !p) = do
  (!e', !_) <- elab f p
  return (PNot e', boolSort)

elab !f (PImp !p1 !p2) = do
  (!p1', !_) <- elab f p1
  (!p2', !_) <- elab f p2
  return (PImp p1' p2', boolSort)

elab !f (PIff !p1 !p2) = do
  (!p1', !_) <- elab f p1
  (!p2', !_) <- elab f p2
  return (PIff p1' p2', boolSort)

elab !f (PAnd !ps) = do
  !ps' <- mapM (elab f) ps
  return (PAnd (fst <$> ps'), boolSort)

elab !f (POr !ps) = do
  !ps' <- mapM (elab f) ps
  return (POr (fst <$> ps'), boolSort)

elab f@(!_,!g) e@(PAtom !eq !e1 !e2) | eq == Eq || eq == Ne = do
  !t1        <- checkExpr g e1
  !t2        <- checkExpr g e2
  (!t1',!t2') <- unite g e t1 t2 `withError` errElabExpr e
  !e1'       <- elabAs f t1' e1
  !e2'       <- elabAs f t2' e2
  !e1''      <- eCstAtom f e1' t1'
  !e2''      <- eCstAtom f e2' t2'
  return (PAtom eq e1'' e2'', boolSort)

elab !f (PAtom !r !e1 !e2)
  | r == Ueq || r == Une = do
  (!e1', !_) <- elab f e1
  (!e2', !_) <- elab f e2
  return (PAtom r e1' e2', boolSort)

elab f@(!env,!_) (PAtom !r !e1 !e2) = do
  !e1' <- uncurry (toInt env) <$> elab f e1
  !e2' <- uncurry (toInt env) <$> elab f e2
  return (PAtom r e1' e2', boolSort)

elab !f (PExist !bs !e) = do
  (!e', !s) <- elab (elabAddEnv f bs) e
  !ef <- asks chElabF
  let !bs' = elaborate (ElabParam ef "PExist Args" mempty) bs
  return (PExist bs' e', s)

elab !f (PAll !bs !e) = do
  (!e', !s) <- elab (elabAddEnv f bs) e
  !ef <- asks chElabF
  let !bs' = elaborate (ElabParam ef "PAll Args" mempty) bs
  return (PAll bs' e', s)

elab !f (ELam (!x,!t) !e) = do
  (!e', !s) <- elab (elabAddEnv f [(x, t)]) e
  !ef <- asks chElabF
  let !t' = elaborate (ElabParam ef "ELam Arg" mempty) t
  return (ELam (x, t') (eCst e' s), FFunc t s)

elab !f (ECoerc !s !t !e) = do
  (!e', !_) <- elab f e
  return (ECoerc s t e', t)

elab !_ (ETApp _ _) =
  error "SortCheck.elab: TODO: implement ETApp"
elab !_ (ETAbs _ _) =
  error "SortCheck.elab: TODO: implement ETAbs"

-- | 'eCstAtom' is to support tests like `tests/pos/undef00.fq`
eCstAtom :: ElabEnv -> Expr -> Sort -> CheckM Expr
eCstAtom f@(sym,g) (EVar x) t
  | Found s <- g x
  , isUndef s
  , not (isNum sym t) = (`ECst` t) <$> elabAs f t (EApp (eVar tyCastName) (eVar x))
eCstAtom _ e t = return (ECst e t)

isUndef :: Sort -> Bool
isUndef s = case bkAbs s of
  (is, FVar j) -> j `elem` is
  _            -> False


elabAddEnv :: Eq a => (t, a -> SESearch b) -> [(a, b)] -> (t, a -> SESearch b)
elabAddEnv (g, f) bs = (g, addEnv f bs)

elabAs :: ElabEnv -> Sort -> Expr -> CheckM Expr
elabAs f t e = notracepp _msg <$> go e
  where
    _msg  = "elabAs: t = " ++ showpp t ++ "; e = " ++ showpp e
    go (EApp e1 e2) = elabAppAs f t e1 e2
    go e'           = fst <$> elab f e'

-- DUPLICATION with `checkApp'`
elabAppAs :: ElabEnv -> Sort -> Expr -> Expr -> CheckM Expr
elabAppAs env@(_, f) t g e = do
  gT       <- checkExpr f g
  eT       <- checkExpr f e
  (iT, oT, isu) <- checkFunSort gT
  let ge    = Just (EApp g e)
  su       <- unifyMany f ge isu [oT, iT] [t, eT]
  let tg    = apply su gT
  g'       <- elabAs env tg g
  let te    = apply su eT
  e'       <- elabAs env te e
  pure     $ EApp (ECst g' tg) (ECst e' te)

elabEApp  :: ElabEnv -> Expr -> Expr -> CheckM (Expr, Sort, Expr, Sort, Sort)
elabEApp f@(_, g) e1 e2 = do
  (e1', s1)     <- {- notracepp ("elabEApp: e1 = " ++ show e1) <$> -} elab f e1
  (e2', s2)     <- {- notracepp ("elabEApp: e2 = " ++ show e2) <$> -} elab f e2
  (e1'', e2'', s1', s2', s) <- elabAppSort g e1' e2' s1 s2
  return           (e1'', s1', e2'', s2', s)

elabAppSort :: Env -> Expr -> Expr -> Sort -> Sort -> CheckM (Expr, Expr, Sort, Sort, Sort)
elabAppSort f e1 e2 s1 s2 = do
  let e            = Just (EApp e1 e2)
  (sIn, sOut, su) <- checkFunSort s1
  su'             <- unify1 f e su sIn s2
  composeTVSubst (Just su)
  composeTVSubst (Just su')
  return (e1 , e2, apply su' s1, apply su' s2, apply su' sOut)


--------------------------------------------------------------------------------
-- | defuncEApp monomorphizes function applications.
--------------------------------------------------------------------------------
defuncEApp :: SymEnv -> Expr -> [(Expr, Sort)] -> Expr
defuncEApp _   e [] = e
defuncEApp env e es = eCst (L.foldl' makeApplication e' es') (snd $ last es)
  where
    (e', es')       = takeArgs (seTheory env) e es

takeArgs :: SEnv TheorySymbol -> Expr -> [(Expr, a)] -> (Expr, [(Expr, a)])
takeArgs env e es =
  case Thy.isSmt2App env e of
    Just n  -> let (es1, es2) = splitAt n es
               in (eApps e (fst <$> es1), es2)
    Nothing -> (e, es)

-- 'e1' is the function, 'e2' is the argument, 's' is the OUTPUT TYPE
makeApplication :: Expr -> (Expr, Sort) -> Expr
makeApplication e1 (e2, s) =
  ECst (EApp (EApp f e1) e2) s
  where
    f  = {- notracepp ("makeApplication: " ++ showpp (e2, t2)) $ -} applyAt t2 s
    t2 = exprSort "makeAppl" e2

applyAt :: Sort -> Sort -> Expr
applyAt s t = ECst (EVar applyName) (FFunc s t)

-- JUST make "toInt" call "makeApplication" also, so they are wrapped in apply
-- MAY CAUSE CRASH (apply-on-apply) so rig `isSmt2App` to treat `apply` as SPECIAL.

-- TODO: proper toInt
toInt :: SymEnv -> Expr -> Sort -> Expr
toInt env e s
  | isSmtInt  = e
  | otherwise = ECst (EApp f (ECst e s)) FInt
  where
    isSmtInt  = isNum env s
    f         = toIntAt s

isNum :: SymEnv -> Sort -> Bool
isNum env s = case sortSmtSort False (seData env) s of
  SInt    -> True
  SString -> True
  SReal   -> True
  _       -> False

toIntAt :: Sort -> Expr
toIntAt s = ECst (EVar toIntName) (FFunc s FInt)

unElab :: Expr -> Expr
unElab = Vis.stripCasts . unApply

unElabSortedReft :: SortedReft -> SortedReft
unElabSortedReft sr = sr { sr_reft = mapPredReft unElab (sr_reft sr) }

unApplySortedReft :: SortedReft -> SortedReft
unApplySortedReft sr = sr { sr_reft = mapPredReft unApply (sr_reft sr) }

unApply :: Expr -> Expr
unApply = Vis.mapExprOnExpr go
  where
    go (ECst (EApp (EApp f e1) e2) _)
      | Just _ <- unApplyAt f = EApp e1 e2
    go (ELam (x,s) e)         = ELam (x, Vis.mapSort go' s) e
    go e                      = e

    go' (FApp (FApp fs t1) t2) | fs == funcSort
          = FFunc t1 t2
    go' t = t


unApplyAt :: Expr -> Maybe Sort
unApplyAt (ECst (EVar f) t@FFunc{})
  | f == applyName = Just t
unApplyAt _        = Nothing


splitArgs :: Expr -> (Expr, [(Expr, Sort)])
splitArgs = go []
  where
    go acc (ECst (EApp e1 e) s) = go ((e, s) : acc) e1
    go _   e@EApp{}             = errorstar $ "UNEXPECTED: splitArgs: EApp without output type: " ++ showpp e
    go acc e                    = (e, acc)

--------------------------------------------------------------------------------
{- | [NOTE:apply-monomorphization]

     Because SMTLIB does not support higher-order functions,
     all _non-theory_ function applications

        EApp e1 e2

     are represented, in SMTLIB, as

        (EApp (EApp apply e1) e2)

     where 'apply' is 'ECst (EVar "apply") t' and
           't'     is 'FFunc a b'
           'a','b' are the sorts of 'e2' and 'e1 e2' respectively.

     Note that *all polymorphism* goes through this machinery.

     Just before sending to the SMT solver, we use the cast 't'
     to generate a special 'apply_at_t' symbol.

     To let us do the above, we populate 'SymEnv' with the _set_
     of all sorts at which 'apply' is used, computed by 'applySorts'.
 -}

{- | [NOTE:coerce-apply] -- related to [NOTE:apply-monomorphism]

Haskell's GADTs cause a peculiar problem illustrated below:

```haskell
data Field a where
  FInt  :: Field Int
  FBool :: Field Bool

{-@ reflect proj @-}
proj :: Field a -> a -> a
proj fld x = case fld of
               FInt  -> 1 + x
               FBool -> not b
```

**The Problem**

The problem is you cannot encode the body of `proj` as a well-sorted refinement:

```haskell
    if is$FInt fld
        then (1 + (coerce (a ~ Int)  x))
        else (not (coerce (a ~ Bool) x))
```

The catch is that `x` is being used BOTH as `Int` and as `Bool`
which is not supported in SMTLIB.

**Approach: Uninterpreted Functions**

We encode `coerce` as an explicit **uninterpreted function**:

```haskell
    if is$FInt fld
        then (1 + (coerce@(a -> int)  x))
        else (not (coerce@(a -> bool) x))
```

where we define, extra constants in the style of `apply`

```haskell
   constant coerce@(a -> int ) :: a -> int
   constant coerce@(a -> bool) :: a -> int
```

However, it would not let us verify:


```haskell
{-@ reflect unwrap @-}
unwrap :: Field a -> a -> a
unwrap fld x = proj fld x

{-@ test :: _ -> TT @-}
test =  unwrap FInt  4    == 5
     && unwrap FBool True == False
```

because we'd get

```haskell
  unwrap FInt 4 :: { if is$FInt FInt then (1 + coerce_int_int 4) else ...  }
```

and the UIF nature of `coerce_int_int` renders the VC invalid.

**Solution: Eliminate Trivial Coercions**

HOWEVER, the solution here, may simply be to use UIFs when the
coercion is non-trivial (e.g. `a ~ int`) but to eschew them when
they are trivial. That is we would encode:

| Expr                   | SMTLIB             |
|:-----------------------|:-------------------|
| `coerce (a ~ int) x`   | `coerce_a_int x`   |
| `coerce (int ~ int) x` | `x`                |

which, I imagine is what happens _somewhere_ inside GHC too?

-}

--------------------------------------------------------------------------------
applySorts :: Vis.Foldable t => t -> [Sort]
--------------------------------------------------------------------------------
applySorts = {- tracepp "applySorts" . -} (defs ++) . Vis.fold vis () []
  where
    defs   = [FFunc t1 t2 | t1 <- basicSorts, t2 <- basicSorts]
    vis    = (Vis.defaultFolder :: Vis.Folder [KVar] t) { Vis.accExpr = go }
    go _ (EApp (ECst (EVar f) t) _)   -- get types needed for [NOTE:apply-monomorphism]
           | f == applyName
           = [t]
    go _ (ECoerc t1 t2 _)             -- get types needed for [NOTE:coerce-apply]
           = [FFunc t1 t2]
    go _ _ = []

--------------------------------------------------------------------------------
-- | Expressions sort  ---------------------------------------------------------
--------------------------------------------------------------------------------
exprSort :: String -> Expr -> Sort
exprSort msg e = fromMaybe (panic err') (exprSortMaybe e)
  where
    err'        = printf "exprSort [%s] on unexpected expression %s" msg (show e)

exprSortMaybe :: Expr -> Maybe Sort
exprSortMaybe = go
  where
    go (ECst _ s) = Just s
    go (ELam (_, sx) e) = FFunc sx <$> go e
    go (EApp e ex)
      | Just (FFunc sx s) <- genSort <$> go e
      = maybe s (`apply` s) . (`unifySorts` sx) <$> go ex
    go _ = Nothing

genSort :: Sort -> Sort
genSort (FAbs _ t) = genSort t
genSort t          = t

unite :: Env -> Expr -> Sort -> Sort -> CheckM (Sort, Sort)
unite f e t1 t2 = do
  su <- unifys f (Just e) [t1] [t2]
  return (apply su t1, apply su t2)

throwErrorAt :: String -> CheckM a
throwErrorAt ~err' = do -- Lazy pattern needed because we use LANGUAGE Strict in this module
                        -- See Note [Lazy error messages]
  sp <- asks chSpan
  liftIO $ throwIO (ChError (\_ -> atLoc sp err'))

-- Note [Lazy error messages]
--
-- We don't want to construct error messages early, or
-- we might trigger some expensive computation of editDistance
-- when no error has actually occurred yet.

-- | Helper for checking symbol occurrences
checkSym :: Env -> Symbol -> CheckM Sort
checkSym f x = case f x of
  Found s -> refreshNegativeTyVars s >>= instantiate
  Alts xs -> throwErrorAt (errUnboundAlts x xs)

-- Negative type variables are implictly universally quantified type variables
refreshNegativeTyVars :: Sort -> CheckM Sort
refreshNegativeTyVars s = do
    let negativeSorts = negSort s
    freshVars <- mapM pair $ S.toList negativeSorts
    pure $ foldr (uncurry subst) s freshVars
  where
    pair i = do
      f <- fresh
      pure (i, FVar f)

    negSort (FVar i) | i < 0 = S.singleton i
    negSort (FAbs _ s')      = negSort s'
    negSort (FFunc s1 s2)    = negSort s1 `S.union` negSort s2
    negSort (FApp s1 s2)     = negSort s1 `S.union` negSort s2
    negSort _                = S.empty

-- | Helper for checking if-then-else expressions
checkIte :: Env -> Expr -> Expr -> Expr -> CheckM Sort
checkIte f p e1 e2 = do
  checkPred f p
  t1 <- checkExpr f e1
  t2 <- checkExpr f e2
  checkIteTy f p e1 e2 t1 t2

getIte :: Env -> Expr -> Expr -> CheckM Sort
getIte f e1 e2 = do
  t1 <- checkExpr f e1
  t2 <- checkExpr f e2
  (`apply` t1) <$> unifys f Nothing [t1] [t2]

checkIteTy :: Env -> Expr -> Expr -> Expr -> Sort -> Sort -> CheckM Sort
checkIteTy f p e1 e2 t1 t2 =
  ((`apply` t1) <$> unifys f e' [t1] [t2]) `withError` errIte e1 e2 t1 t2
  where
    e' = Just (EIte p e1 e2)

-- | Helper for checking cast expressions
checkCst :: Env -> Sort -> Expr -> CheckM Sort
checkCst f t (EApp g e)
  = checkApp f (Just t) g e
checkCst f t e
  = do t' <- checkExpr f e
       su <- unifys f (Just e) [t] [t'] `withError` errCast e t' t
       pure (apply su t)

checkApp :: Env -> Maybe Sort -> Expr -> Expr -> CheckM Sort
checkApp f to g es
  = snd <$> checkApp' f to g es

checkExprAs :: Env -> Sort -> Expr -> CheckM Sort
checkExprAs f t (EApp g e)
  = checkApp f (Just t) g e
checkExprAs f t e
  = do t' <- checkExpr f e
       θ  <- unifys f (Just e) [t'] [t]
       pure $ apply θ t

-- | Helper for checking uninterpreted function applications
-- | Checking function application should be curried, e.g.
-- | fromJust :: Maybe a -> a, f :: Maybe (b -> b), x: c |- fromJust f x
--   RJ: The above comment makes no sense to me :(

-- DUPLICATION with 'elabAppAs'
checkApp' :: Env -> Maybe Sort -> Expr -> Expr -> CheckM (TVSubst, Sort)
checkApp' f to g e = do
  gt       <- checkExpr f g
  et       <- checkExpr f e
  (it, ot, isu) <- checkFunSort gt
  let ge    = Just (EApp g e)
  su        <- unifyMany f ge isu [it] [et]
  let t     = apply su ot
  case to of
    Nothing    -> return (su, t)
    Just t'    -> do θ' <- unifyMany f ge su [t] [t']
                     let ti = apply θ' et
                     _ <- checkExprAs f ti e
                     return (θ', apply θ' t)


-- | Helper for checking binary (numeric) operations
checkNeg :: Env -> Expr -> CheckM Sort
checkNeg f e = do
  t <- checkExpr f e
  checkNumeric f t >> return t

checkOp :: Env -> Expr -> Bop -> Expr -> CheckM Sort
checkOp f e1 o e2
  = do t1 <- checkExpr f e1
       t2 <- checkExpr f e2
       checkOpTy f (EBin o e1 e2) t1 t2


checkOpTy :: Env -> Expr -> Sort -> Sort -> CheckM Sort
checkOpTy _ _ FInt FInt
  = return FInt

checkOpTy _ _ FReal FReal
  = return FReal
-- Coercing int to real is somewhat suspicious, but z3 seems
-- to be ok with it
checkOpTy _ _ FInt  FReal
  = return FReal
checkOpTy _ _ FReal FInt
  = return FReal

checkOpTy f e t t'
  | Just s <- unify f (Just e) t t'
  = checkNumeric f (apply s t) >> return (apply s t)

checkOpTy _ e t t'
  = throwErrorAt (errOp e t t')

checkFractional :: Env -> Sort -> CheckM ()
checkFractional f s@(FObj l)
  = do t <- checkSym f l
       unless (t == FFrac) $ throwErrorAt (errNonFractional s)
checkFractional _ s
  = unless (isReal s) $ throwErrorAt (errNonFractional s)

checkNumeric :: Env -> Sort -> CheckM ()
checkNumeric f s@(FObj l)
  = do t <- checkSym f l
       unless (t `elem` [FNum, FFrac, intSort, FInt]) (throwErrorAt $ errNonNumeric s)
checkNumeric _ s
  = unless (isNumeric s) (throwErrorAt $ errNonNumeric s)

checkEqConstr :: Env -> Maybe Expr -> TVSubst -> Symbol -> Sort -> CheckM TVSubst
checkEqConstr _ _  θ a (FObj b)
  | a == b
  = return θ
checkEqConstr f e θ a t =
  case f a of
    Found tA -> unify1 f e θ tA t
    _        -> throwErrorAt $ errUnifyMsg (Just "ceq2") e (FObj a) t

--------------------------------------------------------------------------------
-- | Checking Predicates -------------------------------------------------------
--------------------------------------------------------------------------------
checkPred :: Env -> Expr -> CheckM ()
checkPred f e = checkExpr f e >>= checkBoolSort e

checkBoolSort :: Expr -> Sort -> CheckM ()
checkBoolSort e s
  | s == boolSort = return ()
  | otherwise     = throwErrorAt (errBoolSort e s)

-- | Checking Relations
checkRel :: HasCallStack => Env -> Brel -> Expr -> Expr -> CheckM ()
checkRel f Eq e1 e2 = do
  t1 <- checkExpr f e1
  t2 <- checkExpr f e2
  su <- unifys f (Just e) [t1] [t2] `withError` errRel e t1 t2
  _  <- checkExprAs f (apply su t1) e1
  _  <- checkExprAs f (apply su t2) e2
  checkRelTy f e Eq t1 t2
  where
    e = PAtom Eq e1 e2

checkRel f r  e1 e2 = do
  t1 <- checkExpr f e1
  t2 <- checkExpr f e2
  checkRelTy f (PAtom r e1 e2) r t1 t2


checkRelTy :: Env -> Expr -> Brel -> Sort -> Sort -> CheckM ()
checkRelTy _ e Ueq s1 s2     = checkURel e s1 s2
checkRelTy _ e Une s1 s2     = checkURel e s1 s2
checkRelTy f _ _ s1@(FObj l) s2@(FObj l') | l /= l'
                             = (checkNumeric f s1 >> checkNumeric f s2) `withError` errNonNumerics l l'
checkRelTy _ _ _ FReal FReal = return ()
checkRelTy _ _ _ FInt  FReal = return ()
checkRelTy _ _ _ FReal FInt  = return ()
checkRelTy f _ _ FInt  s2    = checkNumeric    f s2 `withError` errNonNumeric s2
checkRelTy f _ _ s1    FInt  = checkNumeric    f s1 `withError` errNonNumeric s1
checkRelTy f _ _ FReal s2    = checkFractional f s2 `withError` errNonFractional s2
checkRelTy f _ _ s1    FReal = checkFractional f s1 `withError` errNonFractional s1
checkRelTy f e Eq t1 t2      = void (unifys f (Just e) [t1] [t2] `withError` errRel e t1 t2)
checkRelTy f e Ne t1 t2      = void (unifys f (Just e) [t1] [t2] `withError` errRel e t1 t2)
checkRelTy _ e _  t1 t2      = unless (t1 == t2) (throwErrorAt $ errRel e t1 t2)

checkURel :: Expr -> Sort -> Sort -> CheckM ()
checkURel e s1 s2 = unless (b1 == b2) (throwErrorAt $ errRel e s1 s2)
  where
    b1            = s1 == boolSort
    b2            = s2 == boolSort


--------------------------------------------------------------------------------
-- | Sort Unification
--------------------------------------------------------------------------------
{-# SCC unify #-}
unify :: Env -> Maybe Expr -> Sort -> Sort -> Maybe TVSubst
--------------------------------------------------------------------------------
unify f e t1 t2
  = case runCM0 dummySpan Nothing (unify1 f e emptySubst t1 t2) of
      Left _   -> Nothing
      Right su -> Just su

--------------------------------------------------------------------------------
unifyTo1 :: Env -> [Sort] -> Maybe Sort
--------------------------------------------------------------------------------
unifyTo1 f ts
  = case runCM0 dummySpan Nothing (unifyTo1M f ts) of
      Left _  -> Nothing
      Right t -> Just t


--------------------------------------------------------------------------------
unifyTo1M :: Env -> [Sort] -> CheckM Sort
--------------------------------------------------------------------------------
unifyTo1M _ []     = panic "unifyTo1: empty list"
unifyTo1M f (t0:ts) = snd <$> foldM step (emptySubst, t0) ts
  where
    step :: (TVSubst, Sort) -> Sort -> CheckM (TVSubst, Sort)
    step (su, t) t' = do
      su' <- unify1 f Nothing su t t'
      return (su', apply su' t)


--------------------------------------------------------------------------------
unifySorts :: Sort -> Sort -> Maybe TVSubst
--------------------------------------------------------------------------------
unifySorts = unifyFast False emptyEnv
  where
    emptyEnv x = die $ err dummySpan $ "SortCheck: lookup in Empty Env: " <> pprint x


--------------------------------------------------------------------------------
-- | Fast Unification; `unifyFast True` is just equality
--------------------------------------------------------------------------------
unifyFast :: Bool -> Env -> Sort -> Sort -> Maybe TVSubst
--------------------------------------------------------------------------------
unifyFast False f t1 t2 = unify f Nothing t1 t2
unifyFast True  _ t1 t2
  | t1 == t2  = Just emptySubst
  | otherwise = Nothing

{-
eqFast :: Sort -> Sort -> Bool
eqFast = go
  where
    go FAbs {} _       = False
    go (FFunc s1 s2) t = case t of
                          FFunc t1 t2 -> go s1 t1 && go s2 t2
                          _ -> False
    go (FApp s1 s2)  t = case t of
                          FApp t1 t2 ->  go s1 t1 && go s2 t2
                          _ -> False

    go (FTC s1) t      = case t of
                            FTC t1 -> s1 == t1
                            _ -> False

    go FInt FInt           = True
    go FReal FReal         = True
    go FNum FNum           = True
    go FFrac FFrac         = True
    go (FVar i1) (FVar i2) = i1 == i2
    go _ _                 = False

 -}
--------------------------------------------------------------------------------
unifys :: HasCallStack => Env -> Maybe Expr -> [Sort] -> [Sort] -> CheckM TVSubst
--------------------------------------------------------------------------------
unifys f e = unifyMany f e emptySubst

unifyMany :: HasCallStack => Env -> Maybe Expr -> TVSubst -> [Sort] -> [Sort] -> CheckM TVSubst
unifyMany f e θ ts ts'
  | length ts == length ts' = foldM (uncurry . unify1 f e) θ $ zip ts ts'
  | otherwise               = throwErrorAt (errUnifyMany ts ts')

unify1 :: Env -> Maybe Expr -> TVSubst -> Sort -> Sort -> CheckM TVSubst
unify1 f e !θ (FVar !i) !t
  = unifyVar f e θ i t
unify1 f e !θ !t (FVar !i)
  = unifyVar f e θ i t
unify1 f e !θ (FApp !t1 !t2) (FApp !t1' !t2')
  = unifyMany f e θ [t1, t2] [t1', t2']
unify1 _ _ !θ (FTC !l1) (FTC !l2)
  | isListTC l1 && isListTC l2
  = return θ
unify1 f e !θ t1@(FAbs _ _) !t2 = do
  !t1' <- instantiate t1
  unifyMany f e θ [t1'] [t2]
unify1 f e !θ !t1 t2@(FAbs _ _) = do
  !t2' <- instantiate t2
  unifyMany f e θ [t1] [t2']
unify1 _ _ !θ !s1 !s2
  | isString s1, isString s2
  = return θ
unify1 _ _ !θ FInt  FReal = return θ

unify1 _ _ !θ FReal FInt  = return θ

unify1 f e !θ !t FInt = do
  checkNumeric f t `withError` errUnify e t FInt
  return θ

unify1 f e !θ FInt !t = do
  checkNumeric f t `withError` errUnify e FInt t
  return θ

unify1 f e !θ (FFunc !t1 !t2) (FFunc !t1' !t2') =
  unifyMany f e θ [t1, t2] [t1', t2']

unify1 f e θ (FObj a) !t =
  checkEqConstr f e θ a t

unify1 f e θ !t (FObj a) =
  checkEqConstr f e θ a t

unify1 _ e θ !t1 !t2
  | t1 == t2
  = return θ
  | otherwise
  = throwErrorAt (errUnify e t1 t2)

subst :: Int -> Sort -> Sort -> Sort
subst !j !tj t@(FVar !i)
  | i == j                  = tj
  | otherwise               = t

subst !j !tj (FApp !t1 !t2)  = FApp t1' t2'
  where
    !t1'                    = subst j tj t1
    !t2'                    = subst j tj t2

-- subst _ _  !(FTC l)         = FTC l
subst !j !tj (FFunc !t1 !t2) = FFunc t1' t2'
  where
    !t1'                    = subst j tj $! t1
    !t2'                    = subst j tj $! t2

subst !j !tj (FAbs !i !t)
  | i == j                  = FAbs i t
  | otherwise               = FAbs i t'
  where
    !t'                     = subst j tj t

subst _  _   !s             = s

--------------------------------------------------------------------------------
instantiate :: Sort -> CheckM Sort
--------------------------------------------------------------------------------
instantiate !t = go t
  where
    go (FAbs !i !t') = do
      !t''   <- instantiate t'
      !v     <- fresh
      return $ subst i (FVar v) t''
    go !t' =
      return t'

unifyVar :: Env -> Maybe Expr -> TVSubst -> Int -> Sort -> CheckM TVSubst
unifyVar _ _ θ !i t@(FVar !j)
  = case lookupVar i θ of
      Just !t'      -> if t == t' then return θ else return (updateVar j t' θ)
      Nothing       -> return (updateVar i t θ)

unifyVar f e θ !i !t
  = case lookupVar i θ of
      Just (FVar !j) -> return $ updateVar i t $ updateVar j t θ
      Just !t'       -> if t == t' then return θ else unify1 f e θ t t'
      Nothing        -> return (updateVar i t θ)


--------------------------------------------------------------------------------
-- | Update global subst to be applied to expressions
--------------------------------------------------------------------------------

updateTVSubst :: TVSubst -> CheckM ()
updateTVSubst theta = do
  refTheta <- asks chTVSubst
  liftIO $ atomicModifyIORef' refTheta $ const (Just theta, ())

-- local (\s -> s {chTVSubst = theta}) (return ())

mergeTVSubst :: TVSubst -> Maybe TVSubst -> TVSubst
mergeTVSubst (Th m1) Nothing = Th m1
mergeTVSubst (Th m1) (Just (Th m2)) = Th m1 <> Th m2

composeTVSubst :: Maybe TVSubst -> CheckM ()
composeTVSubst Nothing = return ()
composeTVSubst (Just theta1) = do
  refTheta <- asks chTVSubst
  theta <- liftIO $ readIORef refTheta
  updateTVSubst (mergeTVSubst theta1 theta)

--------------------------------------------------------------------------------
-- | Applying a Type Substitution ----------------------------------------------
--------------------------------------------------------------------------------
apply :: TVSubst -> Sort -> Sort
--------------------------------------------------------------------------------
apply !θ          = Vis.mapSort f
  where
    f t@(FVar !i) = fromMaybe t (lookupVar i θ)
    f !t          = t

applyExpr :: Maybe TVSubst -> Expr -> Expr
applyExpr Nothing e  = e
applyExpr (Just θ) e = Vis.mapExprOnExpr f e
  where
    f (ECst !e' !s) = ECst e' (apply θ s)
    f !e'          = e'

--------------------------------------------------------------------------------
_applyCoercion :: Symbol -> Sort -> Sort -> Sort
--------------------------------------------------------------------------------
_applyCoercion a t = Vis.mapSort f
  where
    f (FObj b)
      | a == b    = t
    f s           = s


--------------------------------------------------------------------------------
-- | Deconstruct a function-sort -----------------------------------------------
--------------------------------------------------------------------------------
checkFunSort :: Sort -> CheckM (Sort, Sort, TVSubst)
checkFunSort (FAbs _ t)    = checkFunSort t
checkFunSort (FFunc t1 t2) = return (t1, t2, emptySubst)
checkFunSort (FVar i)      = do j <- fresh
                                k <- fresh
                                return (FVar j, FVar k, updateVar i (FFunc (FVar j) (FVar k)) emptySubst)
checkFunSort t             = throwErrorAt (errNonFunction 1 t)

--------------------------------------------------------------------------------
-- | API for manipulating Sort Substitutions -----------------------------------
--------------------------------------------------------------------------------

newtype TVSubst = Th (M.IntMap Sort) deriving (Show)

instance Semigroup TVSubst where
  (Th s1) <> (Th s2) = Th (s1 <> s2)

instance Monoid TVSubst where
  mempty  = Th mempty
  mappend = (<>)

lookupVar :: Int -> TVSubst -> Maybe Sort
lookupVar i (Th m)   = M.lookup i m
{-# SCC lookupVar #-}

updateVar :: Int -> Sort -> TVSubst -> TVSubst
updateVar !i !t (Th m) = Th (M.insert i t m)

emptySubst :: TVSubst
emptySubst = Th M.empty

--------------------------------------------------------------------------------
-- | Error messages ------------------------------------------------------------
--------------------------------------------------------------------------------

errElabExpr   :: Expr -> String
errElabExpr e = printf "Elaborate fails on %s" (showpp e)

errUnifyMsg :: Maybe String -> Maybe Expr -> Sort -> Sort -> String
errUnifyMsg msgMb eo t1 t2
  = printf "Cannot unify %s with %s %s %s"
      (showpp t1) {- (show t1) -} (showpp t2) {-(show t2)-} (errUnifyExpr eo) msgStr
    where
      msgStr = case msgMb of { Nothing -> ""; Just s -> "<< " ++ s ++ " >>"}

errUnify :: Maybe Expr -> Sort -> Sort -> String
errUnify = errUnifyMsg Nothing

errUnifyExpr :: Maybe Expr -> String
errUnifyExpr Nothing  = ""
errUnifyExpr (Just e) = "in expression: " ++ showpp e

errUnifyMany :: [Sort] -> [Sort] -> String
errUnifyMany ts ts'  = printf "Cannot unify types with different cardinalities %s and %s"
                         (showpp ts) (showpp ts')

errRel :: HasCallStack => Expr -> Sort -> Sort -> String
errRel e t1 t2       =
  traced $ printf "Invalid Relation %s with operand types %s and %s"
                         (showpp e) (showpp t1) (showpp t2)

errOp :: Expr -> Sort -> Sort -> String
errOp e t t'
  | t == t'          = printf "Operands have non-numeric types %s in %s"
                         (showpp t) (showpp e)
  | otherwise        = printf "Operands have different types %s and %s in %s"
                         (showpp t) (showpp t') (showpp e)

errIte :: Expr -> Expr -> Sort -> Sort -> String
errIte e1 e2 t1 t2   = printf "Mismatched branches in Ite: then %s : %s, else %s : %s"
                         (showpp e1) (showpp t1) (showpp e2) (showpp t2)

errCast :: Expr -> Sort -> Sort -> String
errCast e t' t       = printf "Cannot cast %s of sort %s to incompatible sort %s"
                         (showpp e) (showpp t') (showpp t)

errUnboundAlts :: Symbol -> [Symbol] -> String
errUnboundAlts x xs  = printf "Unbound symbol %s --- perhaps you meant: %s ?"
                         (showpp x) (L.intercalate ", " (showpp <$> xs))

errNonFunction :: Int -> Sort -> String
errNonFunction i t   = printf "The sort %s is not a function with at least %s arguments\n" (showpp t) (showpp i)

errNonNumeric :: Sort -> String
errNonNumeric  l     = printf "The sort %s is not numeric" (showpp l)

errNonNumerics :: Symbol -> Symbol -> String
errNonNumerics l l'  = printf "FObj sort %s and %s are different and not numeric" (showpp l) (showpp l')

errNonFractional :: Sort -> String
errNonFractional  l  = printf "The sort %s is not fractional" (showpp l)

errBoolSort :: Expr -> Sort -> String
errBoolSort     e s  = printf "Expressions %s should have bool sort, but has %s" (showpp e) (showpp s)
