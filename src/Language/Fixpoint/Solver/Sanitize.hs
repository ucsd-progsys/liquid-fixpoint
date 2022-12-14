-- | Validate and Transform Constraints to Ensure various Invariants -------------------------
--   1. Each binder must be associated with a UNIQUE sort
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Language.Fixpoint.Solver.Sanitize
  ( -- * Transform FInfo to enforce invariants
    sanitize

    -- * Sorts for each Symbol (move elsewhere)
  , symbolEnv

    -- * Remove substitutions K[x := e] where `x` is not in dom(K)
  , dropDeadSubsts
  ) where

import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.Visitor
import           Language.Fixpoint.SortCheck     (elaborate, applySorts, isFirstOrder)
-- import           Language.Fixpoint.Defunctionalize
import qualified Language.Fixpoint.Misc                            as Misc
import qualified Language.Fixpoint.Types                           as F
import           Language.Fixpoint.Types.Config (Config)
import qualified Language.Fixpoint.Types.Config as Cfg
import qualified Language.Fixpoint.Types.Errors                    as E
import qualified Language.Fixpoint.Smt.Theories                    as Thy
import           Language.Fixpoint.Graph (kvEdges, CVertex (..))
import qualified Data.HashMap.Strict                               as M
import qualified Data.HashSet                                      as S
import qualified Data.List                                         as L
import qualified Data.Text                                         as T
import           Data.Maybe          (isNothing, mapMaybe, fromMaybe)
import           Control.Monad       ((>=>))
import           Text.PrettyPrint.HughesPJ

type SanitizeM a = Either E.Error a

--------------------------------------------------------------------------------
sanitize :: Config -> F.SInfo a -> SanitizeM (F.SInfo a)
--------------------------------------------------------------------------------
sanitize cfg =    -- banIllScopedKvars
        --      Misc.fM dropAdtMeasures
        --      >=>
                     banIrregularData
         >=> Misc.fM dropFuncSortedShadowedBinders
         >=> Misc.fM sanitizeWfC
         >=> Misc.fM replaceDeadKvars
         >=> Misc.fM (dropDeadSubsts . restrictKVarDomain)
         >=>         banMixedRhs
         >=>         banQualifFreeVars
         >=>         banConstraintFreeVars
         >=> Misc.fM addLiterals
         >=> Misc.fM (eliminateEta cfg)
         >=> Misc.fM cancelCoercion


--------------------------------------------------------------------------------
-- | 'dropAdtMeasures' removes all the measure definitions that correspond to
--   constructor, selector or test names for declared datatypes, as these are
--   now "natively" handled by the SMT solver.
--------------------------------------------------------------------------------
_dropAdtMeasures :: F.SInfo a -> F.SInfo a
_dropAdtMeasures si = si { F.ae = dropAdtAenv (F.ddecls si) (F.ae si) }

dropAdtAenv :: [F.DataDecl] -> F.AxiomEnv -> F.AxiomEnv
dropAdtAenv ds ae = ae { F.aenvSimpl = filter (not . isAdt) (F.aenvSimpl ae) }
  where
    isAdt         = (`S.member` adtSyms) . F.smName
    adtSyms       = adtSymbols ds

adtSymbols :: [F.DataDecl] -> S.HashSet F.Symbol
adtSymbols = S.fromList . map fst . concatMap Thy.dataDeclSymbols

--------------------------------------------------------------------------------
-- | `addLiterals` traverses the constraints to find (string) literals that
--   are then added to the `dLits` field.
--------------------------------------------------------------------------------
addLiterals :: F.SInfo a -> F.SInfo a
--------------------------------------------------------------------------------
addLiterals si = si { F.dLits = F.unionSEnv (F.dLits si) lits'
                    , F.gLits = F.unionSEnv (F.gLits si) lits'
                    }
  where
    lits'      = M.fromList [ (F.symbol x, F.strSort) | x <- symConsts si ]



cancelCoercion :: F.SInfo a -> F.SInfo a
cancelCoercion = mapExpr (trans (defaultVisitor { txExpr = go }) () ())
  where
    go _ (F.ECoerc t1 t2 (F.ECoerc t2' t1' e))
      | t1 == t1' && t2 == t2'
      = e
    go _ e = e

--------------------------------------------------------------------------------
-- | `eliminateEta` converts equations of the form f x = g x into f = g
--------------------------------------------------------------------------------
eliminateEta :: Config -> F.SInfo a -> F.SInfo a
--------------------------------------------------------------------------------
eliminateEta cfg si
  | Cfg.etaElim cfg
  , Cfg.oldPLE  cfg
  = si { F.ae = ae' }
  | Cfg.etaElim cfg
  = si { F.ae = (ae {F.aenvEqs = etaElimNEW `fmap` F.aenvEqs ae }) }
  | otherwise
  = si
  where
    ae' = ae {F.aenvEqs = eqs}
    ae = F.ae si
    eqs = fmap etaElim (F.aenvEqs ae)

    etaElim eq = F.notracepp "Eliminating" $
                 case body of
                   F.PAtom F.Eq e0 e1 ->
                     let (f0, args0) = fapp e0
                         (f1, args1) = F.notracepp "f1" $ fapp e1 in
                     if reverse args0 == args
                     then let commonArgs = F.notracepp "commonArgs" .
                                           fmap fst .
                                           takeWhile (uncurry (==)) $
                                           zip args0 args1
                              commonLength = length commonArgs
                              (newArgsAndSorts, elimedArgsAndSorts) =
                                splitAt (length args - commonLength) argsAndSorts
                              args0' = F.eVar <$> reverse (drop commonLength args0)
                              args1' = F.eVar <$> reverse (drop commonLength args1) in
                       eq { F.eqArgs = newArgsAndSorts
                          , F.eqSort = foldr F.FFunc sort
                                       (snd <$> elimedArgsAndSorts)
                          , F.eqBody = F.PAtom F.Eq (F.eApps f0 args0') (F.eApps f1 args1')}
                     else eq
                   _ -> eq
      where argsAndSorts = F.eqArgs eq
            args = fst <$> argsAndSorts
            body = F.eqBody eq
            sort = F.eqSort eq
    etaElimNEW eq = F.notracepp "Eliminating" $
                  let (f1, args1) = fapp (F.eqBody eq) in
                  let commonArgs = F.notracepp "commonArgs" .
                                           fmap fst .
                                           takeWhile (uncurry (==)) $
                                           zip args0 args1
                      commonLength = length commonArgs
                      (newArgsAndSorts, elimedArgsAndSorts) =
                                splitAt (length args - commonLength) argsAndSorts
                      args1' = F.eVar <$> reverse (drop commonLength args1) in
                  eq { F.eqArgs = newArgsAndSorts
                     , F.eqSort = foldr F.FFunc sort
                                       (snd <$> elimedArgsAndSorts)
                     , F.eqBody = F.eApps f1 args1'}
      where argsAndSorts = F.eqArgs eq
            args  = fst <$> argsAndSorts
            args0 = reverse args
            sort  = F.eqSort eq

    fapp :: F.Expr -> (F.Expr, [F.Symbol])
    fapp ee = fromMaybe (ee, []) (fapp' ee)

    fapp' :: F.Expr -> Maybe (F.Expr, [F.Symbol])
    fapp' (F.EApp e0 (F.EVar arg)) = do
      (fvar, args) <- fapp' e0
      splitApp (fvar, arg:args)
    fapp' e = pure (e, [])

    theorySymbols = F.notracepp "theorySymbols" $ Thy.theorySymbols $ F.ddecls si

    splitApp (e, es)
      | isNothing $ F.notracepp ("isSmt2App? " ++ showpp e) $ Thy.isSmt2App theorySymbols $ stripCasts e
      = pure (e,es)
      | otherwise
      = Nothing

--------------------------------------------------------------------------------
-- | See issue liquid-fixpoint issue #230. This checks that whenever we have,
--      G1        |- K.su1
--      G2, K.su2 |- rhs
--   then
--      G1 \cap G2 \subseteq wenv(k)
--------------------------------------------------------------------------------
_banIllScopedKvars :: F.SInfo a ->  SanitizeM (F.SInfo a)
--------------------------------------------------------------------------------
_banIllScopedKvars si = Misc.applyNonNull (Right si) (Left . badKs) errs
  where
    errs              = concatMap (checkIllScope si kDs) ks
    kDs               = kvarDefUses si
    ks                = filter notKut $ M.keys (F.ws si)
    notKut            = not . (`F.ksMember` F.kuts si)

badKs :: [(F.KVar, F.SubcId, F.SubcId, F.IBindEnv)] -> F.Error
badKs = E.catErrors . map E.errIllScopedKVar

type KvConstrM = M.HashMap F.KVar [Integer]
type KvDefs    = (KvConstrM, KvConstrM)

checkIllScope :: F.SInfo a -> KvDefs -> F.KVar -> [(F.KVar, F.SubcId, F.SubcId, F.IBindEnv)]
checkIllScope si (inM, outM) k = mapMaybe (uncurry (isIllScope si k)) ios
  where
    ios                        = [(i, o) | i <- ins, o <- outs, i /= o ]
    ins                        = M.lookupDefault [] k inM
    outs                       = M.lookupDefault [] k outM

isIllScope :: F.SInfo a -> F.KVar -> F.SubcId -> F.SubcId -> Maybe (F.KVar, F.SubcId, F.SubcId, F.IBindEnv)
isIllScope si k inI outI
  | F.nullIBindEnv badXs = Nothing
  | otherwise            = Just (k, inI, outI, badXs)
  where
    badXs                = F.diffIBindEnv commonXs kXs
    kXs                  = {- F.tracepp ("kvarBinds " ++ show k) $ -} kvarBinds si k
    commonXs             = F.intersectionIBindEnv inXs outXs
    inXs                 = subcBinds si inI
    outXs                = subcBinds si outI

subcBinds :: F.SInfo a -> F.SubcId -> F.IBindEnv
subcBinds si i = F._cenv $ F.cm si M.! i

kvarBinds :: F.SInfo a -> F.KVar -> F.IBindEnv
kvarBinds si = F.wenv . (F.ws si M.!)

kvarDefUses :: F.SInfo a -> KvDefs
kvarDefUses si = (Misc.group ins, Misc.group outs)
  where
    es         = kvEdges si
    outs       = [(k, o) | (KVar k, Cstr o) <- es ]
    ins        = [(k, i) | (Cstr i, KVar k) <- es ]

--------------------------------------------------------------------------------
-- | `dropDeadSubsts` removes dead `K[x := e]` where `x` NOT in the domain of K.
--------------------------------------------------------------------------------
dropDeadSubsts :: F.SInfo a -> F.SInfo a
dropDeadSubsts si = mapKVarSubsts (F.filterSubst . f) si
  where
    kvsM          = M.mapWithKey (\k _ -> kvDom k) (F.ws si)
    kvDom         = S.fromList . F.kvarDomain si
    f k x _       = S.member x (M.lookupDefault mempty k kvsM)

--------------------------------------------------------------------------------
-- | `restrictKVarDomain` updates the kvar-domains in the wf constraints
--   to a subset of the original binders, where we DELETE the parameters
--   `x` which appear in substitutions of the form `K[x := y]` where `y`
--   is not in the env.
--------------------------------------------------------------------------------
restrictKVarDomain :: F.SInfo a -> F.SInfo a
restrictKVarDomain si = si { F.ws = M.mapWithKey (restrictWf kvm) (F.ws si) }
  where
    kvm               = safeKvarEnv si

-- | `restrictWf kve k w` restricts the env of `w` to the parameters in `kve k`.
restrictWf :: KvDom -> F.KVar -> F.WfC a -> F.WfC a
restrictWf kve k w = w { F.wenv = F.filterIBindEnv f (F.wenv w) }
  where
    f i            = S.member i kis
    kis            = S.fromList [ i | (_, i) <- F.toListSEnv kEnv ]
    kEnv           = M.lookupDefault mempty k kve

-- | `safeKvarEnv` computes the "real" domain of each kvar, which is
--   a SUBSET of the input domain, in which we KILL the parameters
--   `x` which appear in substitutions of the form `K[x := y]`
--   where `y` is not in the env.

type KvDom     = M.HashMap F.KVar (F.SEnv F.BindId)
type KvBads    = M.HashMap F.KVar [F.Symbol]

safeKvarEnv :: F.SInfo a -> KvDom
safeKvarEnv si = L.foldl' (dropKvarEnv si) env0 cs
  where
    cs         = M.elems  (F.cm si)
    env0       = initKvarEnv si

dropKvarEnv :: F.SInfo a -> KvDom -> F.SimpC a -> KvDom
dropKvarEnv si kve c = M.mapWithKey (dropBadParams kBads) kve
  where
    kBads            = badParams si c

dropBadParams :: KvBads -> F.KVar -> F.SEnv F.BindId -> F.SEnv F.BindId
dropBadParams kBads k kEnv = L.foldl' (flip F.deleteSEnv) kEnv xs
  where
    xs                     = M.lookupDefault mempty k kBads

badParams :: F.SInfo a -> F.SimpC a -> M.HashMap F.KVar [F.Symbol]
badParams si c = Misc.group bads
  where
    bads       = [ (k, x) | (v, k, F.Su su) <- subcKSubs xsrs c
                          , let vEnv = maybe sEnv (`S.insert` sEnv) v
                          , (x, e)          <- M.toList su
                          , badArg vEnv e
                 ]
    sEnv       = S.fromList (fst <$> xsrs)
    xsrs       = F.envCs (F.bs si) (F.senv c)

badArg :: S.HashSet F.Symbol -> F.Expr -> Bool
badArg sEnv (F.EVar y) = not (y `S.member` sEnv)
badArg _    _          = True

type KSub = (Maybe F.Symbol, F.KVar, F.Subst)

subcKSubs :: [(F.Symbol, F.SortedReft)] -> F.SimpC a -> [KSub]
subcKSubs xsrs c = rhs ++ lhs
  where
    lhs          = [ (Just v, k, su) | (_, sr) <- xsrs
                                     , let rs   = F.reftConjuncts (F.sr_reft sr)
                                     , F.Reft (v, F.PKVar k su) <- rs
                   ]
    rhs          = [(Nothing, k, su) | F.PKVar k su <- [F.crhs c]]


initKvarEnv :: F.SInfo a -> KvDom
initKvarEnv si = initEnv si <$> F.ws si

initEnv :: F.SInfo a -> F.WfC a -> F.SEnv F.BindId
initEnv si w = F.fromListSEnv [ (bind i, i) | i <- is ]
  where
    is       = F.elemsIBindEnv $ F.wenv w
    bind i   = Misc.fst3 (F.lookupBindEnv i be)
    be       = F.bs si

--------------------------------------------------------------------------------
-- | check that no constraint has free variables (ignores kvars)
--------------------------------------------------------------------------------
banConstraintFreeVars :: F.SInfo a -> SanitizeM (F.SInfo a)
banConstraintFreeVars fi0 = Misc.applyNonNull (Right fi0) (Left . badCs) bads
  where
    fi      = mapKVars (const $ Just F.PTrue) fi0
    bads    = [(c, fs) | c <- M.elems $ F.cm fi, Just fs <- [cNoFreeVars fi k c]]
    k       = known fi

known :: F.SInfo a -> F.Symbol -> Bool
known fi  = \x -> F.memberSEnv x lits || F.memberSEnv x prims
  where
    lits  = F.gLits fi
    prims = Thy.theorySymbols . F.ddecls $ fi

cNoFreeVars :: F.SInfo a -> (F.Symbol -> Bool) -> F.SimpC a -> Maybe [F.Symbol]
cNoFreeVars fi known c = if S.null fv then Nothing else Just (S.toList fv)
  where
    be   = F.bs fi
    ids  = F.elemsIBindEnv $ F.senv c
    cDom = [Misc.fst3 $ F.lookupBindEnv i be | i <- ids]
    cRng = concat [S.toList . F.reftFreeVars . F.sr_reft . Misc.snd3 $ F.lookupBindEnv i be | i <- ids]
        ++ F.syms (F.crhs c)
    fv   = (`Misc.nubDiff` cDom) . filter (not . known) $ cRng

badCs :: Misc.ListNE (F.SimpC a, [F.Symbol]) -> E.Error
badCs = E.catErrors . map (E.errFreeVarInConstraint . Misc.mapFst F.subcId)

--------------------------------------------------------------------------------
-- | check that every DataDecl is regular
--------------------------------------------------------------------------------
banIrregularData :: F.SInfo a -> SanitizeM (F.SInfo a)
banIrregularData fi = Misc.applyNonNull (Right fi) (Left . badDataDecl) bads
  where
    bads = F.checkRegular (F.ddecls fi )

badDataDecl :: Misc.ListNE F.DataDecl -> E.Error
badDataDecl ds = E.catErrors [ E.errBadDataDecl d | d <- ds ]

--------------------------------------------------------------------------------
-- | check that no qualifier has free variables
--------------------------------------------------------------------------------
banQualifFreeVars :: F.SInfo a -> SanitizeM (F.SInfo a)
--------------------------------------------------------------------------------
banQualifFreeVars fi = Misc.applyNonNull (Right fi) (Left . badQuals) bads
  where
    bads    = [ (q, xs) | q <- F.quals fi, let xs = free q, not (null xs) ]
    free q  = filter (not . isLit) (F.syms q)
    isLit x = F.memberSEnv x (F.gLits fi)
    -- lits    = fst <$> F.toListSEnv (F.gLits fi)
    -- free q  = S.toList $ F.syms (F.qBody q) `nubDiff` (lits ++ F.prims ++ F.syms (F.qpSym <$> F.qParams q))

badQuals     :: Misc.ListNE (F.Qualifier, Misc.ListNE F.Symbol) -> E.Error
badQuals bqs = E.catErrors [ E.errFreeVarInQual q xs | (q, xs) <- bqs]


--------------------------------------------------------------------------------
-- | check that each constraint has RHS of form [k1,...,kn] or [p]
--------------------------------------------------------------------------------
banMixedRhs :: F.SInfo a -> SanitizeM (F.SInfo a)
--------------------------------------------------------------------------------
banMixedRhs fi = Misc.applyNonNull (Right fi) (Left . badRhs) bads
  where
    ics        = M.toList $ F.cm fi
    bads       = [(i, c) | (i, c) <- ics, not $ isOk c]
    isOk c     = isKvarC c || isConcC c

badRhs :: Misc.ListNE (Integer, F.SimpC a) -> E.Error
badRhs = E.catErrors . map badRhs1

badRhs1 :: (Integer, F.SimpC a) -> E.Error
badRhs1 (i, c) = E.err E.dummySpan $ vcat [ "Malformed RHS for constraint id" <+> pprint i
                                          , nest 4 (pprint (F.crhs c)) ]

--------------------------------------------------------------------------------
-- | symbol |-> sort for EVERY variable in the SInfo; 'symbolEnv' can ONLY be
--   called with **sanitized** environments (post the uniqification etc.) or
--   else you get duplicate sorts and other such errors.
--   We do this peculiar dance with `env0` to extract the apply-sorts from the
--   function definitions inside the `AxiomEnv` which cannot be elaborated as
--   it makes it hard to actually find the fundefs within (breaking PLE.)
--------------------------------------------------------------------------------
symbolEnv :: Config -> F.SInfo a -> F.SymEnv
symbolEnv cfg si = F.symEnv sEnv tEnv ds lits (ts ++ ts')
  where
    ts'          = applySorts ae'
    ae'          = elaborate (F.atLoc E.dummySpan "symbolEnv") env0 (F.ae si)
    env0         = F.symEnv sEnv tEnv ds lits ts
    tEnv         = Thy.theorySymbols ds
    ds           = F.ddecls si
    ts           = Misc.hashNub (applySorts si ++ [t | (_, t) <- F.toListSEnv sEnv])
    sEnv         = (F.tsSort <$> tEnv) `mappend` F.fromListSEnv xts
    xts          = symbolSorts cfg si ++ alits
    lits         = F.dLits si `F.unionSEnv'` F.fromListSEnv alits
    alits        = litsAEnv $ F.ae si

litsAEnv :: F.AxiomEnv -> [(F.Symbol, F.Sort)]
litsAEnv ae = zip (F.symbol <$> symConsts ae) (repeat F.strSort)

symbolSorts :: Config -> F.GInfo c a -> [(F.Symbol, F.Sort)]
symbolSorts cfg fi = either E.die id $ symbolSorts' cfg fi

symbolSorts' :: Config -> F.GInfo c a -> SanitizeM [(F.Symbol, F.Sort)]
symbolSorts' _cfg fi  = (normalize . compact . (defs ++)) =<< bindSorts fi
  where
    normalize       = fmap (map (unShadow txFun dm))
    dm              = M.fromList defs
    defs            = F.toListSEnv . F.gLits $ fi
    txFun           = id

unShadow :: (F.Sort -> F.Sort) -> M.HashMap F.Symbol a -> (F.Symbol, F.Sort) -> (F.Symbol, F.Sort)
unShadow tx dm (x, t)
  | M.member x dm = (x, t)
  | otherwise     = (x, tx t)

_defuncSort :: F.Sort -> F.Sort
_defuncSort F.FFunc{} = F.funcSort
_defuncSort t         = t

compact :: [(F.Symbol, F.Sort)] -> Either E.Error [(F.Symbol, F.Sort)]
compact xts
  | null bad  = Right [(x, t) | (x, [t]) <- ok ]
  | otherwise = Left $ dupBindErrors bad'
  where
    bad'      = [(x, (, []) <$> ts) | (x, ts) <- bad]
    (bad, ok) = L.partition multiSorted . binds $ xts
    binds     = M.toList . M.map Misc.sortNub . Misc.group

--------------------------------------------------------------------------------
bindSorts  :: F.GInfo c a -> Either E.Error [(F.Symbol, F.Sort)]
--------------------------------------------------------------------------------
bindSorts fi
  | null bad   = Right [ (x, t) | (x, [(t, _)]) <- ok ]
  | otherwise  = Left $ dupBindErrors [ (x, ts) | (x, ts) <- bad]
  where
    (bad, ok)  = L.partition multiSorted . binds $ fi
    binds      = symBinds . F.bs


multiSorted :: (x, [t]) -> Bool
multiSorted = (1 <) . length . snd

dupBindErrors :: [(F.Symbol, [(F.Sort, [F.BindId] )])] -> E.Error
dupBindErrors = foldr1 E.catError . map dbe
  where
   dbe (x, y) = E.err E.dummySpan $ vcat [ "Multiple sorts for" <+> pprint x
                                         , nest 4 (pprint y) ]

--------------------------------------------------------------------------------
symBinds  :: F.BindEnv a -> [SymBinds]
--------------------------------------------------------------------------------
symBinds  = {- THIS KILLS ELEM: tracepp "symBinds" . -}
            M.toList
          . M.map Misc.groupList
          . Misc.group
          . binders

type SymBinds = (F.Symbol, [(F.Sort, [F.BindId])])

binders :: F.BindEnv a -> [(F.Symbol, (F.Sort, F.BindId))]
binders be = [(x, (F.sr_sort t, i)) | (i, (x, t, _)) <- F.bindEnvToList be]


--------------------------------------------------------------------------------
-- | Drop func-sorted `bind` that are shadowed by `constant` (if same type, else error)
--------------------------------------------------------------------------------
dropFuncSortedShadowedBinders :: F.SInfo a -> F.SInfo a
--------------------------------------------------------------------------------
dropFuncSortedShadowedBinders fi = dropBinders ok (const True) fi
  where
    ok x t  = M.member x defs ==> (F.allowHO fi || isFirstOrder t)
    defs    = M.fromList $ F.toListSEnv $ F.gLits fi

infixl 9 ==>
(==>) :: Bool -> Bool -> Bool
p ==> q = not p || q

--------------------------------------------------------------------------------
-- | Drop irrelevant binders from WfC Environments
--------------------------------------------------------------------------------
sanitizeWfC :: F.SInfo a -> F.SInfo a
sanitizeWfC si = si { F.ws = ws' }
  where
    ws'        = deleteWfCBinds drops <$> F.ws si
    (_,drops)  = filterBindEnv keepF   $  F.bs si
    keepF      = conjKF [nonConstantF si, nonFunctionF si, _nonDerivedLH]
    -- drops   = F.tracepp "sanitizeWfC: dropping" $ L.sort drops'

conjKF :: [KeepBindF] -> KeepBindF
conjKF fs x t = and [f x t | f <- fs]

-- | `nonDerivedLH` keeps a bind x if it does not start with `$` which is used
--   typically for names that are automatically "derived" by GHC (and which can)
--   blow up the environments thereby clogging instantiation, etc.
--   NOTE: This is an LH specific hack and should be moved there.

_nonDerivedLH :: KeepBindF
_nonDerivedLH x _ = not . T.isPrefixOf "$" . last . T.split ('.' ==) . F.symbolText $ x

nonConstantF :: F.SInfo a -> KeepBindF
nonConstantF si = \x _ -> not (x `F.memberSEnv` cEnv)
  where
    cEnv        = F.gLits si

nonFunctionF :: F.SInfo a -> KeepBindF
nonFunctionF si
  | F.allowHO si    = \_ _ -> True
  | otherwise       = \_ t -> isNothing (F.functionSort t)

--------------------------------------------------------------------------------
-- | Generic API for Deleting Binders from FInfo
--------------------------------------------------------------------------------
dropBinders :: KeepBindF -> KeepSortF -> F.SInfo a -> F.SInfo a
--------------------------------------------------------------------------------
dropBinders f g fi  = fi { F.bs    = bs'
                         , F.cm    = cm'
                         , F.ws    = ws'
                         , F.gLits = lits' }
  where
    -- discards        = diss
    (bs', discards) = filterBindEnv f $ F.bs fi
    cm'             = deleteSubCBinds discards   <$> F.cm fi
    ws'             = deleteWfCBinds  discards   <$> F.ws fi
    lits'           = F.filterSEnv g (F.gLits fi)

type KeepBindF = F.Symbol -> F.Sort -> Bool
type KeepSortF = F.Sort -> Bool

deleteSubCBinds :: [F.BindId] -> F.SimpC a -> F.SimpC a
deleteSubCBinds bs sc = sc { F._cenv = foldr F.deleteIBindEnv (F.senv sc) bs }

deleteWfCBinds :: [F.BindId] -> F.WfC a -> F.WfC a
deleteWfCBinds bs wf = wf { F.wenv = foldr F.deleteIBindEnv (F.wenv wf) bs }

filterBindEnv :: KeepBindF -> F.BindEnv a -> (F.BindEnv a, [F.BindId])
filterBindEnv f be  = (keepBindEnv , discard')
  where
    keepBindEnv     = F.bindEnvFromList [(i, (x, sr, a)) | (i, (x, sr, a)) <- keep]
    (keep, discard) = L.partition f' $ F.bindEnvToList be
    discard'        = fst <$> discard
    f' (_, (x, t, _)) = f x (F.sr_sort t)


---------------------------------------------------------------------------
-- | Replace KVars that do not have a WfC with PFalse
---------------------------------------------------------------------------
replaceDeadKvars :: F.SInfo a -> F.SInfo a
---------------------------------------------------------------------------
replaceDeadKvars fi = mapKVars go fi
  where
    go k | k `M.member` F.ws fi = Nothing
         | otherwise            = Just F.PFalse
