--------------------------------------------------------------------------------
-- | This module is a preliminary part of the implementation of "Proof by 
--   Logical Evaluation" where we unfold function definitions if they *must* be 
--   unfolded, to strengthen the environments with function-definition-equalities. 
--
--   In this module, we attempt to verify as many of the PLE constaints as 
--   possible without invoking the SMT solver or performing any I/O at all.
--   To this end, we use an interpreter in Haskell to attempt to evaluate down
--   expressions and generate equalities. 
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ViewPatterns              #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Fixpoint.Solver.Interpreter
  ( instInterpreter

  -- The following exports are for property testing.
  , ICtx(..)
  , Knowledge(..)
  , Simplifiable(..)
  ) where

import           Language.Fixpoint.Types hiding (simplify)
import           Language.Fixpoint.Types.Config  as FC
import           Language.Fixpoint.Types.Solutions        (CMap)
import qualified Language.Fixpoint.Types.Visitor as Vis
import qualified Language.Fixpoint.Misc          as Misc 
import           Language.Fixpoint.Defunctionalize
import qualified Language.Fixpoint.Utils.Trie    as T 
import           Language.Fixpoint.Utils.Progress 
import           Language.Fixpoint.SortCheck
import           Language.Fixpoint.Graph.Deps             (isTarget) 
import           Language.Fixpoint.Solver.Sanitize        (symbolEnv)
import           Language.Fixpoint.Solver.Simplify
import           Control.Monad.State
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import qualified Data.List            as L
import qualified Data.Maybe           as Mb
--import           Debug.Trace                              (trace)

mytracepp :: (PPrint a) => String -> a -> a
mytracepp = notracepp 

--mytrace :: String -> a -> a
--mytrace = {-trace-} flip const

--------------------------------------------------------------------------------
-- | Strengthen Constraint Environments via PLE 
--------------------------------------------------------------------------------
instInterpreter :: (Loc a) => Config -> SInfo a -> Maybe [SubcId] -> IO (SInfo a)
instInterpreter cfg fi' subcIds = do
    let cs = M.filterWithKey
               (\i c -> isPleCstr aEnv i c && maybe True (i `L.elem`) subcIds)
               (cm fi)
    let t  = mkCTrie (M.toList cs)                      -- 1. BUILD the Trie
    res   <- withProgress (1 + M.size cs) $ 
               pleTrie t $ instEnv fi cs sEnv           -- 2. TRAVERSE Trie to compute InstRes
    return $ resSInfo cfg sEnv fi res                   -- 3. STRENGTHEN SInfo using InstRes
  where
    sEnv   = symbolEnv cfg fi
    aEnv   = ae fi 
    fi     = normalize fi' 

------------------------------------------------------------------------------- 
-- | Step 1a: @instEnv@ sets up the incremental-PLE environment 
instEnv :: (Loc a) => SInfo a -> CMap (SimpC a) -> SymEnv -> InstEnv a 
instEnv fi cs sEnv = InstEnv bEnv aEnv cs γ s0
  where
    csBinds           = M.foldl' (\acc c -> unionIBindEnv acc (senv c)) emptyIBindEnv cs
    bEnv              = filterBindEnv (\i _ _ -> memberIBindEnv i csBinds) (bs fi)
    aEnv              = ae fi
    γ                 = knowledge fi  
    s0                = EvalEnv sEnv mempty 

---------------------------------------------------------------------------------------------- 
-- | Step 1b: @mkCTrie@ builds the @Trie@ of constraints indexed by their environments 
mkCTrie :: [(SubcId, SimpC a)] -> CTrie 
mkCTrie ics  = T.fromList [ (cBinds c, i) | (i, c) <- ics ]
  where
    cBinds   = L.sort . elemsIBindEnv . senv 

---------------------------------------------------------------------------------------------- 
-- | Step 2: @pleTrie@ walks over the @CTrie@ to actually do the incremental-PLE
pleTrie :: CTrie -> InstEnv a -> IO InstRes   
pleTrie t env = loopT env ctx0 diff0 Nothing res0 t 
  where 
    diff0        = []
    res0         = M.empty 
    ctx0         = initCtx env ((mkEq <$> es0) ++ (mkEq' <$> es0'))
    es0          = L.filter (null . eqArgs) (aenvEqs   . ieAenv $ env)
    es0'         = L.filter (null . smArgs) (aenvSimpl . ieAenv $ env)
    mkEq  eq     = (EVar $ eqName eq, eqBody eq)
    mkEq' rw     = (EApp (EVar $ smName rw) (EVar $ smDC rw), smBody rw)

loopT :: InstEnv a -> ICtx -> Diff -> Maybe BindId -> InstRes -> CTrie -> IO InstRes
loopT env ctx delta i res t = case t of 
  T.Node []  -> return res
  T.Node [b] -> loopB env ctx delta i res b
  T.Node bs  -> (withAssms env ctx delta Nothing $ \ctx' -> do 
                  (ctx'', res') <- ple1 env ctx' i res 
                  foldM (loopB env ctx'' [] i) res' bs)

loopB :: InstEnv a -> ICtx -> Diff -> Maybe BindId -> InstRes -> CBranch -> IO InstRes
loopB env ctx delta iMb res b = case b of 
  T.Bind i t -> loopT env ctx (i:delta) (Just i) res t
  T.Val cid  -> withAssms env ctx delta (Just cid) $ \ctx' -> do 
                  progressTick
                  (snd <$> ple1 env ctx' iMb res) 

-- Adds to @ctx@ candidate expressions to unfold from the bindings in @delta@
-- and the rhs of @cidMb@.
-- 
-- Adds to @ctx@ assumptions from @env@ and @delta@ plus rewrites that
-- candidates can use
withAssms :: InstEnv a -> ICtx -> Diff -> Maybe SubcId -> (ICtx -> IO b) -> IO b 
withAssms env@(InstEnv {..}) ctx delta cidMb act = act $
  updCtx env ctx delta cidMb 

-- | @ple1@ performs the PLE at a single "node" in the Trie 
ple1 :: InstEnv a -> ICtx -> Maybe BindId -> InstRes -> IO (ICtx, InstRes)
ple1 (InstEnv {..}) ctx i res = 
  updCtxRes res i <$> evalCandsLoop {-anfEnv-} M.empty ctx ieKnowl ieEvEnv 

evalCandsLoop :: ConstMap -> ICtx -> Knowledge -> EvalEnv -> IO ICtx 
evalCandsLoop ie ictx0 γ env = go ictx0 
  where
    withRewrites exprs =
      let
        rws = [rewrite e rw | rw <- snd <$> M.toList (knSims γ)
                            ,  e <- S.toList (snd `S.map` exprs)]
      in 
        exprs <> (S.fromList $ concat rws)
    go ictx | S.null (icCands ictx) = return ictx 
    go ictx =  do let cands = icCands ictx
                  let env' = env { evAccum = icEquals ictx <> evAccum env }
                  (ictx', evalResults)  <- 
                               foldM (evalOneCandStep ie γ env') (ictx, []) (S.toList cands) 
                  let us = mconcat evalResults 
                  if S.null (us `S.difference` icEquals ictx)
                        then return ictx 
                        else do  let oks      = fst `S.map` us
                                 let us'      = withRewrites us 
                                 let ictx''   = ictx' { icSolved = icSolved ictx <> oks 
                                                      , icEquals = icEquals ictx <> us' }
                                 let newcands = mconcat (makeCandidates γ ictx'' <$> S.toList (cands <> (snd `S.map` us)))
                                 go (ictx'' { icCands = S.fromList newcands})
                                 
-- evalOneCands :: Knowledge -> EvalEnv -> ICtx -> [Expr] -> IO (ICtx, [EvAccum])
-- evalOneCands γ env' ictx = foldM step (ictx, [])
evalOneCandStep :: ConstMap -> Knowledge -> EvalEnv -> (ICtx, [EvAccum]) -> Expr -> IO (ICtx, [EvAccum])
evalOneCandStep env γ env' (ictx, acc) e = do 
  res <- evalOne env γ env' ictx e 
  return (ictx, res : acc)

rewrite :: Expr -> Rewrite -> [(Expr,Expr)] 
rewrite e rw = Mb.catMaybes $ map (`rewriteTop` rw) (notGuardedApps e)

rewriteTop :: Expr -> Rewrite -> Maybe (Expr,Expr) 
rewriteTop e rw
  | (EVar f, es) <- splitEApp e
  , f == smDC rw
  , length es == length (smArgs rw)
  = Just (EApp (EVar $ smName rw) e, subst (mkSubst $ zip (smArgs rw) es) (smBody rw))
  | otherwise  
  = Nothing

---------------------------------------------------------------------------------------------- 
-- | Step 3: @resSInfo@ uses incremental PLE result @InstRes@ to produce the strengthened SInfo 
---------------------------------------------------------------------------------------------- 

resSInfo :: Config -> SymEnv -> SInfo a -> InstRes -> SInfo a
resSInfo cfg env fi res = strengthenBinds fi res' 
  where
    res'     = M.fromList $ zip is ps''
    ps''     = zipWith (\i -> elaborate (atLoc dummySpan ("PLE1 " ++ show i)) env) is ps' 
    ps'      = defuncAny cfg env ps
    (is, ps) = unzip (M.toList res)

---------------------------------------------------------------------------------------------- 
-- | @InstEnv@ has the global information needed to do PLE
---------------------------------------------------------------------------------------------- 

data InstEnv a = InstEnv 
  { ieBEnv  :: !BindEnv
  , ieAenv  :: !AxiomEnv 
  , ieCstrs :: !(CMap (SimpC a))
  , ieKnowl :: !Knowledge
  , ieEvEnv :: !EvalEnv
  } 

---------------------------------------------------------------------------------------------- 
-- | @ICtx@ is the local information -- at each trie node -- obtained by incremental PLE
---------------------------------------------------------------------------------------------- 

data ICtx    = ICtx 
  { icCands    :: S.HashSet Expr            -- ^ "Candidates" for unfolding
  , icEquals   :: EvAccum                   -- ^ Accumulated equalities
  , icSolved   :: S.HashSet Expr            -- ^ Terms that we have already expanded
  , icSimpl    :: !ConstMap                 -- ^ Map of expressions to constants
  , icSubcId   :: Maybe SubcId              -- ^ Current subconstraint ID
  } 

---------------------------------------------------------------------------------------------- 
-- | @InstRes@ is the final result of PLE; a map from @BindId@ to the equations "known" at that BindId
---------------------------------------------------------------------------------------------- 

type InstRes = M.HashMap BindId Expr

---------------------------------------------------------------------------------------------- 
-- | @Unfold is the result of running PLE at a single equality; 
--     (e, [(e1, e1')...]) is the source @e@ and the (possible empty) 
--   list of PLE-generated equalities (e1, e1') ... 
---------------------------------------------------------------------------------------------- 

type CTrie   = T.Trie   SubcId
type CBranch = T.Branch SubcId
type Diff    = [BindId]    -- ^ in "reverse" order

initCtx :: InstEnv a -> [(Expr,Expr)] -> ICtx
initCtx _   es   = ICtx 
  { icCands  = mempty 
  , icEquals = S.fromList es
  , icSolved = mempty
  , icSimpl  = mempty 
  , icSubcId = Nothing
  }

equalitiesPred :: S.HashSet (Expr, Expr) -> [Expr]
equalitiesPred eqs = [ EEq e1 e2 | (e1, e2) <- S.toList eqs, e1 /= e2 ] 

updCtxRes :: InstRes -> Maybe BindId -> ICtx -> (ICtx, InstRes) 
updCtxRes res iMb ctx = (ctx, res')
  where 
    res' = updRes res iMb (pAnd $ equalitiesPred $ icEquals ctx)


updRes :: InstRes -> Maybe BindId -> Expr -> InstRes
updRes res (Just i) e = M.insertWith (error "tree-like invariant broken in ple. See https://github.com/ucsd-progsys/liquid-fixpoint/issues/496") i e res 
updRes res  Nothing _ = res 


---------------------------------------------------------------------------------------------- 
-- | @updCtx env ctx delta cidMb@ adds the assumptions and candidates from @delta@ and @cidMb@ 
--   to the context. 
---------------------------------------------------------------------------------------------- 

updCtx :: InstEnv a -> ICtx -> Diff -> Maybe SubcId -> ICtx 
updCtx (InstEnv {..}) ctx delta cidMb 
    = ctx { icCands  = S.fromList cands           <> icCands  ctx
          , icEquals = initEqs                    <> icEquals ctx
          , icSimpl  = M.fromList (S.toList sims) <> icSimpl ctx <> econsts
          , icSubcId = cidMb -- fst <$> L.find (\(_, b) -> (head delta) `memberIBindEnv` (_cenv b)) ieCstrs
          }                  -- eliminate above if nothing broken 
  where         
    initEqs   = S.fromList $ concat [rewrite e rw | e  <- cands ++ (snd <$> S.toList (icEquals ctx))
                                                  , rw <- snd <$> M.toList (knSims ieKnowl)]
    cands     = concatMap (makeCandidates ieKnowl ctx) (rhs:es)
    sims      = S.filter (isSimplification (knDCs ieKnowl)) (initEqs <> icEquals ctx)
    econsts   = M.fromList $ findConstants ieKnowl es
    (rhs:es)  = unElab <$> (eRhs : (expr <$> binds))
    eRhs      = maybe PTrue crhs subMb
    binds     = [ lookupBindEnv i ieBEnv | i <- delta ] 
    subMb     = getCstr ieCstrs <$> cidMb


findConstants :: Knowledge -> [Expr] -> [(Expr, Expr)]
findConstants γ es = [(EVar x, c) | (x,c) <- go [] (concatMap splitPAnd es)]  
  where 
    go su ess = if ess == ess' 
                  then su 
                  else go (su ++ su') ess' 
       where ess' = subst (mkSubst su') <$> ess
             su'  = makeSu ess 
    makeSu exprs  = [(x,c) | (EEq (EVar x) c) <- exprs 
                           , isConstant (knDCs γ) c 
                           , EVar x /= c ]

makeCandidates :: Knowledge -> ICtx -> Expr -> [Expr]
makeCandidates γ ctx expr 
  = mytracepp ("\n" ++ show (length cands) ++ " New Candidates") cands
  where 
    cands = 
      filter (\e -> isRedex γ e && not (e `S.member` icSolved ctx)) (notGuardedApps expr) ++
      filter (\e -> hasConstructors γ e && not (e `S.member` icSolved ctx)) (largestApps expr)      

    -- Constructor occurrences need to be considered as candidadates since
    -- they identify relevant measure equations. The function 'rewrite'
    -- introduces these equations.
    hasConstructors :: Knowledge -> Expr -> Bool
    hasConstructors γ e =  not $ S.null $ S.intersection (exprSymbolsSet e) (knDCs γ)

isRedex :: Knowledge -> Expr -> Bool 
isRedex γ e = isGoodApp γ e || isIte e 
  where 
    isIte EIte {} = True 
    isIte _       = False 


isGoodApp :: Knowledge -> Expr -> Bool 
isGoodApp γ e 
  | (EVar f, es) <- splitEApp e
  , Just i       <- L.lookup f (knSummary γ)
  = length es >= i
  | otherwise
  = False 
    



getCstr :: M.HashMap SubcId (SimpC a) -> SubcId -> SimpC a 
getCstr env cid = Misc.safeLookup "Instantiate.getCstr" cid env

isPleCstr :: AxiomEnv -> SubcId -> SimpC a -> Bool
isPleCstr aenv sid c = isTarget c && M.lookupDefault False sid (aenvExpand aenv) 

type EvAccum = S.HashSet (Expr, Expr)

--------------------------------------------------------------------------------
data EvalEnv = EvalEnv
  { evEnv      :: !SymEnv
  , evAccum    :: EvAccum
  }

type EvalST a = StateT EvalEnv IO a
--------------------------------------------------------------------------------


evalOne :: ConstMap -> Knowledge -> EvalEnv -> ICtx -> Expr -> IO EvAccum
evalOne ienv γ env ctx e {- null (getAutoRws γ ctx) -} = do
    (e', st) <- runStateT (fastEval ienv γ ctx e) env  
    let evAcc' = if (mytracepp ("evalOne: " ++ showpp e) e') == e then evAccum st else S.insert (e, e') (evAccum st)
    return evAcc' 

notGuardedApps :: Expr -> [Expr]
notGuardedApps = flip go []
  where
    go e0 acc = case e0 of
      EApp e1 e2 -> e0 : go e1 (go e2 acc)
      PAnd es    -> foldr go acc es
      POr es     -> foldr go acc es
      PAtom _ e1 e2 -> go e1 $ go e2 acc
      PIff e1 e2 -> go e1 $ go e2 acc
      PImp e1 e2 -> go e1 $ go e2 acc
      EBin  _ e1 e2 -> go e1 $ go e2 acc
      PNot e -> go e acc
      ENeg e -> go e acc
      EIte b _ _ -> go b $ e0 : acc
      ECoerc _ _ e -> go e acc
      ECst e _ -> go e acc
      ESym _ -> acc
      ECon _ -> acc
      EVar _ -> acc
      ELam _ _ -> acc
      ETApp _ _ -> acc
      ETAbs _ _ -> acc
      PKVar _ _ -> acc
      PAll _ _ -> acc
      PExist _ _ -> acc
      PGrad{} -> acc

largestApps :: Expr -> [Expr]
largestApps = flip go []
  where
    go e0 acc = case e0 of
      EApp _ _ -> e0 : acc
      PAnd es -> foldr go acc es
      POr es -> foldr go acc es
      PAtom _ e1 e2 -> go e1 $ go e2 acc
      PIff e1 e2 -> go e1 $ go e2 acc
      PImp e1 e2 -> go e1 $ go e2 acc
      EBin  _ e1 e2 -> go e1 $ go e2 acc
      PNot e -> go e acc
      ENeg e -> go e acc
      EIte b _ _ -> go b $ e0 : acc
      ECoerc _ _ e -> go e acc
      ECst e _ -> go e acc
      ESym _ -> acc
      ECon _ -> acc
      EVar _ -> e0 : acc
      ELam _ _ -> acc
      ETApp _ _ -> acc
      ETAbs _ _ -> acc
      PKVar _ _ -> acc
      PAll _ _ -> acc
      PExist _ _ -> acc
      PGrad{} -> acc

fastEval :: ConstMap -> Knowledge -> ICtx -> Expr -> EvalST Expr
fastEval ienv γ ctx e 
    = do env  <- gets (seSort . evEnv)
         return $ mytracepp ("evaluating" ++ show e) $ interpret ienv γ ctx env $ simplify γ ctx e

--------------------------------------------------------------------------------
-- | 'substEq' unfolds or instantiates an equation at a particular list of
--   argument values. We must also substitute the sort-variables that appear
--   as coercions. See tests/proof/ple1.fq
--------------------------------------------------------------------------------

unfoldExpr :: ConstMap -> Knowledge -> ICtx -> SEnv Sort -> Expr -> {-EvalST-} Expr
unfoldExpr ie γ ctx env (EIte e0 e1 e2) = let g' = interpret' ie γ ctx env e0 in
                                             if g' == PTrue
                                                then unfoldExpr ie γ ctx env e1
                                                else if g' == PFalse
                                                        then unfoldExpr ie γ ctx env e2
                                                        else EIte g' e1 e2
unfoldExpr _  _ _   _   e               = e

substEq :: SEnv Sort -> Equation -> [Expr] -> Expr
substEq env eq es = subst su (substEqCoerce env eq es)
  where su = mkSubst $ zip (eqArgNames eq) es

substEqCoerce :: SEnv Sort -> Equation -> [Expr] -> Expr
substEqCoerce env eq es = Vis.applyCoSub coSub $ eqBody eq
  where 
    ts    = snd    <$> eqArgs eq
    sp    = panicSpan "mkCoSub"
    eTs   = sortExpr sp env <$> es
    coSub = mkCoSub env eTs ts

mkCoSub :: SEnv Sort -> [Sort] -> [Sort] -> Vis.CoSub
mkCoSub env eTs xTs = M.fromList [ (x, unite ys) | (x, ys) <- Misc.groupList xys ] 
  where
    unite ts    = Mb.fromMaybe (uError ts) (unifyTo1 senv ts)
    senv        = mkSearchEnv env
    uError ts   = panic ("mkCoSub: cannot build CoSub for " ++ showpp xys ++ " cannot unify " ++ showpp ts) 
    xys         = Misc.sortNub $ concat $ zipWith matchSorts _xTs _eTs
    (_xTs,_eTs) = (xTs, eTs)

matchSorts :: Sort -> Sort -> [(Symbol, Sort)]
matchSorts s1 s2 = go s1 s2
  where
    go (FObj x)      {-FObj-} y    = [(x, y)]
    go (FAbs _ t1)   (FAbs _ t2)   = go t1 t2
    go (FFunc s1 t1) (FFunc s2 t2) = go s1 s2 ++ go t1 t2
    go (FApp s1 t1)  (FApp s2 t2)  = go s1 s2 ++ go t1 t2
    go _             _             = []

--------------------------------------------------------------------------------

eqArgNames :: Equation -> [Symbol]
eqArgNames = map fst . eqArgs

interpret' :: ConstMap -> Knowledge -> ICtx -> SEnv Sort -> Expr -> Expr
interpret' ie γ ctx env e = mytracepp ("Interpreting " ++ show e) $ interpret ie γ ctx env e

interpret :: ConstMap -> Knowledge -> ICtx -> SEnv Sort -> Expr -> Expr
interpret _  _ _   _   e@(ESym _)       = e
interpret _  _ _   _   e@(ECon _)       = e
interpret ie γ ctx env (EVar sym)
    | Just e' <- M.lookup (EVar sym) (icSimpl ctx)
    = interpret' ie γ ctx env e'
interpret _  _ _   _   e@(EVar _)       = e
interpret ie γ ctx env   (EApp e1 e2)
  | isSetPred e1                        = let e2' = interpret' ie γ ctx env e2 in 
                                             applySetFolding e1 e2'
interpret ie γ ctx env e@(EApp _ _)     = case splitEApp e of
  (f, es) -> let (f':es') = map (interpret' ie γ ctx env) (f:es) in interpretApp ie γ ctx env f' es'
    where
      interpretApp ie γ ctx env (EVar f) es
        | Just eq <- M.lookup f (knAms γ)
        , length (eqArgs eq) <= length es 
        = let (es1,es2) = splitAt (length (eqArgs eq)) es
              ges       = substEq env eq es1
              exp       = unfoldExpr ie γ ctx env ges 
              exp'      = eApps exp es2 in  --exp' -- TODO undo
            if (eApps (EVar f) es) == exp' then exp' else interpret' ie γ ctx env exp'

      interpretApp ie γ ctx env (EVar f) (e1:es)
        | (EVar dc, as) <- splitEApp e1
        , Just rw <- M.lookup (f, dc) (knSims γ)
        , length as == length (smArgs rw)
        = let e' = eApps (subst (mkSubst $ zip (smArgs rw) as) (smBody rw)) es in --e' -- TODO undo
            if (eApps (EVar f) es) == e' then e' else interpret' ie γ ctx env e' 

      interpretApp _  γ _   _   (EVar f) ([e0])
        | (EVar dc, _as) <- splitEApp e0
        , isTestSymbol f
        = if testSymbol dc == f then PTrue else 
            if S.member dc (knAllDCs γ) then PFalse else {-simplify γ ctx $-} eApps (EVar f) [e0]

      interpretApp _  _ _   _   f        es     = {-simplify γ ctx $-} eApps f es
interpret ie γ ctx env   (ENeg e1)      = let e1' = interpret' ie γ ctx env e1 in
                                            applyConstantFolding Minus (ECon (I 0)) e1'
--                                          simplify γ ctx (ENeg e1')
interpret ie γ ctx env   (EBin o e1 e2) = let e1' = interpret' ie γ ctx env e1 
                                              e2' = interpret' ie γ ctx env e2 in
                                            applyConstantFolding o e1' e2'
--                                          simplify γ ctx (EBin o e1' e2')
interpret ie γ ctx env   (EIte g e1 e2) = let b = interpret' ie γ ctx env g in
                                            if b == PTrue then interpret' ie γ ctx env e1 else
                                              if b == PFalse then interpret' ie γ ctx env e2 else 
                                                simplify γ ctx $ EIte b e1 e2
--                                          EIte b (interpret' γ ctx env e1) (interpret' γ ctx env e2)
interpret ie γ ctx env   (ECst e1 s)    = let e1' = interpret' ie γ ctx env e1 in
                                            simplifyCasts e1' s -- ECst e1' s
interpret ie γ ctx env (ELam (x,s) e)   = let γ' = γ { knLams = (x, s) : knLams γ }
                                              e' = interpret' ie γ' ctx env e in 
                                            ELam (x, s) e'
interpret ie γ ctx env   (ETApp e1 t)   = let e1' = interpret' ie γ ctx env e1 in ETApp e1' t
interpret ie γ ctx env   (ETAbs e1 sy)  = let e1' = interpret' ie γ ctx env e1 in ETAbs e1' sy
interpret ie γ ctx env   (PAnd es)      = let es' = map (interpret' ie γ ctx env) es in go [] (reverse es')
  where
    go []  []         = PTrue
    go [p] []         = interpret' ie γ ctx env p
    go acc []         = PAnd acc
    go acc (PTrue:es) = go acc es
    go _   (PFalse:_) = PFalse
    go acc (e:es)     = go (e:acc) es
interpret ie γ ctx env (POr es)         = let es' = map (interpret' ie γ ctx env) es in go [] (reverse es')
  where
    go []  []          = PFalse
    go [p] []          = interpret' ie γ ctx env p
    go acc []          = POr acc
    go _   (PTrue:_)   = PTrue
    go acc (PFalse:es) = go acc es
    go acc (e:es)      = go (e:acc) es
interpret ie γ ctx env (PNot e)         = let e' = interpret' ie γ ctx env e in case e' of
    (PNot e'')    -> e''
    PTrue         -> PFalse 
    PFalse        -> PTrue 
    _             -> PNot e'
interpret ie γ ctx env (PImp e1 e2)     = let e1' = interpret' ie γ ctx env e1 
                                              e2' = interpret' ie γ ctx env e2 in
                                            if e1' == PFalse || e2' == PTrue then PTrue else
                                              if e1' == PTrue then e2' else
                                                if e2' == PFalse then interpret' ie γ ctx env (PNot e1') else 
                                                  PImp e1' e2'
interpret ie γ ctx env (PIff e1 e2)     = let e1' = interpret' ie γ ctx env e1 
                                              e2' = interpret' ie γ ctx env e2 in
                                            if e1' == PTrue then e2' else
                                              if e2' == PTrue then e1' else
                                                if e1' == PFalse then interpret' ie γ ctx env (PNot e2') else
                                                  if e2' == PFalse then interpret' ie γ ctx env (PNot e1') else
                                                    PIff e1' e2'
interpret ie γ ctx env (PAtom o e1 e2)  = let e1' = interpret' ie γ ctx env e1
                                              e2' = interpret' ie γ ctx env e2 in
                                            applyBooleanFolding o e1' e2'
interpret _  _ _   _   e@(PKVar _ _)    = e
interpret ie γ ctx env e@(PAll xss e1)  = case xss of
  [] -> interpret' ie γ ctx env e1
  _  -> e
interpret ie γ ctx env e@(PExist xss e1) = case xss of
  [] -> interpret' ie γ ctx env e1
  _  -> e
interpret _  _ _   _   e@(PGrad _ _ _ _) = e
interpret ie γ ctx env (ECoerc s t e)    = let e' = interpret' ie γ ctx env e in
                                             if s == t then e' else (ECoerc s t e')

        
--------------------------------------------------------------------------------
-- | Knowledge (SMT Interaction)
--------------------------------------------------------------------------------
data Knowledge = KN 
  { knSims              :: M.HashMap (Symbol, Symbol) Rewrite  -- ^ Rewrite rules came from match and data type definitions 
  , knAms               :: M.HashMap Symbol Equation  -- ^ All function definitions -- restore ! here?
  , knLams              :: ![(Symbol, Sort)]
  , knSummary           :: ![(Symbol, Int)]     -- ^ summary of functions to be evaluates (knSims and knAsms) with their arity
  , knDCs               :: !(S.HashSet Symbol)  -- ^ data constructors drawn from Rewrite 
  , knAllDCs            :: !(S.HashSet Symbol)  -- ^ 
  , knSels              :: !SelectorMap 
  , knConsts            :: !ConstDCMap
  }

knowledge :: SInfo a -> Knowledge
knowledge si = KN 
  { knSims                     = M.fromList $ (\r -> ((smName r, smDC r), r)) <$> sims 
  , knAms                      = M.fromList $ (\a -> (eqName a, a)) <$> aenvEqs aenv
  , knLams                     = [] 
  , knSummary                  =    ((\s -> (smName s, 1)) <$> sims) 
                                 ++ ((\s -> (eqName s, length (eqArgs s))) <$> aenvEqs aenv)
  , knDCs                      = S.fromList (smDC <$> sims)  <> constNames si
  , knAllDCs                   = S.fromList $ (val . dcName) <$> concatMap ddCtors (ddecls si)
  , knSels                     = M.fromList . Mb.catMaybes $ map makeSel  sims 
  , knConsts                   = M.fromList . Mb.catMaybes $ map makeCons sims 
  } 
  where 
    sims = aenvSimpl aenv  
    aenv = ae si 

    makeCons rw 
      | null (syms $ smBody rw)
      = Just (smName rw, (smDC rw, smBody rw))
      | otherwise
      = Nothing 

    makeSel rw 
      | EVar x <- smBody rw
      = (smName rw,) . (smDC rw,) <$> L.elemIndex x (smArgs rw)
      | otherwise 
      = Nothing 

    constNames si = (S.fromList . fst . unzip . toListSEnv . gLits $ si) `S.union`
                      (S.fromList . fst . unzip . toListSEnv . dLits $ si)
-- testSymbol (from names)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- (sel_i, D, i), meaning sel_i (D x1 .. xn) = xi, 
-- i.e., sel_i selects the ith value for the data constructor D  
type SelectorMap = M.HashMap Symbol (Symbol, Int)
type ConstDCMap  = M.HashMap Symbol (Symbol, Expr)

-- ValueMap maps expressions to constants (including data constructors)
type ConstMap = M.HashMap Expr Expr
type LDataCon = Symbol              -- Data Constructors 

isSimplification :: S.HashSet LDataCon -> (Expr,Expr) -> Bool 
isSimplification dcs (_,c) = isConstant dcs c 
  
isConstant :: S.HashSet LDataCon -> Expr -> Bool 
isConstant dcs e = S.null (S.difference (exprSymbolsSet e) dcs) 

class Simplifiable a where 
  simplify :: Knowledge -> ICtx -> a -> a 


instance Simplifiable Expr where 
  simplify γ ictx e = mytracepp ("simplification of " ++ show e) $ fix (Vis.mapExpr tx) e 
    where 
      fix f e = if e == e' then e else fix f e' where e' = f e 
      tx e 
        | Just e' <- M.lookup e (icSimpl ictx)
        = e' 

      tx (PAtom rel e1 e2) = applyBooleanFolding rel e1 e2
      tx (EBin bop e1 e2) = applyConstantFolding bop e1 e2
      tx (ENeg e)         = applyConstantFolding Minus (ECon (I 0)) e
      tx (EApp e1 e2)
        | isSetPred e1    = applySetFolding e1 e2

      tx (EApp (EVar f) a)
        | Just (dc, c)  <- M.lookup f (knConsts γ) 
        , (EVar dc', _) <- splitEApp a
        , dc == dc' 
        = c
      tx (EIte b e1 e2)
        | isTautoPred b  = e1 
        | isContraPred b = e2
      tx (ECst e s)       = simplifyCasts e s
      tx (ECoerc s t e)
        | s == t = e 
      tx (EApp (EVar f) a)
        | Just (dc, i)  <- M.lookup f (knSels γ) 
        , (EVar dc', es) <- splitEApp a
        , dc == dc' 
        = es!!i
      tx (PAnd es)         = go [] (reverse es)
        where
          go []  []     = PTrue
          go [p] []     = p
          go acc []     = PAnd acc
          go acc (e:es) = if e == PTrue then go acc es
                                  else if e == PFalse then PFalse else go (e:acc) es
      tx (POr es)          = go [] (reverse es)
        where
          go []  []     = PFalse
          go [p] []     = p
          go acc []     = POr acc
          go acc (e:es) = if e == PTrue then PTrue
                                  else if e == PFalse then go acc es else go (e:acc) es
      tx (PNot e)          = if e == PTrue then PFalse 
                                else if e == PFalse then PTrue 
                                else PNot e
      tx e = e
      
simplifyCasts :: Expr -> Sort -> Expr
simplifyCasts (ECon (I n)) FInt  = ECon (I n)
simplifyCasts (ECon (R x)) FReal = ECon (R x)
simplifyCasts e            s     = ECst e s

-------------------------------------------------------------------------------
-- | Normalization of Equation: make their arguments unique -------------------
-------------------------------------------------------------------------------

class Normalizable a where 
  normalize :: a -> a 

instance Normalizable (GInfo c a) where 
  normalize si = si {ae = normalize $ ae si}

instance Normalizable AxiomEnv where 
  normalize aenv = aenv { aenvEqs   = {-notracepp-} mytracepp "aenvEqs"   (normalize <$> aenvEqs   aenv)
                        , aenvSimpl = {-notracepp-} mytracepp "aenvSimpl" (normalize <$> aenvSimpl aenv) }

instance Normalizable Rewrite where 
  normalize rw = rw { smArgs = xs', smBody = normalizeBody (smName rw) $ subst su $ smBody rw }
    where 
      su  = mkSubst $ zipWith (\x y -> (x,EVar y)) xs xs'
      xs  = smArgs rw 
      xs' = zipWith mkSymbol xs [0..]
      mkSymbol x i = x `suffixSymbol` intSymbol (smName rw) i 

instance Normalizable Equation where 
  normalize eq = eq {eqArgs = zip xs' ss, 
                     eqBody = normalizeBody (eqName eq) $ subst su $ eqBody eq }
    where 
      su           = mkSubst $ zipWith (\x y -> (x,EVar y)) xs xs'
      (xs,ss)      = unzip (eqArgs eq) 
      xs'          = zipWith mkSymbol xs [0..]
      mkSymbol x i = x `suffixSymbol` intSymbol (eqName eq) i 

normalizeBody :: Symbol -> Expr -> Expr
normalizeBody f = go   
  where 
    go e 
      | any (== f) (syms e) 
      = go' e 
    go e 
      = e 
    
    go' (PAnd [PImp c e1,PImp (PNot c') e2])
      | c == c' = EIte c e1 (go' e2)
    go' e = e 
