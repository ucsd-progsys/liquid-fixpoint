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
{-# LANGUAGE FlexibleContexts          #-}
module Language.Fixpoint.Solver.Interpreter (instInterpreter) where

import           Language.Fixpoint.Types hiding (simplify)
import           Language.Fixpoint.Types.Config  as FC
import qualified Language.Fixpoint.Types.Visitor as Vis
import qualified Language.Fixpoint.Misc          as Misc 
import           Language.Fixpoint.Smt.Theories
import           Language.Fixpoint.Defunctionalize
import           Language.Fixpoint.SortCheck
import           Language.Fixpoint.Solver.Sanitize        (symbolEnv)
import           Language.Fixpoint.Solver.Rewrite
import           Control.Monad.State
import           Data.Hashable
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import qualified Data.List            as L
import qualified Data.Maybe           as Mb
import           Debug.Trace                              (trace)

mytracepp :: (PPrint a) => String -> a -> a
mytracepp = notracepp 

traceEval :: String -> Expr -> Expr -> Expr 
traceEval str e1 e2 
  | False 
  = trace (str ++ "EVAL\n" ++ showpp (unElab e1) ++ "\n~>\n" ++ showpp (unElab e2)) e2 
  | otherwise 
  = e2 

_mytrace :: String -> a -> a
_mytrace = {-trace-} flip const

-- NV TODO: we do not even need to return fi' 
-- just check out the constraints that are SAFE! 

instInterpreter :: (PPrint a, Fixpoint a, Loc a) => Config -> SInfo a -> [SubcId] -> IO (SInfo a, [SubcId])
instInterpreter cfg fi bad = 
  let (fi', st) = runState (eval env fi) (ES bsUn M.empty (mkDcs cfg fi) bad) 
  in return (fi', bad L.\\ eunsolved st)
    where 
        kn   = knowledge cfg fi
        env  = EE kn cfg (symbolEnv cfg fi)
        bsUn = mapBindEnv (\_ (x, (RR s (Reft (v, e)))) -> (x, RR s (Reft (v, unElab e)))) (bs fi)


class Evalable a where 
  eval :: EvalEEnv -> a -> State EEvalST a 

instance (PPrint a, Fixpoint a) => Evalable (SInfo a) where 
  eval γ fi = do bad <- eunsolved <$> get 
                 let eval' γ (i,c) = if L.elem i bad then eval γ (i,c) else return (i,c) 
                 cm' <- M.fromList <$> (mapM (eval' γ) (M.toList $ cm fi))
                 return (fi{ cm = cm'}) 

instance PPrint a => Evalable (SubcId, SimpC a) where 
  eval γ (i, c) = do consts <- eeconst <$> get
                     s0 <- get 
                     subs <- makeSubstitutions (_cenv c) (S.difference (exprSymbolsSet (unElab (_crhs c))) consts)  
                     modify $ \s -> s{esubs = M.union subs (esubs s)}
                     rhs' <-  eval γ (_crhs c)
                     modify $ \s -> s{esubs = esubs s0}
                     if isTautoPred rhs' 
                       then modify $ \st -> st {eunsolved = eunsolved st L.\\ [i]}
                       else return ()
                     return (i, c{_crhs = traceEval ("\nRHS " ++ show i  ++ " is solved:" ++ showpp (isTautoPred rhs' ) ++ "\n" ++ showpp subs) (_crhs c) rhs'})

instance Evalable Expr where 
  eval γ e0  = do st    <- get 
                  let e' = interpret' (esubs st) (ekn γ) mempty e
                  return $ if e' == e then e0 else defunc e' 
    where e       = unElab e0 
          defunc  = elaborate (atLoc dummySpan "interp") env . defuncAny (ecfg γ) env 
          env     = eenv γ



makeSubstitutions :: IBindEnv -> S.HashSet Symbol -> State EEvalST ConstMap
makeSubstitutions bs fv = do consts <- eeconst <$> get
                             env    <- ebenv <$> get
                             let xss = envCs env bs
                             let ps  = concat [splitPAnd $ subst1 e' (v, EVar x) | (x, (RR _ (Reft (v, e')))) <- xss ]
                             return $ M.fromList . cutCircles $ go xss ps consts consts [] fv 
  where 
    go xss ps consts checked acc fv = let xts = (concatMap (makeSubstitutionDC fv) xss) ++ (concatMap (makeSubstitutionVar consts fv) ps) in 
                                      let fv' = S.difference (S.unions ((exprSymbolsSet . snd) <$> xts)) checked in 
                                      if S.null fv' then (acc ++ xts)
                                                    else go xss ps consts (S.union checked fv') (acc ++ xts) fv' 

    cutCircles es = let cic = [ (x1, y1) | (EVar x1, EVar y1) <- es
                                         , (EVar x2, EVar y2) <- es
                                         , x1 == y2, y1 == x2] in 
                    let sub = mkSubst $ map (\(x1,x2) -> if S.member x2 fv then (x1, EVar x2) else (x2, EVar x1)) cic in                     
                    filter (\(x1,x2) -> not (x1 == x2)) $ subst sub es


makeSubstitutionDC :: S.HashSet Symbol -> (Symbol, SortedReft) -> [(Expr, Expr)]
makeSubstitutionDC fv (x, (RR _ (Reft (v, e')))) | isDataconSymbol v =
    let subs0 = mkSubst [(x, EVar y) | EEq (EVar x) (EVar y) <- es, S.member y fv] in 
    concat [makeSubst x e | e <- subst subs0 es, x <- S.toList fv]
  where 
    e = subst1 e' (v, EVar x) 
    -- Checkers 
    makeSubst x e@(EApp _ (EVar y)) | x == y 
       = [(e, PTrue)]
    makeSubst x (PNot e@(EApp _ (EVar y))) | x == y 
       = [(e, PFalse)]   
    -- Selectors  
    makeSubst x (EEq el@(EApp _ (EVar y)) er) | x == y 
       = [(el, er)]
    makeSubst _ _ = []
    es = splitPAnd e
makeSubstitutionDC _ _  = []

makeSubstitutionVar :: S.HashSet LDataCon
                    -> S.HashSet Symbol 
                    -> Expr 
                    -> [(Expr, Expr)]
makeSubstitutionVar _ fv (PIff (EVar y) er) 
  | S.member y fv = [(EVar y, er)]
makeSubstitutionVar consts fv (EEq (EVar y) er)  
  | S.member y fv 
  , isConstant consts er || isANFSymbol y 
  = [(EVar y, er)]
makeSubstitutionVar _ _ _ = []  

--------------------------------------------------------------------------------
-- | Evaluation Environments ---------------------------------------------------
--------------------------------------------------------------------------------


data EvalEEnv = EE {ekn :: Knowledge, ecfg :: Config, eenv :: SymEnv}
data EEvalST  = ES {ebenv :: BindEnv, esubs :: ConstMap, eeconst :: S.HashSet Symbol, eunsolved :: [SubcId]}


--------------------------------------------------------------------------------
-- | Helpers -------------------------------------------------------------------
--------------------------------------------------------------------------------


mkDcs :: Config -> SInfo a -> S.HashSet Symbol 
mkDcs _ si = S.fromList (smDC <$> sims)  <> constNames si
  where 
    sims = aenvSimpl (ae si)  
    constNames si = (S.fromList . fst . unzip . toListSEnv . gLits $ si) `S.union`
                    (S.fromList . fst . unzip . toListSEnv . dLits $ si)


interpret' :: ConstMap -> Knowledge -> SEnv Sort -> Expr -> Expr
interpret' ie γ env e = mytracepp ("Interpreting " ++ show e) $ interpret ie γ env e

interpret :: ConstMap -> Knowledge -> SEnv Sort -> Expr -> Expr
interpret _  _   _   e@(ESym _)       = e
interpret _  _   _   e@(ECon _)       = e

interpret ctx γ env e 
    |  Just e' <- M.lookup e ctx 
    = interpret' ctx γ env e'

interpret ctx γ env (EVar sym)
    | Just e' <- M.lookup (EVar sym) ctx
    = interpret' ctx γ env e'
interpret _   _  _   e@(EVar _)         = e
interpret ctx γ  env (EApp e1 e2)
  | isSetPred e1                        = let e2' = interpret' ctx γ env e2 in 
                                             applySetFolding e1 e2'
interpret ctx γ env e@(EApp _ _)     = case splitEApp e of
  (f, es) -> let (f':es') = map (interpret' ctx γ env) (f:es) in interpretApp ctx γ env f' es'
    where
      interpretApp ctx γ env (EVar f) es
        | Just eq <- M.lookup f (knAms γ)
        , length (eqArgs eq) <= length es 
        = let (es1,es2) = splitAt (length (eqArgs eq)) es
              ges       = substEq env eq es1
              exp       = unfoldExpr ctx γ env ges 
              exp'      = eApps exp es2 in  --exp' -- TODO undo
            if (eApps (EVar f) es) == exp' then exp' else interpret' ctx γ env exp'

      interpretApp ctx γ env (EVar f) (e1:es)
        | (EVar dc, as) <- splitEApp e1
        , Just rw <- M.lookup (f, dc) (knSims γ)
        , length as == length (smArgs rw)
        = let e' = eApps (subst (mkSubst $ zip (smArgs rw) as) (smBody rw)) es in --e' -- TODO undo
            if (eApps (EVar f) es) == e' then e' else interpret' ctx γ env e' 

      interpretApp _  γ _    (EVar f) ([e0])
        | (EVar dc, _as) <- splitEApp e0
        , isTestSymbol f
        = if testSymbol dc == f then PTrue else 
            if S.member dc (knAllDCs γ) then PFalse else {-simplify γ ctx $-} eApps (EVar f) [e0]

      interpretApp _ _   _   f        es     = {-simplify γ ctx $-} eApps f es
interpret ctx γ env   (ENeg e1)      = let e1' = interpret' ctx γ  env e1 in
                                            applyConstantFolding Minus (ECon (I 0)) e1'
--                                          simplify γ ctx (ENeg e1')
interpret ctx γ  env   (EBin o e1 e2) = let e1' = interpret' ctx γ env e1 
                                            e2' = interpret' ctx γ env e2 in
                                            applyConstantFolding o e1' e2'
--                                          simplify γ ctx (EBin o e1' e2')
interpret ctx γ env   (EIte g e1 e2) = let b = interpret' ctx γ  env g in
                                            if b == PTrue then interpret' ctx γ  env e1 else
                                              if b == PFalse then interpret' ctx γ  env e2 else 
                                                simplify γ ctx $ EIte b e1 e2
--                                          EIte b (interpret' γ ctx env e1) (interpret' γ ctx env e2)
interpret ctx γ  env   (ECst e1 s)    = let e1' = interpret' ctx γ  env e1 in
                                            simplifyCasts e1' s -- ECst e1' s
interpret ctx γ  env (ELam (x,s) e)   = let γ' = γ { knLams = (x, s) : knLams γ }
                                            e' = interpret' ctx γ' env e in 
                                            ELam (x, s) e'
interpret ctx γ  env   (ETApp e1 t)   = let e1' = interpret' ctx γ  env e1 in ETApp e1' t
interpret ctx γ  env   (ETAbs e1 sy)  = let e1' = interpret' ctx γ  env e1 in ETAbs e1' sy
interpret ctx γ  env   (PAnd es)      = let es' = map (interpret' ctx γ  env) es in go [] (reverse es')
  where
    go []  []     = PTrue
    go [p] []     = interpret' ctx γ env p
    go acc []     = PAnd acc
    go acc (e:es) = if e == PTrue then go acc es
                                  else if e == PFalse then PFalse else go (e:acc) es
interpret ctx γ env (POr es)         = let es' = map (interpret' ctx γ  env) es in go [] (reverse es')
  where
    go []  []     = PFalse
    go [p] []     = interpret' ctx γ  env p
    go acc []     = POr acc
    go acc (e:es) = if e == PTrue then PTrue
                                  else if e == PFalse then go acc es else go (e:acc) es
interpret ctx γ env (PNot e)         = let e' = interpret' ctx γ  env e in case e' of
    (PNot e'')    -> e''
    PTrue         -> PFalse 
    PFalse        -> PTrue 
    _             -> PNot e'
interpret ctx γ  env (PImp e1 e2)     = let e1' = interpret' ctx γ  env e1 
                                            e2' = interpret' ctx γ  env e2 in
                                            if e1' == PFalse || e2' == PTrue then PTrue else
                                              if e1' == PTrue then e2' else
                                                if e2' == PFalse then interpret' ctx γ  env (PNot e1') else 
                                                  PImp e1' e2'
interpret ctx γ  env (PIff e1 e2)     = let e1' = interpret' ctx γ  env e1 
                                            e2' = interpret' ctx γ  env e2 in
                                            if e1' == PTrue then e2' else
                                              if e2' == PTrue then e1' else
                                                if e1' == PFalse then interpret' ctx γ env (PNot e2') else
                                                  if e2' == PFalse then interpret' ctx γ env (PNot e1') else
                                                    PIff e1' e2'
interpret ctx γ  env (PAtom o e1 e2)  = let e1' = interpret' ctx γ env e1
                                            e2' = interpret' ctx γ env e2 in
                                            applyBooleanFolding o e1' e2'
interpret _  _   _   e@(PKVar _ _)    = e
interpret ctx γ  env e@(PAll xss e1)  = case xss of
  [] -> interpret' ctx γ  env e1
  _  -> e
interpret ctx γ  env e@(PExist xss e1) = case xss of
  [] -> interpret' ctx γ  env e1
  _  -> e
interpret _  _   _   e@(PGrad _ _ _ _) = e
interpret ctx γ  env (ECoerc s t e)    = let e' = interpret' ctx γ  env e in
                                             if s == t then e' else (ECoerc s t e')


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
  , knAutoRWs           :: M.HashMap SubcId [AutoRewrite]
  , knRWTerminationOpts :: RWTerminationOpts
  }

knowledge :: Config -> SInfo a -> Knowledge
knowledge cfg si = KN 
  { knSims                     = M.fromList $ (\r -> ((smName r, smDC r), r)) <$> sims 
  , knAms                      = M.fromList $ (\a -> (eqName a, a)) <$> aenvEqs aenv
  , knLams                     = [] 
  , knSummary                  =    ((\s -> (smName s, 1)) <$> sims) 
                                 ++ ((\s -> (eqName s, length (eqArgs s))) <$> aenvEqs aenv)
  , knDCs                      = S.fromList (smDC <$> sims)  <> constNames si
  , knAllDCs                   = S.fromList $ (val . dcName) <$> concatMap ddCtors (ddecls si)
  , knSels                     = M.fromList . Mb.catMaybes $ map makeSel  sims 
  , knConsts                   = M.fromList . Mb.catMaybes $ map makeCons sims 
  , knAutoRWs                  = aenvAutoRW aenv
  , knRWTerminationOpts        = if (rwTerminationCheck cfg)
                                   then RWTerminationCheckEnabled 
                                   else RWTerminationCheckDisabled
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

unfoldExpr :: ConstMap -> Knowledge  -> SEnv Sort -> Expr -> {-EvalST-} Expr
unfoldExpr ctx γ env (EIte e0 e1 e2) = let g' = interpret' ctx γ  env e0 in
                                             if g' == PTrue
                                                then unfoldExpr ctx γ  env e1
                                                else if g' == PFalse
                                                        then unfoldExpr ctx γ env e2
                                                        else EIte g' e1 e2
unfoldExpr _  _ _     e               = e

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

isConstant :: S.HashSet LDataCon -> Expr -> Bool 
isConstant dcs e = S.null (S.difference (exprSymbolsSet e) dcs) 

class Simplifiable a where 
  simplify :: Knowledge -> ConstMap -> a -> a 


instance Simplifiable Expr where 
  simplify γ ictx e = mytracepp ("simplification of " ++ show e) $ fix (Vis.mapExpr tx) e 
    where 
      fix f e = if e == e' then e else fix f e' where e' = f e 
      tx e 
        | Just e' <- M.lookup e ictx
        = e' 
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
      tx (PAtom rel e1 e2) = applyBooleanFolding rel e1 e2
      tx e = e
      
applyBooleanFolding :: Brel -> Expr -> Expr -> Expr
applyBooleanFolding brel e1 e2 = 
  case (e1, e2) of 
    ((ECon (R left)), (ECon (R right))) ->
      Mb.fromMaybe e (bfR brel left right)
    ((ECon (R left)), (ECon (I right))) ->
      Mb.fromMaybe e (bfR brel left (fromIntegral right))
    ((ECon (I left)), (ECon (R right))) ->
      Mb.fromMaybe e (bfR brel (fromIntegral left) right)
    ((ECon (I left)), (ECon (I right))) ->
      Mb.fromMaybe e (bfI brel left right)
    _ -> if isTautoPred e then PTrue else 
           if isContraPred e then PFalse else e
  where
    e = PAtom brel e1 e2
    
    getOp :: Ord a => Brel -> (a -> a -> Bool)
    getOp Gt   =  (>)
    getOp Ge   =  (>=)
    getOp Lt   =  (<)
    getOp Le   =  (<=)
    getOp Eq   =  (==)
    getOp Ne   =  (/=)
    getOp Ueq  =  (==)
    getOp Une  =  (/=)

    bfR :: Brel -> Double -> Double -> Maybe Expr
    bfR brel left right = if (getOp brel) left right then Just PTrue else Just PFalse

    bfI :: Brel -> Integer -> Integer -> Maybe Expr
    bfI brel left right = if (getOp brel) left right then Just PTrue else Just PFalse
        

applyConstantFolding :: Bop -> Expr -> Expr -> Expr
applyConstantFolding bop e1 e2 =
  case (e1, e2) of
    ((ECon (R left)), (ECon (R right))) ->
      Mb.fromMaybe e (cfR bop left right)
    ((ECon (R left)), (ECon (I right))) ->
      Mb.fromMaybe e (cfR bop left (fromIntegral right))
    ((ECon (I left)), (ECon (R right))) ->
      Mb.fromMaybe e (cfR bop (fromIntegral left) right)
    ((ECon (I left)), (ECon (I right))) ->
      Mb.fromMaybe e (cfI bop left right)
    (EBin Mod  _   _              , _)  -> e
    (EBin bop1 e11 (ECon (R left)), ECon (R right))
      | bop == bop1 -> Mb.fromMaybe e ((EBin bop e11) <$> (cfR (rop bop) left right))
      | otherwise   -> e
    (EBin bop1 e11 (ECon (R left)), ECon (I right))
      | bop == bop1 -> Mb.fromMaybe e ((EBin bop e11) <$> (cfR (rop bop) left (fromIntegral right)))
      | otherwise   -> e
    (EBin bop1 e11 (ECon (I left)), ECon (R right))
      | bop == bop1 -> Mb.fromMaybe e ((EBin bop e11) <$> (cfR (rop bop) (fromIntegral left) right))
      | otherwise   -> e
    (EBin bop1 e11 (ECon (I left)), ECon (I right))
      | bop == bop1 -> Mb.fromMaybe e ((EBin bop e11) <$> (cfI (rop bop) left right))
      | otherwise   -> e
    _ -> e
  where
    
    rop :: Bop -> Bop
    rop Plus   = Plus
    rop Minus  = Plus
    rop Times  = Times
    rop Div    = Times
    rop RTimes = RTimes
    rop RDiv   = RTimes
    rop Mod    = Mod

    e = EBin bop e1 e2
    
    getOp :: Num a => Bop -> Maybe (a -> a -> a)
    getOp Minus    = Just (-)
    getOp Plus     = Just (+)
    getOp Times    = Just (*)
    getOp RTimes   = Just (*)
    getOp _        = Nothing

    cfR :: Bop -> Double -> Double -> Maybe Expr
    cfR bop left right = fmap go (getOp' bop)
      where
        go f = ECon $ R $ f left right
        
        getOp' Div      = Just (/)
        getOp' RDiv     = Just (/)
        getOp' op       = getOp op

    cfI :: Bop -> Integer -> Integer -> Maybe Expr
    cfI bop left right = fmap go (getOp' bop)
      where
        go f = ECon $ I $ f left right
        
        getOp' Mod = Just mod
        getOp' op  = getOp op

isSetPred :: Expr -> Bool
isSetPred (EVar s) | s == setEmp          = True
isSetPred (EApp e1 _) = case e1 of
  (EVar s) | s == setMem || s == setSub  -> True
  _                                      -> False
isSetPred _                               = False

-- Note: this is currently limited to sets of integer constants
applySetFolding :: Expr -> Expr -> Expr
applySetFolding e1 e2   = case e1 of
    (EVar s) | s == setEmp
      -> Mb.fromMaybe e $ pure (fromBool . S.null)   <*> evalSetI e2
    (EApp (EVar s) e1') | s == setMem
      -> Mb.fromMaybe e $ fromBool <$> (S.member <$> getInt e1' <*> evalSetI e2)
                        | s == setEmp
      -> Mb.fromMaybe e $ fromBool <$> (S.null <$> (S.difference <$> evalSetI e1' <*> evalSetI e2))
                        | otherwise 
      -> e
    _                   -> e
  where
    e = EApp e1 e2

    fromBool True  = PTrue
    fromBool False = PFalse

    getInt :: Expr -> Maybe Integer
    getInt (ECon (I n)) = Just n
    getInt _            = Nothing
    
    getOp :: (Eq a, Hashable a) => Symbol -> Maybe (S.HashSet a -> S.HashSet a -> S.HashSet a)
    getOp s | s == setCup = Just S.union
            | s == setCap = Just S.intersection
            | s == setDif = Just S.difference
            | otherwise   = Nothing

    evalSetI :: Expr -> Maybe (S.HashSet Integer)
    evalSetI (EApp e1 e2) = case e1 of
      (EVar s) | s == setEmpty -> Just S.empty
               | s == setSng   -> case e2 of
        (ECon (I n))             -> Just $ S.singleton n
        _                        -> Nothing
      (EApp (EVar f) e1')  -> getOp f <*> evalSetI e1' <*> evalSetI e2
      _                    -> Nothing   
    evalSetI _            = Nothing

simplifyCasts :: Expr -> Sort -> Expr
simplifyCasts (ECon (I n)) FInt  = ECon (I n)
simplifyCasts (ECon (R x)) FReal = ECon (R x)
simplifyCasts e            s     = ECst e s

