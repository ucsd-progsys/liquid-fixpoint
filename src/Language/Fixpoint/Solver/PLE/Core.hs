{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Fixpoint.Solver.PLE.Core
  ( FuelCount(..)
  , EvalST
  , EvalEnv(..)
  , EvalType(..)
  , EvEqualities
  , ICtx(..)
  , Knowledge(..)
  , defFuelCount
  , eval
  )
  where

import           Language.Fixpoint.Types hiding (simplify)
import           Language.Fixpoint.Types.Config  as FC
import qualified Language.Fixpoint.Types.Visitor as Vis
import qualified Language.Fixpoint.Misc          as Misc
import qualified Language.Fixpoint.Smt.Interface as SMT
import           Language.Fixpoint.SortCheck
import           Language.Fixpoint.Solver.Simplify

import           Control.Monad (when)
import           Control.Monad.State
import           Data.Bifunctor (second)
import qualified Data.HashMap.Strict  as M
import qualified Data.HashSet         as S
import qualified Data.List            as L
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe           as Mb

mytracepp :: (PPrint a) => String -> a -> a
mytracepp = notracepp

----------------------------------------------------------------------------------------------
-- | @ICtx@ is the local information -- at each trie node -- obtained by incremental PLE
----------------------------------------------------------------------------------------------

data ICtx    = ICtx
  { icAssms    :: S.HashSet Pred            -- ^ Equalities converted to SMT format
  , icCands    :: S.HashSet Expr            -- ^ "Candidates" for unfolding
  , icEquals   :: EvEqualities              -- ^ Accumulated equalities
  , icSimpl    :: !ConstMap                 -- ^ Map of expressions to constants
  , icSubcId   :: Maybe SubcId              -- ^ Current subconstraint ID
  , icANFs     :: [[(Symbol, SortedReft)]]  -- Hopefully contain only ANF things
  }

type EvEqualities = S.HashSet (Expr, Expr)

--------------------------------------------------------------------------------
data EvalEnv s = EvalEnv
  { evEnv      :: !SymEnv
    -- | Equalities where we couldn't evaluate the guards
  , evPendingUnfoldings :: M.HashMap Expr Expr
  , evNewEqualities :: EvEqualities -- ^ Equalities discovered during a traversal of
                                    -- an expression
  , evSMTCache :: M.HashMap Expr Bool -- ^ Whether an expression is valid or its negation
  , evFuel     :: FuelCount

  -- Other parameters
  , evExtra :: s
  }

data FuelCount = FC
  { fcMap :: M.HashMap Symbol Int
  , fcMax :: Maybe Int
  }
  deriving (Show)

defFuelCount :: Config -> FuelCount
defFuelCount cfg = FC mempty (fuel cfg)

type EvalST s a = StateT (EvalEnv s) IO a
--------------------------------------------------------------------------------

-- The FuncNormal and RWNormal evaluation strategies are used for REST
-- For example, consider the following function:
--   add(x, y) = if x == 0 then y else add(x - 1, y + 1)
-- And a rewrite rule:
--   forall a, b . add(a,b) -> add(b, a)
-- Then the expression add(t, add(2, 1)) would evaluate under NoRW to:
--   if t == 0 then 3 else add(t - 1, 4)
-- However, under FuncNormal, it would evaluate to: add(t, 3)
-- Thus, FuncNormal could engage the rewrite rule add(t, 3) = add(3, t)


data EvalType =
    NoRW       -- Normal PLE
  | FuncNormal -- REST: Expand function definitions only when the branch can be decided
  | RWNormal   -- REST: Fully Expand Defs in the context of rewriting (similar to NoRW)
  deriving (Eq)

-- Indicates whether or not the evaluation has expanded a function statement
-- into a conditional branch.
-- In this case, rewriting should stop
-- It's unclear whether or not rewriting in either branch makes sense,
-- since one branch could be an ill-formed expression.
newtype FinalExpand = FE Bool deriving (Show)

noExpand :: FinalExpand
noExpand = FE False

expand :: FinalExpand
expand = FE True

mapFE :: (Expr -> Expr) -> (Expr, FinalExpand) -> (Expr, FinalExpand)
mapFE f (e, fe) = (f e, fe)

feVal :: FinalExpand -> Bool
feVal (FE f) = f

feAny :: [FinalExpand] -> FinalExpand
feAny xs = FE $ any feVal xs

infixl 9 <|>
(<|>) :: FinalExpand -> FinalExpand -> FinalExpand
(<|>) (FE True) _ = expand
(<|>) _         f = f


feSeq :: [(Expr, FinalExpand)] -> ([Expr], FinalExpand)
feSeq xs = (map fst xs, feAny (map snd xs))

-- | Unfolds function invocations in expressions.
--
-- Also reduces if-then-else when the boolean condition or the negation can be
-- proved valid. This is the actual implementation of guard-validation-before-unfolding
-- that is described in publications.
--
-- Also adds to the monad state all the unfolding equalities that have been
-- discovered as necessary.
--
eval :: Knowledge -> ICtx -> EvalType -> Expr -> EvalST s (Expr, FinalExpand)
eval γ ctx et = go
  where
    go (ELam (x,s) e)   = evalELam γ ctx et (x, s) e
    go e@EIte{}         = evalIte γ ctx et e
    go (ECoerc s t e)   = mapFE (ECoerc s t)  <$> go e
    go e@(EApp _ _)     =
      case splitEAppThroughECst e of
       (f, es) | et == RWNormal ->
          -- Just evaluate the arguments first, to give rewriting a chance to step in
          -- if necessary
          do
            (es', finalExpand) <- feSeq <$> mapM (eval γ ctx et) es
            if es /= es'
              then return (eApps f es', finalExpand)
              else do
                (f', fe)  <- eval γ ctx et f
                (me', fe') <- evalApp γ ctx f' es et
                return (Mb.fromMaybe (eApps f' es') me', fe <|> fe')
       (f, es) ->
          do
            (f':es', fe) <- feSeq <$> mapM (eval γ ctx et) (f:es)
            (me', fe') <- evalApp γ ctx f' es' et
            return (Mb.fromMaybe (eApps f' es') me', fe <|> fe')

    go (PAtom r e1 e2) = binOp (PAtom r) e1 e2
    go (ENeg e)         = do (e', fe)  <- eval γ ctx et e
                             return (ENeg e', fe)
    go (EBin o e1 e2)   = do (e1', fe1) <- eval γ ctx et e1
                             (e2', fe2) <- eval γ ctx et e2
                             return (EBin o e1' e2', fe1 <|> fe2)
    go (ETApp e t)      = mapFE (`ETApp` t) <$> go e
    go (ETAbs e s)      = mapFE (`ETAbs` s) <$> go e
    go (PNot e')        = mapFE PNot <$> go e'
    go (PImp e1 e2)     = binOp PImp e1 e2
    go (PIff e1 e2)     = binOp PIff e1 e2
    go (PAnd es)        = efAll PAnd (go `traverse` es)
    go (POr es)         = efAll POr (go `traverse` es)
    go e | EVar _ <- dropECst e = do
      (me', fe) <- evalApp γ ctx e [] et
      return (Mb.fromMaybe e me', fe)
    go (ECst e t)       = do (e', fe) <- eval γ ctx et e
                             return (ECst e' t, fe)
    go e                = return (e, noExpand)

    binOp f e1 e2 = do
      (e1', fe1) <- go e1
      (e2', fe2) <- go e2
      return (f e1' e2', fe1 <|> fe2)

    efAll f mes = do
      xs <- mes
      let (xs', fe) = feSeq xs
      return (f xs', fe)

-- | 'evalELamb' produces equations that preserve the context of a rewrite
-- so equations include any necessary lambda bindings.
evalELam :: Knowledge -> ICtx -> EvalType -> (Symbol, Sort) -> Expr -> EvalST s (Expr, FinalExpand)
evalELam γ ctx et (x, s) e = do
    oldPendingUnfoldings <- gets evPendingUnfoldings
    oldEqs <- gets evNewEqualities
    (e', fe) <- eval (γ { knLams = (x, s) : knLams γ }) ctx et e
    let e2' = simplify γ ctx e'
        elam = ELam (x, s) e
    -- Discard the old equalities which miss the lambda binding
    modify $ \st -> st
      { evPendingUnfoldings = oldPendingUnfoldings
      , evNewEqualities = S.insert (elam, ELam (x, s) e2') oldEqs
      }
    return (elam, fe)

-- | @evalApp kn ctx e es@ unfolds expressions in @eApps e es@ using rewrites
-- and equations
evalApp :: Knowledge -> ICtx -> Expr -> [Expr] -> EvalType -> EvalST s (Maybe Expr, FinalExpand)
evalApp γ ctx e0 es et
  | EVar f <- dropECst e0
  , Just eq <- Map.lookup f (knAms γ)
  , length (eqArgs eq) <= length es
  = do
       env  <- gets (seSort . evEnv)
       okFuel <- checkFuel f
       if okFuel && et /= FuncNormal
         then do
                let (es1,es2) = splitAt (length (eqArgs eq)) es
                    newE = substEq env eq es1
                (e', fe) <- evalIte γ ctx et newE -- TODO:FUEL this is where an "unfolding" happens, CHECK/BUMP counter
                let e2' = stripPLEUnfold e'
                    e3' = simplify γ ctx (eApps e2' es2) -- reduces a bit the equations
                    undecidedGuards = case e' of
                      EIte{} -> True
                      _ -> False

                if undecidedGuards
                  then do
                    modify $ \st ->
                      st {
                        evPendingUnfoldings = M.insert (eApps e0 es) e3' (evPendingUnfoldings st)
                      }
                    -- Don't unfold the expression if there is an if-then-else
                    -- guarding it, just to preserve the size of further
                    -- rewrites.
                    return (Nothing, noExpand)
                  else do
                    useFuel f
                    modify $ \st ->
                      st
                        { evNewEqualities = S.insert (eApps e0 es, e3') (evNewEqualities st)
                        , evPendingUnfoldings = M.delete (eApps e0 es) (evPendingUnfoldings st)
                        }
                    return (Just e2', fe)
         else return (Nothing, noExpand)
  where
    -- At the time of writing, any function application wrapping an
    -- if-statement would have the effect of unfolding the invocation.
    -- However, using pleUnfold still has the advantage of not generating
    -- extra equations to unfold pleUnfold itself. Using pleUnfold also
    -- makes the intention of the user rather explicit.
    stripPLEUnfold e
      | (ef, [arg]) <- splitEAppThroughECst e
      , EVar f <- dropECst ef
      , f == "Language.Haskell.Liquid.ProofCombinators.pleUnfold"
      = arg
      | otherwise = e

evalApp γ ctx e0 args@(e:es) _
  | EVar f <- dropECst e0
  , (d, as) <- splitEAppThroughECst e
  , EVar dc <- dropECst d
  , Just rws <- Map.lookup dc (knSims γ)
    -- User data measures aren't sent to the SMT solver because
    -- it knows already about selectors and constructor tests.
  , Just (rw, isUserDataSMeasure) <- L.find (\(rw, _) -> smName rw == f) rws
  , length as == length (smArgs rw)
  = do
    let newE = eApps (subst (mkSubst $ zip (smArgs rw) as) (smBody rw)) es
    when (isUserDataSMeasure == NoUserDataSMeasure) $
      modify $ \st ->
        st { evNewEqualities =
               S.insert (eApps e0 args, simplify γ ctx newE) (evNewEqualities st)
           }
    return (Just newE, noExpand)

evalApp γ ctx e0 es _et
  | eqs@(_:_) <- noUserDataMeasureEqs γ (eApps e0 es)
  = do
       let eqs' = map (second $ simplify γ ctx) eqs
       modify $ \st ->
         st { evNewEqualities = foldr S.insert (evNewEqualities st) eqs' }
       return (Nothing, noExpand)

evalApp _ _ _e _es _
  = return (Nothing, noExpand)

-- | Evaluates if-then-else statements until they can't be evaluated anymore
-- or some other expression is found.
evalIte :: Knowledge -> ICtx -> EvalType -> Expr -> EvalST s (Expr, FinalExpand)
evalIte γ ctx et (EIte i e1 e2) = do
      (b, _) <- eval γ ctx et i
      b'  <- mytracepp ("evalEIt POS " ++ showpp (i, b)) <$> isValidCached γ b
      case b' of
        Just True -> evalIte γ ctx et e1
        Just False -> evalIte γ ctx et e2
        _ -> return (EIte b e1 e2, expand)
evalIte _ _ _ e' = return (e', noExpand)

-- | Creates equations that explain how to rewrite a given constructor
-- application with all measures that aren't user data measures
noUserDataMeasureEqs :: Knowledge -> Expr -> [(Expr,Expr)]
noUserDataMeasureEqs γ e =
  [ (EApp (EVar $ smName rw) e, subst (mkSubst $ zip (smArgs rw) es) (smBody rw))
  | (ef, es) <- [splitEAppThroughECst e]
  , EVar f <- [dropECst ef]
  , Just rws <- [Map.lookup f (knSims γ)]
  , (rw, NoUserDataSMeasure) <- rws
  , length es == length (smArgs rw)
  ]

--------------------------------------------------------------------------------
-- | 'substEq' unfolds or instantiates an equation at a particular list of
--   argument values. We must also substitute the sort-variables that appear
--   as coercions. See tests/proof/ple1.fq
--------------------------------------------------------------------------------
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

-- | @mkCoSub senv eTs xTs = su@ creates a substitution @su@ such that
-- @subst su xTs == eTs@.
--
-- The variables in the domain of the substitution are those that appear
-- as @FObj symbol@ in @xTs@.
mkCoSub :: SEnv Sort -> [Sort] -> [Sort] -> Vis.CoSub
mkCoSub env eTs xTs = M.fromList [ (x, unite ys) | (x, ys) <- Misc.groupList xys ]
  where
    unite ts    = Mb.fromMaybe (uError ts) (unifyTo1 symToSearch ts)
    symToSearch = mkSearchEnv env
    uError ts   = panic ("mkCoSub: cannot build CoSub for " ++ showpp xys ++ " cannot unify " ++ showpp ts)
    xys         = Misc.sortNub $ concat $ zipWith matchSorts xTs eTs

matchSorts :: Sort -> Sort -> [(Symbol, Sort)]
matchSorts = go
  where
    go (FObj x)      {-FObj-} y    = [(x, y)]
    go (FAbs _ t1)   (FAbs _ t2)   = go t1 t2
    go (FFunc s1 t1) (FFunc s2 t2) = go s1 s2 ++ go t1 t2
    go (FApp s1 t1)  (FApp s2 t2)  = go s1 s2 ++ go t1 t2
    go _             _             = []

--------------------------------------------------------------------------------

eqArgNames :: Equation -> [Symbol]
eqArgNames = map fst . eqArgs

isValidCached :: Knowledge -> Expr -> EvalST s (Maybe Bool)
isValidCached γ e = do
  env <- get
  case M.lookup e (evSMTCache env) of
    Nothing -> do
      let isFreeInE (s, _) = not (S.member s (exprSymbolsSet e))
      b <- liftIO $ knPreds γ (knContext γ) (knLams γ) e
      if b
        then do
          when (all isFreeInE (knLams γ)) $
            put (env { evSMTCache = M.insert e True (evSMTCache env) })
          return (Just True)
        else do
          b2 <- liftIO $ knPreds γ (knContext γ) (knLams γ) (PNot e)
          if b2
            then do
              when (all isFreeInE (knLams γ)) $
                put (env { evSMTCache = M.insert e False (evSMTCache env) })
              return (Just False)
            else
              return Nothing

    mb -> return mb

--------------------------------------------------------------------------------
-- | Knowledge (SMT Interaction)
--------------------------------------------------------------------------------
data Knowledge = KN
  { -- | Rewrites rules came from match definitions
    --
    -- They are grouped by the data constructor that they unfold, and are
    -- augmented with an attribute that say whether they originate from a
    -- user data declaration.
    knSims              :: Map Symbol [(Rewrite, IsUserDataSMeasure)]
  , knAms               :: Map Symbol Equation -- ^ All function definitions
  , knContext           :: SMT.Context
  , knPreds             :: SMT.Context -> [(Symbol, Sort)] -> Expr -> IO Bool
  , knLams              :: ![(Symbol, Sort)]
  , knSummary           :: ![(Symbol, Int)]     -- ^ summary of functions to be evaluates (knSims and knAsms) with their arity
  , knDCs               :: !(S.HashSet Symbol)  -- ^ data constructors drawn from Rewrite
  , knDataCtors         :: !(M.HashMap Symbol DataCtor) -- ^ data constructors by name
  , knSels              :: !SelectorMap
  , knConsts            :: !ConstDCMap
  , knAutoRWs           :: M.HashMap SubcId [AutoRewrite]
  }

-- | A type to express whether SMeasures originate from data definitions.
-- That is whether they are constructor tests, selectors, or something else.
data IsUserDataSMeasure = NoUserDataSMeasure | UserDataSMeasure
  deriving (Eq, Show)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- (sel_i, D, i), meaning sel_i (D x1 .. xn) = xi,
-- i.e., sel_i selects the ith value for the data constructor D
type SelectorMap = [(Symbol, (Symbol, Int))]
type ConstDCMap = [(Symbol, (Symbol, Expr))]

-- ValueMap maps expressions to constants (including data constructors)
type ConstMap = M.HashMap Expr Expr

simplify :: Knowledge -> ICtx -> Expr -> Expr
simplify γ ictx exprs = mytracepp ("simplification of " ++ showpp exprs) $ fix' (Vis.mapExprOnExpr tx) exprs
    where
      fix' f e = if e == e' then e else fix' f e' where e' = f e
      tx e
        | Just e' <- M.lookup e (icSimpl ictx)
        = e'

      tx (PAtom rel e1 e2) = applyBooleanFolding rel e1 e2
      tx (EBin bop e1 e2) = applyConstantFolding bop e1 e2
      tx (ENeg e)         = applyConstantFolding Minus (ECon (I 0)) e
      tx (EApp e1 e2)
        | isSetPred e1    = applySetFolding e1 e2

      tx (EApp ef a)
        | EVar f <- dropECst ef
        , Just (dc, c)  <- L.lookup f (knConsts γ)
        , (ed, _) <- splitEAppThroughECst a
        , EVar dc' <- dropECst ed
        , dc == dc'
        = c
      tx (EIte b e1 e2)
        | isTautoPred b  = e1
        | isContraPred b = e2
      tx (ECoerc s t e)
        | s == t = e
      tx (EApp ef a)
        | EVar f <- dropECst ef
        , Just (dc, i)  <- L.lookup f (knSels γ)
        , (ed, es) <- splitEAppThroughECst a
        , EVar dc' <- dropECst ed
        , dc == dc'
        = es!!i
      tx e = e


-- | Increment the fuel count of the given symbol in the current evaluation
-- environment.
useFuel :: Symbol -> EvalST s ()
useFuel f = do
  modify (\st -> st { evFuel = useFuelCount f (evFuel st) })

-- | Increment the fuel count.
useFuelCount :: Symbol -> FuelCount -> FuelCount
useFuelCount f fc = fc { fcMap = M.insert f (k + 1) m }
  where
    k             = M.lookupDefault 0 f m
    m             = fcMap fc

-- | Returns False if there is a fuel count in the evaluation environment and
-- the fuel count exceeds the maximum. Returns True otherwise.
checkFuel :: Symbol -> EvalST s Bool
checkFuel f = do
  fc <- gets evFuel
  case (M.lookup f (fcMap fc), fcMax fc) of
    (Just fk, Just n) -> pure (fk <= n)
    _                 -> pure True
