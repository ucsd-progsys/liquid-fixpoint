{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Language.Fixpoint.Solver.Rewrite
  ( getRewrite
  , subExprs
  , unify
  , ordConstraints
  , convert
  , passesTerminationCheck
  , RewriteArgs(..)
  , RWTerminationOpts(..)
  , SubExpr
  , TermOrigin(..)
  , OCType
  , RESTOrdering(..)
  ) where

import           Control.Monad (guard)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Hashable
import qualified Data.HashMap.Strict  as M
import qualified Data.List            as L
import qualified Data.Text as TX
import           GHC.IO.Handle.Types (Handle)
import           GHC.Generics
import           Text.PrettyPrint (text)
import           Language.Fixpoint.Types.Config (RESTOrdering(..))
import           Language.Fixpoint.Types hiding (simplify)
import           Language.Fixpoint.Smt.Types (SmtM)
import           Language.REST
import           Language.REST.KBO (kbo)
import           Language.REST.OCAlgebra as OC
import           Language.REST.Op
import           Language.REST.SMT (SMTExpr)
import qualified Language.REST.RuntimeTerm as RT

-- | @(e, f)@ asserts that @e@ is a subexpression of @f e@
type SubExpr = (Expr, Expr -> Expr)

data TermOrigin = PLE | RW deriving (Show, Eq)

instance PPrint TermOrigin where
  pprintTidy _ = text . show


data RWTerminationOpts =
    RWTerminationCheckEnabled
  | RWTerminationCheckDisabled

data RewriteArgs = RWArgs
 { isRWValid          :: Expr -> SmtM Bool
 , rwTerminationOpts  :: RWTerminationOpts
 }

-- Monomorphize ordering constraints so we don't litter PLE with type variables
-- Also helps since GHC doesn't support impredicate polymorphism (yet)
data OCType =
    RestOC (SMTExpr Bool)
  | FuelOC Int
  deriving (Eq, Show, Generic, Hashable)

ordConstraints :: RESTOrdering -> (Handle, Handle) -> IO (OCAlgebra OCType RT.RuntimeTerm IO)
ordConstraints RESTRPO      solver = do
  oca <- rpoOCA solver "$R_"
  return (bimapConstraints RestOC asRestOC oca)
  where
    asRestOC (RestOC e) = e
    asRestOC _          = undefined

ordConstraints RESTKBO solver = do
    oca <- kbo solver "$K_"
    return (bimapConstraints RestOC asRestOC oca)
  where
    asRestOC (RestOC e) = e
    asRestOC _          = undefined

ordConstraints RESTLPO      solver = do
  oca <- lpoOCA solver "$L_"
  return (bimapConstraints RestOC asRestOC oca)
  where
    asRestOC (RestOC e) = e
    asRestOC _          = undefined

ordConstraints (RESTFuel m) _      = return (bimapConstraints FuelOC asFuelOC (fuelOC m))
  where
    asFuelOC (FuelOC n) = n
    asFuelOC _          = undefined


-- Note: if you change the domain of this function, you need to change
-- also Language.Fixpoint.Solver.PLE.isExprRewritable
convert :: Expr -> RT.RuntimeTerm
convert (EIte i t e)   = RT.App "$ite" $ map convert [i,t,e]
convert e@EApp{}       | (f, terms) <- splitEAppThroughECst e, EVar fName <- dropECst f
                       = RT.App (Op (symbolText fName)) $ map convert terms
convert (EVar s)       = RT.App (Op (symbolText s)) []
convert (PNot e)       = RT.App "$not" [ convert e ]
convert (PAnd es)      = RT.App "$and" $ map convert es
convert (POr es)       = RT.App "$or" $ map convert es
convert (PAtom s l r)  = RT.App (Op $ "$atom" `TX.append` (TX.pack . show) s) [convert l, convert r]
convert (EBin o l r)   = RT.App (Op $ "$ebin" `TX.append` (TX.pack . show) o) [convert l, convert r]
convert (ECon c)       = RT.App (Op $ "$econ" `TX.append` (TX.pack . show) c) []
convert (ESym (SL tx)) = RT.App (Op tx) []
convert (ECst t _)     = convert t
convert (PIff e0 e1)   = convert (PAtom Eq e0 e1)
convert (PImp e0 e1)   = convert (POr [PNot e0, e1])
convert e              = error (show e)

passesTerminationCheck :: OCAlgebra oc a IO -> RewriteArgs -> oc -> IO Bool
passesTerminationCheck aoc rwArgs c =
  case rwTerminationOpts rwArgs of
    RWTerminationCheckEnabled  -> isSat aoc c
    RWTerminationCheckDisabled -> return True

-- | Yields the result of rewriting an expression with an autorewrite equation.
--
-- Yields nothing if:
--
--  * The result of the rewrite is identical to the original expression
--  * Any of the arguments of the autorewrite has a refinement type which is
--    not satisfied in the current context.
--
getRewrite ::
     OCAlgebra oc Expr IO
  -> RewriteArgs
  -> oc
  -> SubExpr
  -> AutoRewrite
  -> MaybeT SmtM ((Expr, Expr), Expr, oc)
getRewrite aoc rwArgs c (subE, toE) (AutoRewrite args lhs rhs) =
  do
    su <- MaybeT $ return $ unify freeVars lhs subE
    let subE' = subst su rhs
    guard $ subE /= subE'
    let expr' = toE subE'
        eqn = (subst su lhs, subE')
    mapM_ (checkSubst su) exprs
    c' <- case rwTerminationOpts rwArgs of
      RWTerminationCheckEnabled  -> liftIO $ refine aoc c subE subE'
      RWTerminationCheckDisabled -> return c
    return (eqn, expr', c')
  where
    check :: Expr -> MaybeT SmtM ()
    check e = do
      valid <- MaybeT $ Just <$> isRWValid rwArgs e
      guard valid

    freeVars = [s | RR _ (Reft (s, _)) <- args ]
    exprs    = [(s, e) | RR _ (Reft (s, e)) <- args ]

    checkSubst su (s, e) =
      do
        let su' = catSubst su $ mkSubst [("VV", subst su (EVar s))]
        -- liftIO $ printf "Substitute %s in %s\n" (show su') (show e)
        check $ subst (catSubst su su') e


subExprs :: Expr -> [SubExpr]
subExprs e = (e,id):subExprs' e

subExprs' :: Expr -> [SubExpr]
subExprs' (EIte c lhs rhs)  = c''
  where
    c' = subExprs c
    c'' = map (\(e, f) -> (e, \e' -> EIte (f e') lhs rhs)) c'

subExprs' (EBin op lhs rhs) = lhs'' ++ rhs''
  where
    lhs' = subExprs lhs
    rhs' = subExprs rhs
    lhs'' :: [SubExpr]
    lhs'' = map (\(e, f) -> (e, \e' -> EBin op (f e') rhs)) lhs'
    rhs'' :: [SubExpr]
    rhs'' = map (\(e, f) -> (e, \e' -> EBin op lhs (f e'))) rhs'

subExprs' (PImp lhs rhs) = lhs'' ++ rhs''
  where
    lhs' = subExprs lhs
    rhs' = subExprs rhs
    lhs'' :: [SubExpr]
    lhs'' = map (\(e, f) -> (e, \e' -> PImp (f e') rhs)) lhs'
    rhs'' :: [SubExpr]
    rhs'' = map (\(e, f) -> (e, \e' -> PImp lhs (f e'))) rhs'

subExprs' (PIff lhs rhs) = lhs'' ++ rhs''
  where
    lhs' = subExprs lhs
    rhs' = subExprs rhs
    lhs'' :: [SubExpr]
    lhs'' = map (\(e, f) -> (e, \e' -> PIff (f e') rhs)) lhs'
    rhs'' :: [SubExpr]
    rhs'' = map (\(e, f) -> (e, \e' -> PIff lhs (f e'))) rhs'

subExprs' (PAtom op lhs rhs) = lhs'' ++ rhs''
  where
    lhs' = subExprs lhs
    rhs' = subExprs rhs
    lhs'' :: [SubExpr]
    lhs'' = map (\(e, f) -> (e, \e' -> PAtom op (f e') rhs)) lhs'
    rhs'' :: [SubExpr]
    rhs'' = map (\(e, f) -> (e, \e' -> PAtom op lhs (f e'))) rhs'

subExprs' e@EApp{} =
  if f == EVar "Language.Haskell.Liquid.ProofCombinators.===" ||
     f == EVar "Language.Haskell.Liquid.ProofCombinators.==." ||
     f == EVar "Language.Haskell.Liquid.ProofCombinators.?"
  then []
  else concatMap replace indexedArgs
    where
      (f, es)          = splitEApp e
      indexedArgs      = zip [0..] es
      replace (i, arg) = do
        (subArg, toArg) <- subExprs arg
        return (subArg, \subArg' -> eApps f $ take i es ++ toArg subArg' : drop (i+1) es)

subExprs' (ECst e t) =
    [ (e', \subE -> ECst (toE subE) t) | (e', toE) <- subExprs' e ]

subExprs' (PAnd es) = [ (e, PAnd . f) | (e, f) <- subs es ]

subExprs' (POr es) = [ (e, POr . f) | (e, f) <- subs es ]

subExprs' (ELet x e1 e2) = e1'' ++ e2''
  where
    e1' = subExprs e1
    e2' = subExprs e2
    e1'' :: [SubExpr]
    e1'' = map (\(e, f) -> (e, \e' -> ELet x (f e') e2)) e1'
    e2'' :: [SubExpr]
    e2'' = map (\(e, f) -> (e, \e' -> ELet x e1 (f e'))) e2'

subExprs' _ = []

-- | Computes the subexpressions of a list of expressions.
-- Each subexpression comes with a function that rebuilds the
-- context in which the subexpression occurs.
--
-- > and [ es == f e | (e, f) <- subs es ]
--
subs :: [Expr] -> [(Expr, Expr -> [Expr])]
subs [] = []
subs [x] = [ (s, \e -> [f e]) | (s, f) <- subExprs x ]
subs (x:xs) = [ (s, \e -> f e : xs) | (s, f) <- subExprs x ]
              ++
              [ (s, \e -> x : f e) | (s, f) <- subs xs ]


unifyAll :: [Symbol] -> [Expr] -> [Expr] -> Maybe Subst
unifyAll _ []     []               = Just (Su M.empty)
unifyAll freeVars (template:xs) (seen:ys) =
  do
    rs@(Su s1) <- unify freeVars template seen
    let xs' = map (subst rs) xs
    let ys' = map (subst rs) ys
    (Su s2) <- unifyAll (freeVars L.\\ M.keys s1) xs' ys'
    return $ Su (M.union s1 s2)
unifyAll _ _ _ = undefined

-- | @unify vs template e = Just su@ yields a substitution @su@
-- such that subst su template == e
--
-- Moreover, @su@ is constraint to only substitute variables in @vs@.
--
-- Yields @Nothing@ if no substitution exists.
--
unify :: [Symbol] -> Expr -> Expr -> Maybe Subst
unify _ template seenExpr | template == seenExpr = Just (Su M.empty)
unify freeVars template seenExpr = case (dropECst template, seenExpr) of
  -- preserve seen casts if possible
  (EVar rwVar, _) | rwVar `elem` freeVars ->
    return $ Su (M.singleton rwVar seenExpr)
  -- otherwise discard the seen casts
  (template', _) -> case (template', dropECst seenExpr) of
    (EVar lhs, EVar rhs) | removeModName lhs == removeModName rhs ->
                           Just (Su M.empty)
      where
        removeModName ts = go "" (symbolString ts) where
          go buf []         = buf
          go _   ('.':rest) = go [] rest
          go buf (x:xs)     = go (buf ++ [x]) xs
    (EApp templateF templateBody, EApp seenF seenBody) ->
      unifyAll freeVars [templateF, templateBody] [seenF, seenBody]
    (ENeg rw, ENeg seen) ->
      unify freeVars rw seen
    (EBin op rwLeft rwRight, EBin op' seenLeft seenRight) | op == op' ->
      unifyAll freeVars [rwLeft, rwRight] [seenLeft, seenRight]
    (EIte cond rwLeft rwRight, EIte seenCond seenLeft seenRight) ->
      unifyAll freeVars [cond, rwLeft, rwRight] [seenCond, seenLeft, seenRight]
    (ECst rw _, seen) ->
      unify freeVars rw seen
    (ETApp rw _, ETApp seen _) ->
      unify freeVars rw seen
    (ETAbs rw _, ETAbs seen _) ->
      unify freeVars rw seen
    (PAnd rw, PAnd seen ) ->
      unifyAll freeVars rw seen
    (POr rw, POr seen ) ->
      unifyAll freeVars rw seen
    (PNot rw, PNot seen) ->
      unify freeVars rw seen
    (PImp templateF templateBody, PImp seenF seenBody) ->
      unifyAll freeVars [templateF, templateBody] [seenF, seenBody]
    (PIff templateF templateBody, PIff seenF seenBody) ->
      unifyAll freeVars [templateF, templateBody] [seenF, seenBody]
    (PAtom rel templateF templateBody, PAtom rel' seenF seenBody) | rel == rel' ->
      unifyAll freeVars [templateF, templateBody] [seenF, seenBody]
    (PAll _ rw, PAll _ seen) ->
      unify freeVars rw seen
    (PExist _ rw, PExist _ seen) ->
      unify freeVars rw seen
    (ECoerc _ _ rw, ECoerc _ _ seen) ->
      unify freeVars rw seen
    (ELet _ rw1 rw2, ELet _ seen1 seen2) ->
      unifyAll freeVars [rw1, rw2] [seen1, seen2]
    _ -> Nothing
