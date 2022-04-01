{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -Wno-name-shadowing    #-}

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

import           Control.Monad.State (guard)
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
import           Language.REST
import           Language.REST.KBO (kbo)
import           Language.REST.LPO (lpo)
import           Language.REST.OCAlgebra as OC
import           Language.REST.OCToAbstract (lift)
import           Language.REST.Op
import           Language.REST.SMT (SMTExpr)
import           Language.REST.WQOConstraints.ADT (ConstraintsADT, adtOC)
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
 { isRWValid          :: Expr -> IO Bool
 , rwTerminationOpts  :: RWTerminationOpts
 }

-- Monomorphize ordering constraints so we don't litter PLE with type variables
-- Also helps since GHC doesn't support impredicate polymorphism (yet)
data OCType =
    RPO (ConstraintsADT Op)
  | LPO (ConstraintsADT Op)
  | KBO (SMTExpr Bool)
  | Fuel Int
  deriving (Eq, Show, Generic, Hashable)

ordConstraints :: RESTOrdering -> (Handle, Handle) -> OCAlgebra OCType RT.RuntimeTerm IO
ordConstraints RESTRPO      solver = bimapConstraints RPO asRPO (adtRPO solver)
  where
    asRPO (RPO t) = t
    asRPO _       = undefined

ordConstraints RESTKBO      solver = bimapConstraints KBO asKBO (kbo solver)
  where
    asKBO (KBO t) = t
    asKBO _       = undefined

ordConstraints RESTLPO      solver = bimapConstraints LPO asLPO (lift (adtOC solver) lpo)
  where
    asLPO (LPO t) = t
    asLPO _       = undefined

ordConstraints (RESTFuel n) _      = bimapConstraints Fuel asFuel $ fuelOC n
  where
    asFuel (Fuel n) = n
    asFuel _        = undefined


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
  -> MaybeT IO ((Expr, Expr), Expr, oc)
getRewrite aoc rwArgs c (subE, toE) (AutoRewrite args lhs rhs) =
  do
    su <- MaybeT $ return $ unify freeVars lhs subE
    let subE' = subst su rhs
    guard $ subE /= subE'
    let expr' = toE subE'
        eqn = (subst su lhs, subE')
    mapM_ (checkSubst su) exprs
    return $ case rwTerminationOpts rwArgs of
      RWTerminationCheckEnabled ->
        let
          c' = refine aoc c subE subE'
        in
          (eqn, expr', c')
      RWTerminationCheckDisabled -> (eqn, expr', c)
  where
    check :: Expr -> MaybeT IO ()
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

subExprs' _ = []

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
    (PGrad _ _ _ rw, PGrad _ _ _ seen) ->
      unify freeVars rw seen
    (ECoerc _ _ rw, ECoerc _ _ seen) ->
      unify freeVars rw seen
    _ -> Nothing
