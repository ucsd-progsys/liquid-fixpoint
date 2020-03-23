{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Fixpoint.Congruence.Solver (solveEUF, resSimpleStatus, _testEuf, _testProps) where

import           Language.Fixpoint.Horn.Types
import           Language.Fixpoint.Horn.Transformations (collectEqualities)
import qualified Language.Fixpoint.Types        as F

import qualified Data.Map as M
import           Language.Fixpoint.Congruence.Types
import           Data.Interned
import           Data.Bifunctor
import           Control.Monad.State
-- import           Debug.Trace (traceShowId)

----------------------------------------------------------------------------------
--- Solvers
----------------------------------------------------------------------------------
data SimpleStatus = Safe | Unsafe | Unknown
                  deriving (Show, Eq)

instance Semigroup SimpleStatus where
    (<>) _ Unknown = Unknown
    (<>) Unknown _ = Unknown
    (<>) Unsafe _ = Unsafe
    (<>) _ Unsafe = Unsafe
    (<>) Safe Safe = Safe

instance Monoid SimpleStatus where
    mempty = Safe

resSimpleStatus :: SimpleStatus -> F.Result a
resSimpleStatus Safe = F.safe
resSimpleStatus Unsafe = F.unsafe
resSimpleStatus Unknown = F.unsafe
----------------------------------------------------------------------------------
-- Proposiational Logic
----------------------------------------------------------------------------------

-- Tests
_test1 :: Cstr ()
_test1 = All (Bind "a" F.boolSort (Reft (F.EVar "a")))
         (All (Bind "b" F.boolSort mempty)
          (Head (Reft (F.EVar "a")) ()))

_test2 :: Cstr ()
_test2 = All (Bind "a" F.boolSort (Reft (F.EVar "a")))
         (All (Bind "b" F.boolSort mempty)
          (Head (Reft (F.EVar "b")) ()))

_testProps :: Bool
_testProps = solveSimple mempty _test1 == Safe && solveSimple mempty _test2 == Unsafe
----------------------------------------------------------------------------------

type Ctx = M.Map F.Symbol ()

solveSimple :: Ctx -> Cstr () -> SimpleStatus
solveSimple ctx (CAnd cs) = mconcat $ solveSimple ctx <$> cs
solveSimple ctx (All (Bind _ _ (Reft (F.EVar v))) c) = solveSimple (M.insert v () ctx) c
solveSimple ctx (All (Bind _ _ (Reft _)) c) = solveSimple ctx c
solveSimple ctx (All (Bind _ _ (PAnd ps)) c) =
    solveSimple (foldr (flip M.insert () . (\(Reft (F.EVar v)) -> v)) ctx ps)  c
solveSimple ctx (Head (Reft (F.EVar v)) _) = if M.lookup v ctx == Just () then Safe else Unsafe
solveSimple _ c = error (show c)

----------------------------------------------------------------------------------
-- EUF
----------------------------------------------------------------------------------

-- Tests
_testeq1 :: Cstr ()
_testeq1 = All (Bind "a" F.boolSort mempty)
           (All (Bind "b" F.boolSort ab)
             (All (Bind "c" F.boolSort bc)
                (Head (Reft (F.PAtom F.Eq (F.EVar "c") (F.EVar "b"))) ())))
  where
  ab = Reft (F.PAtom F.Eq (F.EVar "a") (F.EVar "b"))
  bc = Reft (F.PAtom F.Eq (F.EVar "c") (F.EVar "b"))

_testeq2 :: Cstr ()
_testeq2 = All (Bind "x" F.boolSort mempty)
           (All (Bind "y" F.boolSort $ Reft $ x === y)
                (Head (Reft $ f x === f y) ()))
  where
  a === b = F.PAtom F.Eq a b
  x = F.EVar "x"
  y = F.EVar "y"
  f x = F.EApp (F.EVar "f") x

_testeq3 :: Cstr ()
_testeq3 = All (Bind "x" F.intSort (Reft $ f(f(f x)) === x))
           (All (Bind "a" F.intSort (Reft $ f(f(f(f(f x)))) === x ))
            (Head (Reft $ f x === x) ()))
  where
  a === b = F.PAtom F.Eq a b
  x = F.EVar "x"
  f x = F.EApp (F.EVar "f") x

-- https://cseweb.ucsd.edu/classes/sp08/cse291e/lectures/lecture-8.pdf
_testeq4 :: Cstr ()
_testeq4 = All (Bind "a" F.intSort mempty)
           (All (Bind "b" F.intSort (Reft $ f a b === a))
            (Head (Reft $ f (f a b) b === a) ()))
  where
  a === b = F.PAtom F.Eq a b
  a = F.EVar "a"
  b = F.EVar "b"
  f x y = F.EApp (F.EApp (F.EVar "f") x) y


_testEuf:: Bool
_testEuf =
     solveEUF _testeq4 == Safe
  && solveEUF _testeq3 == Safe
  && solveEUF _testeq2 == Safe
  && solveEUF _testeq1 == Safe

-- Personae Dramatis
--------------------------------------------------------------------------------------
type CtxEUF = (M.Map Term Term, M.Map Term [Term]) -- (UF, term DAG)
data Equal = Equ F.Expr F.Expr

lookupRepr :: Term -> CtxEUF -> Maybe Term
lookupRepr a = M.lookup a . fst
lookupDAG :: Term -> CtxEUF -> [Term]
lookupDAG a = concat . M.lookup a . snd
insertRepr, insertDAG:: Term -> Term -> CtxEUF -> CtxEUF
insertRepr a b = first (M.insert a b)
insertDAG a b = second (M.insertWith (++) a [b])

_showUf :: CtxEUF -> String
_showUf (repr,par) = sm repr ++ sm' par
  where ui = unintern
        sm uf = "fromList " ++ show (first ui . second ui <$> M.assocs uf)
        sm' uf = " fromList " ++ show (first ui . second (map ui) <$> M.assocs uf)

-- Unionfinds
--------------------------------------------------------------------------------------
reprRank :: CtxEUF -> Term -> (Int,Term)
-- We can add path compression later, but makes sure to compare
-- "generalized" ranks so that repr is always ground.
reprRank uf a = case lookupRepr a uf of
              Just b -> if b == a then (0,a) else first (+1) $ reprRank uf b
              Nothing -> (0,a)

find :: CtxEUF -> Term -> Term
find uf = snd . reprRank uf

-- traceuf :: CtxEUF -> CtxEUF
-- traceuf uf = trace (_showUf uf) uf

union :: CtxEUF -> (Term, Term) -> CtxEUF
union env (a, b) = if ranka < rankb
                    then merge repa repb env
                    else merge repb repa env
  where
  (ranka,repa) = reprRank env a
  (rankb,repb) = reprRank env b
  merge a b env = if find env a == find env b
      then env
      else let env' = insertRepr a b $ mergeParents a b env in
           foldl union env' [(u,v) | u@(App _ fu eu) <- lookupDAG a env
                                   , v@(App _ fv ev) <- lookupDAG b env
                                   , find env' fu == find env' fv
                                   , find env' eu == find env' ev ]
  mergeParents :: Term -> Term -> CtxEUF -> CtxEUF
  mergeParents a b = second (M.insertWith (++) b $ lookupDAG a env)

-- with Expresssions and Equaltiies
--------------------------------------------------------------------------------------
-- refactor... we need the FULL term DAG BEFORE we run CC, so no point in
-- adding it again
internExpr :: F.Expr -> State CtxEUF Term
internExpr (F.EVar f) = pure $ var f
internExpr (F.EApp f e) = do
  iF <- internExpr f
  iE <- internExpr e
  let term = app iF iE
  modify $ insertDAG iE term
  modify $ insertDAG iF term
  pure term
internExpr _ = error "TODO"

uftest :: CtxEUF -> Equal -> Bool
uftest uf (Equ a b) = repr uf a == repr uf b
  where
  repr :: CtxEUF -> F.Expr -> Term
  repr uf e = find uf $ evalState (internExpr e) uf

ufinsert :: CtxEUF -> Equal -> CtxEUF
ufinsert uf (Equ a b) = union uf' (iA, iB)
  where ((iA, iB),uf') = runState ((,) <$> internExpr a <*> internExpr b) uf

--------------------------------------------------------------------------------------
solveEUF :: Cstr a -> SimpleStatus
--------------------------------------------------------------------------------------
solveEUF c = solveEq ctx0 c
  where
  ctx0 = execState (mapM_ internExpr $ (\(Equ a b) -> [a,b]) =<< collect c) mempty

  collect (CAnd cs)
    = mconcat $ collect <$> cs
  collect (Any (Bind _ _ p) c)
    = collectEqs p <> collect c
  collect (All (Bind _ _ p) c)
    = collectEqs p <> collect c
  collect (Head p _)
    = collectEqs p

  collectEqs :: Pred -> [Equal]
  collectEqs p = (\(x,term) -> Equ (F.EVar x) term) <$> collectEqualities p

  solveEq :: CtxEUF -> Cstr a -> SimpleStatus
  solveEq ctx (CAnd cs)
    = mconcat $ solveEq ctx <$> cs
  solveEq ctx (All (Bind _ _ p) c)
    = solveEq (foldl ufinsert ctx (collectEqs p)) c
  solveEq ctx (Head p _)
      -- UH-OH! here's the problem; we need to throw Unknown if
      -- collectEqualities doesn't give us the whole term
    = if isEUF p && and (uftest ctx <$> collectEqs p) then Safe else Unsafe
  solveEq _ c
    = error (show c)

isEUF :: Pred -> Bool
isEUF = goP -- . traceShowId
  where
    goP (Reft e) = goE e
    goP (PAnd ps) = and $ goP <$> ps
    goP _ = False

    goE (F.PAtom F.Eq _ _) = True
    goE (F.PAnd es) = and $ goE <$> es
    goE _ = False

