{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE DoAndIfThenElse      #-}

{-# OPTIONS_GHC -Wno-orphans        #-}

-- | This module contains the code for serializing Haskell values
--   into SMTLIB2 format, that is, the instances for the @SMTLIB2@
--   typeclass. We split it into a separate module as it depends on
--   Theories (see @smt2App@).

module Language.Fixpoint.Smt.Serialize (smt2SortMono) where

import           Control.Monad.State
import           Data.ByteString.Builder (Builder)
import           Language.Fixpoint.SortCheck
import           Language.Fixpoint.Types
import qualified Language.Fixpoint.Types.Visitor as Vis
import           Language.Fixpoint.Smt.Types
import qualified Language.Fixpoint.Smt.Theories as Thy

-- import           Data.Text.Format
import           Language.Fixpoint.Misc (sortNub, errorstar)
import           Language.Fixpoint.Utils.Builder as Builder
import qualified Data.Text as T
import Data.Text (Text)
-- import Debug.Trace (trace)

instance SMTLIB2 (Symbol, Sort) where
  smt2 c@(sym, t) =
    -- build "({} {})" (smt2 env sym, smt2SortMono c env t)
    do s <- smt2 sym
       ss <- smt2SortMono c t
       pure $ parenSeqs [s , ss]

instance SMTLIB2 (Symbol, Expr) where
  smt2 (sym, e) =
    do s <- smt2 sym
       ss <- smt2 e
       pure $ parenSeqs [s, ss]

smt2SortMono, smt2SortPoly :: (PPrint a) => a -> Sort -> SymM Builder
smt2SortMono = smt2Sort False
smt2SortPoly = smt2Sort True

smt2Sort :: (PPrint a) => Bool -> a -> Sort -> SymM Builder
smt2Sort poly _ t =
  do env <- get
     smt2 (Thy.sortSmtSort poly (seData env) t)

smt2data :: [DataDecl] -> SymM Builder
smt2data = smt2data' . map padDataDecl

smt2data' :: [DataDecl] -> SymM Builder
smt2data' ds =
  do n <- traverse smt2dataname ds
     d <- traverse smt2datactors ds
     pure $ seqs [ parens $ smt2many n , parens $ smt2many d ]


smt2dataname :: DataDecl -> SymM Builder
smt2dataname (DDecl tc as _) =
  do name <- smt2 (symbol tc)
     n    <- smt2 as
     pure $ parenSeqs [name, n]


smt2datactors :: DataDecl -> SymM Builder
smt2datactors (DDecl _ as cs) =
  do ds <- traverse (smt2ctor as) cs
     if as > 0
      then do tvars <- traverse smt2TV [0..(as-1)]
              pure $ parenSeqs ["par", parens (smt2many tvars), parens (smt2many ds)]
      else pure $                                               parens (smt2many ds)
  where
    smt2TV = smt2 . SVar

smt2ctor :: Int -> DataCtor -> SymM Builder
smt2ctor as (DCtor c fs) =
  do h <- smt2 c
     t <- traverse (smt2field as) fs
     pure $ parenSeqs (h : t)

smt2field :: Int -> DataField -> SymM Builder
smt2field as d@(DField x t) =
  do s <- smt2 x
     ss <- smt2SortPoly d $ mkPoly as t
     pure $ parenSeqs [s , ss]

-- | SMTLIB/Z3 don't like "unused" type variables; they get pruned away and
--   cause wierd hassles. See tests/pos/adt_poly_dead.fq for an example.
--   'padDataDecl' adds a junk constructor that "uses" up all the tyvars just
--   to avoid this pruning problem.

padDataDecl :: DataDecl -> DataDecl
padDataDecl d@(DDecl tc n cs)
  | hasDead    = DDecl tc n (junkDataCtor tc n : cs)
  | otherwise  = d
  where
    hasDead    = length usedVars < n
    usedVars   = declUsedVars d

junkDataCtor :: FTycon -> Int -> DataCtor
junkDataCtor c n = DCtor (atLoc c junkc) [DField (junkFld i) (FVar i) | i <- [0..(n-1)]]
  where
    junkc        = suffixSymbol "junk" (symbol c)
    junkFld i    = atLoc c    (intSymbol junkc i)

declUsedVars :: DataDecl -> [Int]
declUsedVars = sortNub . Vis.foldDataDecl go []
  where
    go is (FVar i) = i : is
    go is _        = is

instance SMTLIB2 Symbol where
  smt2 s = do env <- get
              case Thy.smt2Symbol env s of
                Just t  -> pure t
                Nothing -> pure $ symbolBuilder s
instance SMTLIB2 Int where
  smt2 i = pure $ Builder.fromString $ show i

instance SMTLIB2 LocSymbol where
  smt2 = smt2 . val

instance SMTLIB2 SymConst where
  smt2 c@(SL t) = do
    seStr <- gets seString
    if seStr
      then pure $ quotes $ fromText $ smtEscape t  -- emit "hello" not lit$36$hello
      else smt2 (symbol c)

-- | Per https://smt-lib.org/theories-UnicodeStrings.shtml
-- "SMT-LIB 2.6 has one escape sequence of its own for string literals. Two
--  double quotes ("") are used to represent the double-quote character within
--  a string literal"

smtEscape :: Text -> Text
smtEscape = T.replace "\"" "\"\""

instance SMTLIB2 Constant where
  smt2 (I n)   = pure $ bShow n
  smt2 (R d)   = pure $ bFloat d
  smt2 (L t s)
    | isString s = pure $ quotes $ fromText t
    | otherwise  = pure $ fromText t

instance SMTLIB2 Bop where
  smt2 Plus   = pure "+"
  smt2 Minus  = pure "-"
  smt2 Times  = pure $ symbolBuilder mulFuncName
  smt2 Div    = pure $ symbolBuilder divFuncName
  smt2 RTimes = pure "*"
  smt2 RDiv   = pure "/"
  smt2 Mod    = pure "mod"

instance SMTLIB2 Brel where
  smt2 Eq  = pure "="
  smt2 Ueq = pure "="
  smt2 Gt  = pure ">"
  smt2 Ge  = pure ">="
  smt2 Lt  = pure "<"
  smt2 Le  = pure "<="
  smt2 _   = errorstar "SMTLIB2 Brel"

-- NV TODO: change the way EApp is printed
instance SMTLIB2 Expr where
  smt2 (ESym z)         = smt2 z
  smt2 (ECon c)         = smt2 c
  smt2 (EVar x)         = smt2 x
  smt2 e@(EApp _ _)     = smt2App e
  smt2 (ENeg e)         = do s <- smt2 e
                             pure $ parenSeqs ["-", s]
  smt2 (EBin o e1 e2)   = do so <- smt2 o
                             s1 <- smt2 e1
                             s2 <- smt2 e2
                             pure $ parenSeqs [so, s1, s2]
  smt2 (ELet x e1 e2)   = do s1 <- smt2 (x, e1)
                             s2 <- smt2 e2
                             pure $ parenSeqs ["let", parens s1, s2]
  smt2 (EIte e1 e2 e3)  = do s1 <- smt2 e1
                             s2 <- smt2 e2
                             s3 <- smt2 e3
                             pure $ parenSeqs ["ite", s1, s2, s3]
  smt2 (ECst e t)       = smt2Cast e t
  smt2 PTrue            = pure "true"
  smt2 PFalse           = pure "false"
  smt2 (PAnd [])        = pure "true"
  smt2 (PAnd ps)        = do s <- smt2s ps
                             pure $ parenSeqs ["and", s]
  smt2 (POr [])         = pure "false"
  smt2 (POr ps)         = do s <- smt2s ps
                             pure $ parenSeqs ["or", s]
  smt2 (PNot p)         = do s <- smt2 p
                             pure $ parenSeqs ["not", s]
  smt2 (PImp p q)       = do s1 <- smt2 p
                             s2 <- smt2 q
                             pure $ parenSeqs ["=>", s1, s2]
  smt2 (PIff p q)       = do s1 <- smt2 p
                             s2 <- smt2 q
                             pure $ parenSeqs ["=", s1, s2]
  smt2 (PExist [] p)    = smt2 p
  smt2 (PExist xs p)    = do s <- smt2s xs
                             s1 <- smt2 p
                             pure $ parenSeqs ["exists", parens s, s1]
  smt2 (PAll   [] p)    = smt2 p
  smt2 (PAll   xs p)    = do s <- smt2s xs
                             s1 <- smt2 p
                             pure $ parenSeqs ["forall", parens s, s1]
  smt2 (PAtom r e1 e2)  = mkRel r e1 e2
  smt2 (ELam b e)       = smt2Lam b e
  smt2 (ECoerc t1 t2 e) = smt2Coerc t1 t2 e
  smt2 e                = panic ("smtlib2 Pred  " ++ show e)

-- | smt2Cast uses the 'as x T' pattern needed for polymorphic ADT constructors
--   like Nil, see `tests/pos/adt_list_1.fq`

smt2Cast :: Expr -> Sort -> SymM Builder
smt2Cast (EVar x) t = smt2Var x t
smt2Cast e        _ = smt2    e

smt2Var :: Symbol -> Sort -> SymM Builder
smt2Var x t
  | isLamArgSymbol x = smtLamArg x t
  | otherwise        = do env <- get
                          case symEnvSort x env of
                            Just s | isPolyInst s t -> smt2VarAs x t
                            _                       -> smt2 x

smt2VarAs :: Symbol -> Sort -> SymM Builder
smt2VarAs x t =
  do s <- smt2 x
     s1 <- smt2SortMono x t
     pure $ parenSeqs ["as", s, s1]

-- the next four functions (ones containing a call to `symbolAtName`) can trigger
-- an expansion of the "nursery" tag table ('seApplsCur' in 'SymEnv') when processing
-- a fresh function sort
smtLamArg :: Symbol -> Sort -> SymM Builder
smtLamArg x t =
  do s <- symbolAtName x (FFunc t FInt)
     pure $ Builder.fromText s

smt2Lam :: (Symbol, Sort) -> Expr -> SymM Builder
smt2Lam (x, xT) full@(ECst _ eT) =
  do x' <- smtLamArg x xT
     lambda <- symbolAtName lambdaName (FFunc xT eT)
     f <- smt2 full
     pure $ parenSeqs [Builder.fromText lambda, x', f]
smt2Lam _ e
  = panic ("smtlib2: Cannot serialize unsorted lambda: " ++ showpp e)

smt2App :: Expr -> SymM Builder
smt2App (EApp (EApp f e1) e2)
  | Just t <- unApplyAt f
  = do a <- symbolAtName applyName t
       s <- smt2s [e1, e2]
       pure $ parenSeqs [Builder.fromText a, s]
smt2App e = do s0 <- traverse smt2 es
               s1 <- Thy.smt2App smt2VarAs f s0
               case s1 of
                 Just b -> pure b
                 Nothing -> do s2 <- smt2 f
                               s3 <- smt2s es
                               pure $ parenSeqs [s2, s3]
  where
    (f, es) = splitEApp' e

smt2Coerc :: Sort -> Sort -> Expr -> SymM Builder
smt2Coerc t1 t2 e = do
  env <- get
  let s1 = Thy.sortSmtSort False (seData env) t1
      s2 = Thy.sortSmtSort False (seData env) t2
  if s1 == s2 then smt2 e
  else do
    coerceFn <- symbolAtName coerceName (FFunc t1 t2)
    s <- smt2 e
    pure $ parenSeqs [Builder.fromText coerceFn , s]

splitEApp' :: Expr -> (Expr, [Expr])
splitEApp'            = go []
  where
    go acc (EApp f e) = go (e:acc) f
  --   go acc (ECst e _) = go acc e
    go acc e          = (e, acc)

mkRel :: Brel -> Expr -> Expr -> SymM Builder
mkRel Ne  e1 e2 = mkNe e1 e2
mkRel Une e1 e2 = mkNe e1 e2
mkRel r   e1 e2 = do s <- smt2 r
                     s1 <- smt2 e1
                     s2 <- smt2 e2
                     pure $ parenSeqs [s, s1, s2]

mkNe :: Expr -> Expr -> SymM Builder
mkNe e1 e2 = do s1 <- smt2 e1
                s2 <- smt2 e2
                pure $ key "not" (parenSeqs ["=", s1, s2])
instance SMTLIB2 Command where
  smt2     (DeclData ds)       = do s <- smt2data ds
                                    pure $ key "declare-datatypes" s
  smt2     (Declare x ts t)    = do s <- smt2s ts
                                    s1 <- smt2 t
                                    pure $ parenSeqs ["declare-fun", Builder.fromText x, parens s, s1]
  smt2     c@(Define t)        = do s <- smt2SortMono c t
                                    pure $ key "declare-sort" s
  smt2     (DefineFunc name paramxs rsort e) =
    do n <- smt2 name
       bParams <- traverse (\(s, t) -> do s0 <- smt2 s
                                          s1 <- smt2 t
                                          pure $ parenSeqs [s0 , s1]) paramxs
       r <- smt2 rsort
       e' <- smt2 e
       pure $ parenSeqs ["define-fun", n, parenSeqs bParams, r, e']

  smt2     (Assert Nothing p)  = {-# SCC "smt2-assert" #-}
                                  do s <- smt2 p
                                     pure $ key "assert" s
  smt2     (Assert (Just i) p) = {-# SCC "smt2-assert" #-}
                                  do s <- smt2 p
                                     pure $ key "assert" (parens ("!"<+> s <+> ":named p-" <> bShow i))
  smt2     (Distinct az)
    | length az < 2            = pure ""
    | otherwise                = do s <- smt2s az
                                    pure $ key "assert" $ key "distinct" s
  smt2     (AssertAx t)        = do s <- smt2 t
                                    pure $ key "assert" s
  smt2     Push                = pure "(push 1)"
  smt2     Pop                 = pure "(pop 1)"
  smt2     CheckSat            = pure "(check-sat)"
  smt2     (GetValue xs)       = do s <- smt2s xs
                                    pure $ key "key-value" (parens s)
  smt2     (CMany cmds)        = smt2s cmds
  smt2     Exit                = pure "(exit)"
  smt2     SetMbqi             = pure "(set-option :smt.mbqi true)"
  smt2     (Comment t)         = pure $ fromText ("; " <> t <> "\n")

instance SMTLIB2 (Triggered Expr) where
  smt2 (TR NoTrigger e)       = smt2 e
  smt2 (TR _ (PExist [] p))   = smt2 p
  smt2 t@(TR _ (PExist xs p)) = smtTr "exists" xs p t
  smt2 (TR _ (PAll   [] p))   = smt2 p
  smt2 t@(TR _ (PAll   xs p)) = smtTr "forall" xs p t
  smt2 (TR _ e)               = smt2 e

{-# INLINE smtTr #-}
smtTr :: Builder -> [(Symbol, Sort)] -> Expr -> Triggered Expr -> SymM Builder
smtTr q xs p t =
  do s <- smt2s xs
     s1 <- smt2 p
     s2 <- smt2s (makeTriggers t)
     pure $ key q (parens s <+> key "!" (s1 <+> ":pattern" <> parens s2))

{-# INLINE smt2s #-}
smt2s :: SMTLIB2 a => [a] -> SymM Builder
smt2s as = smt2many <$> traverse smt2 as

{-# INLINE smt2many #-}
smt2many :: [Builder] -> Builder
smt2many = seqs
