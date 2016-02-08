{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternGuards        #-}

-- | This module contains the code for serializing Haskell values
--   into SMTLIB2 format, that is, the instances for the @SMTLIB2@
--   typeclass. We split it into a separate module as it depends on
--   Theories (see @smt2App@).

module Language.Fixpoint.Smt.Serialize () where

import           Language.Fixpoint.Types
import           Language.Fixpoint.Smt.Types
import qualified Language.Fixpoint.Smt.Theories as Thy
import qualified Data.Text                      as T
import           Data.Text.Format               hiding (format)
import           Data.Maybe (fromMaybe)
import           Language.Fixpoint.Misc (errorstar)

{-
    (* (L t1 t2 t3) is now encoded as
        ---> (((L @ t1) @ t2) @ t3)
        ---> App(@, [App(@, [App(@, [L[]; t1]); t2]); t3])
        The following decodes the above as
     *)
    let rec app_args_of_t acc = function
      | App (c, [t1; t2]) when c = tc_app -> app_args_of_t (t2 :: acc) t1
      | App (c, [])                       -> (c, acc)
      | t                                 -> (tc_app, t :: acc)

      (*
      | Ptr (Loc s)                       -> (tycon s, acc)
      | t                                 -> assertf "app_args_of_t: unexpected t1 = %s" (to_string t)
      *)

    let app_of_t = function
      | App (c, _) as t when c = tc_app   -> Some (app_args_of_t [] t)
      | App (c, ts)                       -> Some (c, ts)
      | _                                 -> None

-}
instance SMTLIB2 Sort where
  smt2 _ s@(FFunc _ _)         = error $ "smt2 FFunc: " ++ show s
  smt2 _ FInt                  = "Int"
  smt2 _ FReal                 = "Real"
  smt2 _ t
    | t == boolSort            = "Bool"
  smt2 _ t
    | Just d <- Thy.smt2Sort t = d
  smt2 _ _                     = "Int"


instance SMTLIB2 Symbol where
  smt2 _ s
    | Just t <- Thy.smt2Symbol s = t
  smt2 _ s                       = symbolSafeText  s

instance SMTLIB2 (Symbol, Sort) where
  smt2 ctx (sym, t) = format "({} {})"  (smt2 ctx sym, smt2 ctx t)

instance SMTLIB2 SymConst where
  smt2 ctx = smt2 ctx . symbol

instance SMTLIB2 Constant where
  smt2 _ (I n)   = format "{}" (Only n)
  smt2 _ (R d)   = format "{}" (Only d)
  smt2 _ (L t _) = format "{}" (Only t) -- errorstar $ "Horrors, how to translate: " ++ show c

instance SMTLIB2 LocSymbol where
  smt2 ctx = smt2 ctx . val

instance SMTLIB2 Bop where
  smt2 _ Plus  = "+"
  smt2 _ Minus = "-"
  smt2 _ Times = "*"
  smt2 _ Div   = "/"
  smt2 _ Mod   = "mod"

instance SMTLIB2 Brel where
  smt2 _ Eq    = "="
  smt2 _ Ueq   = "="
  smt2 _ Gt    = ">"
  smt2 _ Ge    = ">="
  smt2 _ Lt    = "<"
  smt2 _ Le    = "<="
  smt2 _ _     = error "SMTLIB2 Brel"

instance SMTLIB2 Expr where
  smt2 ctx (ESym z)         = smt2 ctx (symbol z)
  smt2 ctx (ECon c)         = smt2 ctx c
  smt2 ctx (EVar x)         = smt2 ctx x
  smt2 ctx (EApp f es)      = smt2App ctx f es
  smt2 ctx (ENeg e)         = format "(- {})"         (Only $ smt2 ctx e)
  smt2 ctx (EBin o e1 e2)   = smt2Bop ctx o e1 e2
  smt2 ctx (EIte e1 e2 e3)  = format "(ite {} {} {})" (smt2 ctx e1, smt2 ctx e2, smt2 ctx e3)
  smt2 ctx (ECst e _)       = smt2 ctx e
  smt2 ctx e                = error  $ "TODO: SMTLIB2 Expr: " ++ show e

smt2Bop ctx o e1 e2
  | ctx && (o == Times || o == Div)
  = smt2App ctx (uOp o) [e1, e2]
  | otherwise
  = format "({} {} {})"     (smt2 ctx o, smt2 ctx e1, smt2 ctx e2)
  where
    uOp o | o == Times = dummyLoc "Z3_OP_MUL"
          | o == Div   = dummyLoc "Z3_OP_DIV"



smt2App :: Bool -> LocSymbol -> [Expr] -> T.Text

smt2App ctx f es = fromMaybe (smt2App' ctx f ds) (Thy.smt2App f ds)
  where
   ds        = smt2 ctx <$> es

smt2App' ctx f [] = smt2 ctx f
smt2App' ctx f ds = format "({} {})" (smt2 ctx f, smt2many ds)

instance SMTLIB2 Pred where
  smt2 ctx (PTrue)          = "true"
  smt2 ctx (PFalse)         = "false"
  smt2 ctx (PAnd [])        = "true"
  smt2 ctx (PAnd ps)        = format "(and {})"    (Only $ smt2s ctx ps)
  smt2 ctx (POr [])         = "false"
  smt2 ctx (POr ps)         = format "(or  {})"    (Only $ smt2s ctx ps)
  smt2 ctx (PNot p)         = format "(not {})"    (Only $ smt2 ctx p)
  smt2 ctx (PImp p q)       = format "(=> {} {})"  (smt2 ctx p, smt2 ctx q)
  smt2 ctx (PIff p q)       = format "(=  {} {})"  (smt2 ctx p, smt2 ctx q)
  smt2 ctx (PExist bs p)    = format "(exists ({}) {})"  (smt2s ctx bs, smt2 ctx p)
  smt2 ctx (PBexp e)        = smt2 ctx e
  smt2 ctx (PAtom r e1 e2)  = mkRel ctx r e1 e2
  smt2 ctx _                = error "smtlib2 Pred"

mkRel ctx Ne  e1 e2         = mkNe ctx e1 e2
mkRel ctx Une e1 e2         = mkNe ctx e1 e2
mkRel ctx r   e1 e2         = format "({} {} {})"      (smt2 ctx r, smt2 ctx e1, smt2 ctx e2)
mkNe  ctx e1 e2             = format "(not (= {} {}))" (smt2 ctx e1, smt2 ctx e2)

instance SMTLIB2 Command where
  smt2 ctx (Declare x ts t)    = format "(declare-fun {} ({}) {})"  (smt2 ctx x, smt2s ctx ts, smt2 ctx t)
  smt2 ctx (Define t)          = format "(declare-sort {})"         (Only $ smt2 ctx t)
  smt2 ctx (Assert Nothing p)  = format "(assert {})"               (Only $ smt2 ctx p)
  smt2 ctx (Assert (Just i) p) = format "(assert (! {} :named p-{}))"  (smt2 ctx p, i)
  smt2 ctx (Distinct az)       = format "(assert (distinct {}))"    (Only $ smt2s ctx az)
  smt2 ctx (Push)              = "(push 1)"
  smt2 ctx (Pop)               = "(pop 1)"
  smt2 ctx (CheckSat)          = "(check-sat)"
  smt2 ctx (GetValue xs)       = T.unwords $ ["(get-value ("] ++ fmap (smt2 ctx) xs ++ ["))"]

smt2s     :: SMTLIB2 a => Bool -> [a] -> T.Text
smt2s ctx = smt2many . fmap (smt2 ctx)

smt2many :: [T.Text] -> T.Text
smt2many = T.intercalate " "

{-
(declare-fun x () Int)
(declare-fun y () Int)
(assert (<= 0 x))
(assert (< x y))
(push 1)
(assert (not (<= 0 y)))
(check-sat)
(pop 1)
(push 1)
(assert (<= 0 y))
(check-sat)
(pop 1)
-}
