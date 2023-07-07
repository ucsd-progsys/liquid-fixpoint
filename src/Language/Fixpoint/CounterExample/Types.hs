{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.CounterExample.Types
  ( CounterExample
  , Prog (..)
  , Name
  , Func (..)
  , Body (..)
  , Statement (..)
  , Decl (..)
  , Signature

  , mainName
  ) where

import Language.Fixpoint.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Text.PrettyPrint.HughesPJ ((<+>), ($+$))
import qualified Text.PrettyPrint.HughesPJ as PP

-- | A counter example for a model.
type CounterExample = HashMap [Symbol] Subst

-- | A program, containing multiple function definitions
-- mapped by their name.
newtype Prog = Prog (HashMap Name Func)
  deriving Show

-- | Identifier of a function. All KVars are translated
-- into functions, so it is just an alias.
type Name = KVar

-- | A function symbol corresponding to a Name.
data Func = Func !Signature ![Body]
  deriving Show

-- | Signature of a function.
type Signature = [Decl]

-- | A sequence of statements.
data Body = Body !SubcId ![Statement]
  deriving Show

-- | A statement used to introduce/check constraints,
-- together with its location information
data Statement
  = Let !Decl
  -- ^ Introduces a new variable.
  | Assume !Expr
  -- ^ Constraints a variable.
  | Assert !Expr
  -- ^ Checks whether a predicate follows given prior constraints.
  | Call !Symbol !Name !Subst
  -- ^ Call to function. The symbol is the origin, used to trace
  -- callstacks.
  deriving Show

-- | A declaration of a Symbol with a Sort.
data Decl = Decl !Symbol !Sort
  deriving Show

-- | The main function, which any horn clause without a
-- KVar on the rhs will be added to.
mainName :: Name
mainName = KV "main"

instance PPrint Prog where
  pprintTidy tidy (Prog funcs) = PP.vcat
                               . PP.punctuate "\n"
                               . map (uncurry $ ppfunc tidy)
                               . Map.toList
                               $ funcs

instance PPrint Func where
  pprintTidy tidy = ppfunc tidy anonymous
    where
      anonymous = KV "_"

ppfunc :: Tidy -> Name -> Func -> PP.Doc
ppfunc tidy name (Func sig bodies) = pdecl $+$ pbody $+$ PP.rbrace
  where
    pdecl = "fn" <+> pname <+> psig <+> PP.lbrace
    pname = pprintTidy tidy name
    psig = PP.parens
         . PP.hsep
         . PP.punctuate PP.comma
         . map (pprintTidy tidy)
         $ sig
    pbody = vpunctuate "||"
          . map (PP.nest 4 . pprintTidy tidy)
          $ bodies

    vpunctuate _ [] = mempty
    vpunctuate _ [d] = d
    vpunctuate p (d:ds) = d $+$ p $+$ vpunctuate p ds

instance PPrint Decl where
  pprintTidy tidy (Decl sym sort) = (psym <> PP.colon) <+> psort
    where
      psym = pprintTidy tidy sym
      psort = pprintTidy tidy sort

instance PPrint Body where
  pprintTidy tidy (Body cid stmts) = pcid $+$ pstmts
    where
      pcid = "// constraint id" <+> pprintTidy tidy cid
      pstmts = PP.vcat
             . map (pprintTidy tidy)
             $ stmts

instance PPrint Statement where
  pprintTidy tidy (Let decl) = "let" <+> pprintTidy tidy decl
  pprintTidy tidy (Assume exprs) = "assume" <+> pprintTidy tidy exprs
  pprintTidy tidy (Assert exprs) = "assert" <+> pprintTidy tidy exprs
  pprintTidy tidy (Call origin name sub) = pname <+> pargs <+> porigin
    where
      pname = pprintTidy tidy name
      pargs = pprintTidy tidy sub
      porigin = "// origin" <+> pprintTidy tidy origin

instance Subable Statement where
  syms (Let decl) = syms decl
  syms (Assume e) = syms e
  syms (Assert e) = syms e
  syms (Call _ _ (Su sub)) = syms sub

  substa f (Let decl) = Let $ substa f decl
  substa f (Assume e) = Assume $ substa f e
  substa f (Assert e) = Assert $ substa f e
  substa f (Call origin name (Su sub)) = Call origin name (Su $ substa f sub)

  substf f (Let decl) = Let $ substf f decl
  substf f (Assume e) = Assume $ substf f e
  substf f (Assert e) = Assert $ substf f e
  substf f (Call origin name (Su sub)) = Call origin name (Su $ substf f sub)

  subst sub (Let decl) = Let $ subst sub decl
  subst sub (Assume e) = Assume $ subst sub e
  subst sub (Assert e) = Assert $ subst sub e
  subst sub (Call origin name (Su sub')) = Call origin name (Su $ subst sub sub')

instance Subable Decl where
  syms (Decl sym _) = [sym]

  substa f (Decl sym sort) = Decl (substa f sym) sort

  substf f (Decl sym sort) = Decl (substf f sym) sort

  subst sub (Decl sym sort) = Decl (subst sub sym) sort
