{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.Counterexample.Types
  ( Counterexample (..)
  , SMTCounterexample
  , FullCounterexample
  , CexEnv
  , Trace
  , Scope (..)

  , Prog (..)
  , Name
  , Func (..)
  , Body (..)
  , Statement (..)
  , Decl (..)
  , Signature

  , mainName
  , dbg -- TODO: Remove this on code clean-up!
  ) where

import Language.Fixpoint.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.HashSet (HashSet)
import Data.Bifunctor (second)

import Text.PrettyPrint.HughesPJ ((<+>), ($+$))
import qualified Text.PrettyPrint.HughesPJ as PP

import Control.Monad.IO.Class

dbg :: (MonadIO m, PPrint a) => a -> m ()
dbg = liftIO . print . pprint

-- | A counterexample that was mapped from an SMT result. It is a simple mapping
-- from symbol to concrete instance.
type SMTCounterexample = Counterexample Subst

-- | A scope contains the current binders in place as well as the path traversed
-- to reach this scope.
data Scope = Scope
  { path :: !Trace
  -- ^ The path traversed to reach the scope.
  , constraint :: !SubcId
  -- ^ The current constraint, which dictates the binders.
  , binders :: !Subst
  -- ^ The binders available in the current scope.
  , visited :: HashSet Name
  -- ^ The functions that have already been visited. This is to track whether we
  -- want to expand a function or if we have already hit the recursion limit.
  }
  deriving (Eq, Ord, Show)

-- | A stack trace is built from multiple frame ids. This is mostly used for
-- SMT encodings. These traces are made into a tree like representation in the
-- actual counterexample object.
type Trace = [BindId]

-- | A program, containing multiple function definitions mapped by their name.
data Prog = Prog
  { functions :: (HashMap Name Func)
  -- ^ The functions of this program.
  , definitions :: [Expr]
  -- ^ Constraints that need to hold originating from define statements.
  }
  deriving Show

-- | Identifier of a function. All KVars are translated into functions, so it is
-- just an alias.
type Name = KVar

-- | A function symbol corresponding to a Name.
data Func = Func !Signature ![Body]
  deriving Show

-- | Signature of a function.
type Signature = [Decl]

-- | A sequence of statements.
data Body = Body !SubcId ![Statement]
  deriving Show

-- | A statement used to introduce/check constraints, together with its location
-- information.
--
-- WARNING: We rely on the order of these declarations for the derive of Ord.
-- Specifically, to sort the statements in this same order for the builder.
-- Let bindings should appear first so everything is in scope and correctly
-- named. Calls should be last: this allows us to prune branches when executing
-- them. Do not change these unless you know what you are doing!
data Statement
  = Let !Decl
  -- ^ Introduces a new variable.
  | Assume !Expr
  -- ^ Constraints a variable.
  | Assert !Expr
  -- ^ Checks whether a predicate follows given prior constraints.
  | Call !BindId ![(Name, Subst)]
  -- ^ Call to function. The bind id is used to trace callstacks. I.e. it is the
  -- caller of the function.
  deriving (Show, Eq, Ord)

-- | A declaration of a Symbol with a Sort.
data Decl = Decl !Symbol !Sort
  deriving (Show, Eq, Ord)

-- | The main function, which any horn clause without a KVar on the rhs will be
-- added to.
mainName :: Name
mainName = KV "main"

instance PPrint Prog where
  pprintTidy tidy prog = pcstr $+$ pfuncs
    where
      pcstr = PP.vcat
            . PP.punctuate "\n"
            . map (pprintTidy tidy)
            . definitions
            $ prog

      pfuncs = PP.vcat
             . PP.punctuate "\n"
             . map (uncurry $ ppfunc tidy)
             . Map.toList
             . functions
             $ prog


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
  pprintTidy tidy (Call bid calls) = "call" <+> porigin $+$ pcalls
    where
      porigin = "// bind id" <+> pprintTidy tidy bid
      pcalls = PP.vcat $ PP.nest 4 . pcall <$> calls
      pcall (name, sub) = pprintTidy tidy name <+> pprintTidy tidy sub

instance Subable Statement where
  syms (Let decl) = syms decl
  syms (Assume e) = syms e
  syms (Assert e) = syms e
  syms (Call _ calls) = mconcat $ syms . (\(Su sub) -> sub) . snd <$> calls

  substa _ (Let decl) = Let decl
  substa f (Assume e) = Assume $ substa f e
  substa f (Assert e) = Assert $ substa f e
  substa f (Call bid calls) = Call bid $ mapsub (substa f) <$> calls

  substf _ (Let decl) = Let decl
  substf f (Assume e) = Assume $ substf f e
  substf f (Assert e) = Assert $ substf f e
  substf f (Call bid calls) = Call bid (mapsub (substf f) <$> calls)

  subst _ (Let decl) = Let decl
  subst sub (Assume e) = Assume $ subst sub e
  subst sub (Assert e) = Assert $ subst sub e
  subst sub (Call bid calls) = Call bid (mapsub (subst sub) <$> calls)

mapsub :: (HashMap Symbol Expr -> HashMap Symbol Expr) -> (a, Subst) -> (a, Subst)
mapsub f = second (\(Su sub) -> Su $ f sub)

instance Subable Decl where
  syms (Decl sym _) = [sym]

  substa f (Decl sym sort) = Decl (substa f sym) sort

  substf f (Decl sym sort) = Decl (substf f sym) sort

  subst sub (Decl sym sort) = Decl (subst sub sym) sort
