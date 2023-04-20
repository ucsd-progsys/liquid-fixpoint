{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Fixpoint.CounterExample
  ( hornToProg
  , mainKVar

  , Prog
  , Func (..)
  , Decl
  , Body
  , Statement (..)
  ) where

import Language.Fixpoint.Types

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Control.Monad.State
import Control.Monad.Reader

import Text.PrettyPrint.HughesPJ ((<+>), ($+$))
import qualified Text.PrettyPrint.HughesPJ as PP

-- | A program, containing multiple function definitions
-- mapped by KVars.
data Prog = Prog (HashMap KVar Func)

-- | A function symbol corresponding to a KVar.
data Func = Func [Decl] [Body]
  deriving Show

-- | A declaration of a Symbol with a Sort.
data Decl = Decl Symbol Sort
  deriving Show

-- | A sequence of statements.
data Body = Body [Statement]
  deriving Show

-- | A statement used to introduce/check constraints.
data Statement
  = Let Decl
  -- ^ Introduces a new variable.
  | Assume Expr
  -- ^ Constraints a variable.
  | Assert Expr
  -- ^ Checks whether a predicate follows given prior constraints.
  | Call KVar Subst
  -- ^ Call to function indexed by KVar.
  deriving Show

-- | The monad used to convert a set of horn constraints to
-- the imperative function format.
type MonadProg info m = (MonadState Prog m, MonadReader (SInfo info) m, Fixpoint info, MonadIO m)

-- dbg :: (MonadIO m, PPrint a) => a -> m ()
-- dbg = liftIO . print . pprint

-- | Make an imperative program from horn clauses. This
-- can be used to generate a counter example.
hornToProg :: Fixpoint info => SInfo info -> IO Prog
hornToProg si = execStateT (runReaderT go si) prog
  where
    -- Initial program has empty main
    prog = Prog $ Map.singleton mainKVar (Func [] [])
    -- Add all horn clauses in SInfo to the function map
    go = reader cm >>= mapM_ addHorn

-- | Given a horn clause, generates a body for a function.
--
-- The body is generated from the lhs of the horn clause.
--
-- This body is added to the function given by the kvar
-- on the rhs of the horn clause. If there was no kvar,
-- it is added to the main function.
addHorn :: MonadProg info m => SimpC info -> m ()
addHorn horn = do
  (name, func) <- case crhs horn of
    PKVar kvar sub -> do
      decl <- getSig kvar
      -- TODO: Use sub to add constraints to input types (specifically some equivalences)
      _ <- substToBody sub
      body <- hornLhsToBody horn
      return (kvar, Func decl [body])
    rhs@_ -> do
      Body stmts <- hornLhsToBody horn
      let body = Body $ stmts <> [Assert rhs]
      return (mainKVar, Func [] [body])
  addFunc name func

getSig :: MonadProg info m => KVar -> m [Decl]
getSig kvar = do
  -- Get the well foundedness constraint of the kvar
  wfcs <- reader ws
  let wfc = wfcs Map.! kvar

  -- Get the bind environment and bindings of the wfc
  bindEnv <- reader bs
  let ibinds = elemsIBindEnv . wenv $ wfc

  -- Lookup all Decl from the wfc using the ibinds
  let asDecl (sym, sr, _) = Decl sym (sr_sort sr)
  let decls = map (asDecl . flip lookupBindEnv bindEnv) ibinds

  -- Get the last Decl from the head of the wfc
  let rhs = let (sym, sort, _) = wrft wfc in Decl sym sort

  -- Return the head + bindings as argument map
  return $ rhs:decls

substToBody :: MonadProg info m => Subst -> m Body
substToBody sub = return []

hornLhsToBody :: MonadProg info m => SimpC info -> m Body
hornLhsToBody horn = do
  bindEnv <- reader bs
  let lhs = clhs bindEnv horn
  return $ lhsToBody lhs

-- | Map a complete lhs of a horn clause to a function body
lhsToBody :: [(Symbol, SortedReft)] -> Body
lhsToBody = mconcat . map (uncurry reftToBody)

-- | Map a refinement to a declaration and constraint pair
reftToBody :: Symbol -> SortedReft -> Body
reftToBody sym RR
  { sr_sort = sort
  , sr_reft = Reft (v, e)
  } = Body
  [ Let (Decl sym sort)
  , constraint
  ]
  where
    -- Make constraint with proper substitution
    constraint = case e of
      PKVar kvar (Su hm) -> Call kvar (Su . Map.map mapv $ hm)
      _ -> Assume $ subst (Su . Map.singleton v $ EVar sym) e

    -- Substitution function for the kvar Subst map (the normal
    -- subst we use for the other expressions doesn't work)
    mapv (EVar v') | v' == v = EVar sym
    mapv i@_ = i

-- | Add a function to the function map with name KVar.
-- If an entry already exists, it will merge the function
-- bodies.
addFunc :: MonadProg info m => KVar -> Func -> m ()
addFunc kvar func = do
  let merge (Func _ b) (Func d b') = Func d (b <> b')
  Prog prog <- get
  put . Prog $ Map.insertWith merge kvar func prog

-- | The main function, which any horn clause without a
-- KVar on the rhs will be added to.
mainKVar :: KVar
mainKVar = KV $ symbol "main"

instance PPrint Prog where
  pprintTidy tidy (Prog funcs) = PP.vcat
                               . PP.punctuate (PP.text "\n")
                               . map (uncurry $ ppfunc tidy)
                               . Map.toList
                               $ funcs

instance PPrint Func where
  pprintTidy tidy = ppfunc tidy anon
    where
      anon = KV $ symbol "_"

ppfunc :: Tidy -> KVar -> Func -> PP.Doc
ppfunc tidy name (Func decls bodies) = psig $+$ pbody $+$ PP.rbrace
  where
    psig = fn <+> pname <+> pdecls <+> PP.lbrace
    fn = PP.text "fn"
    pname = pprintTidy tidy name
    pdecls = PP.parens
           . PP.hsep
           . PP.punctuate PP.comma
           . map (pprintTidy tidy)
           $ decls
    pbody = punctuate' (PP.text "||")
          . map (PP.nest 4 . pprintTidy tidy)
          $ bodies

    punctuate' _ [] = mempty
    punctuate' _ (d:[]) = d
    punctuate' p (d:ds) = d $+$ p $+$ punctuate' p ds

instance PPrint Decl where
  pprintTidy tidy (Decl sym sort) = (psym <> PP.colon) <+> psort
    where
      psym = pprintTidy tidy sym
      psort = pprintTidy tidy sort

instance PPrint Body where
  pprintTidy tidy (Body stmts) = PP.vcat
                               . map (pprintTidy tidy)
                               $ stmts

instance PPrint Statement where
  pprintTidy tidy (Let decl) = PP.text "let" <+> pprintTidy tidy decl
  pprintTidy tidy (Assume exprs) = PP.text "assume" <+> pprintTidy tidy exprs
  pprintTidy tidy (Assert exprs) = PP.text "assert" <+> pprintTidy tidy exprs
  pprintTidy tidy (Call kvar sub) = pprintTidy tidy kvar <+> pprintTidy tidy sub

instance Semigroup Body where
  Body b <> Body b' = Body $ b <> b'

instance Monoid Body where
  mempty = Body []
