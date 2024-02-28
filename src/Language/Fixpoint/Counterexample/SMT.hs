{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.Counterexample.SMT
  ( SMTContext (..)
  , smtDeclare
  , smtAssert
  , smtAssume
  , smtScope
  , smtCheck
  , smtModel
  ) where

import Language.Fixpoint.Types
import Language.Fixpoint.Counterexample.Types
import qualified Language.Fixpoint.Smt.Interface as SMT

import Control.Monad.Reader

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as T
import Data.String (IsString(..))
import Data.Bifunctor (first)
import Data.Char (chr)
import Data.List (intercalate, foldl')

class SMTContext a where
  smtContext :: a -> SMT.Context

type MonadSMT r m = (SMTContext r, MonadReader r m, MonadIO m)

-- | Declare a new symbol, returning an updated substitution
-- given with this new symbol in it. The substitution map is
-- required to avoid duplicating variable names.
smtDeclare :: MonadSMT s m => Scope -> Decl -> m Scope
smtDeclare scope@Scope { binders = Su binds } (Decl sym _)
  | Map.member sym binds = return scope
smtDeclare scope (Decl sym sort) = do
  ctx <- reader smtContext
  let sym' = scopeSym scope sym
  liftIO $ SMT.smtDecl ctx sym' sort
  let Su sub = binders scope
  let binders' = Su $ Map.insert sym (EVar sym') sub
  return scope { binders = binders' }

-- | Assume the given expression.
smtAssert :: MonadSMT s m => Expr -> m ()
smtAssert = smtAssume . PNot

-- | Assert the given expression.
smtAssume :: MonadSMT s m => Expr -> m ()
smtAssume e = do
  ctx <- reader smtContext
  liftIO $ SMT.smtAssert ctx e

-- | Run the checker within a scope (i.e. a push/pop pair).
smtScope :: MonadSMT s m => m a -> m a
smtScope inner = do
  ctx <- reader smtContext
  liftIO $ SMT.smtPush ctx
  !result <- inner
  liftIO $ SMT.smtPop ctx
  return result

-- | Check if there is a counterexample, returing one
-- if it is available.
smtCheck :: MonadSMT s m => Runner m
smtCheck = do
  ctx <- reader smtContext
  valid <- liftIO $ SMT.smtCheckUnsat ctx

  if valid then return Nothing else Just <$> smtModel

-- | Returns a model, with as precondition that the SMT 
-- solver had a satisfying assignment prior to this.
smtModel :: MonadSMT s m => m SMTCounterexample
smtModel = do
  ctx <- reader smtContext
  Su sub <- liftIO $ SMT.smtGetModel ctx

  -- Filter just the variables for which we have a trace
  let renames = first unscopeSym <$> Map.toList sub
  let traces = [ (trace, sym, e) | (Just (sym, trace), e) <- renames ]

  -- Insert per singleton. Entries are monoidically merged when inserted.
  let insert (trace, sym, e) = cexInsert trace $ Su (Map.singleton sym e)

  -- Insert all elements
  let cex = foldl' (flip insert) mempty traces
  return cex

-- | Returns a version of a symbol with the scope encoded into its name.
scopeSym :: Scope -> Symbol -> Symbol
scopeSym scope sym = symbol name
  where
    name = intercalate bindSep strs
    strs = symbolString <$> progPrefix : sym : paths
    paths = uncurry joinCall <$> path scope
    joinCall caller callee = symbol . mconcat $ [show caller, callSep, show callee]

-- | We encode the trace of a symbol in its name. This way,
-- we do not lose it in the SMT solver. This function translates
-- the encoding back.
unscopeSym :: Symbol -> Maybe (Symbol, [FrameId])
unscopeSym sym = case T.splitOn bindSep sym' of
  (prefix:name:trace) | prefix == progPrefix 
    -> Just (symbol name, toFrameId <$> trace)
  _ -> Nothing
  where
    toFrameId = split . T.splitOn callSep

    split [caller, callee] = (read' caller, read' callee)
    split _ = error "Scoped name was not correctly shaped"

    read' :: Read a => T.Text -> a
    read' = read . T.unpack

    sym' = escapeSmt . symbolText $ sym

escapeSmt :: T.Text -> T.Text
escapeSmt = go False . T.split (=='$')
  where
    go _ [] = ""
    go escape (t:ts) = txt t <> go (not escape) ts
      where
        txt | escape    = T.singleton . chr . read . T.unpack
            | otherwise = id

-- | The separator used to encode the stack trace (of binders) inside of smt
-- symbols.
bindSep :: IsString a => a
bindSep = "@"

-- | The separator used to separate the caller from the callee inside of a
-- single stack frame of the stack trace.
callSep :: IsString a => a
callSep = "~~"

-- | Prefix used to show that this smt symbol was generated during a run of
-- the program.
progPrefix :: IsString a => a
progPrefix = "prog"

