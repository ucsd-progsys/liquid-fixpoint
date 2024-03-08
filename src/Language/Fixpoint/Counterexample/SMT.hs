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
import Text.Read (readMaybe)
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
smtDeclare scope (Decl sym sort) = do
  ctx <- reader smtContext
  let sym' = scopeSym scope sym
  liftIO $ SMT.smtDecl ctx sym' sort
  let Su sub = binders scope
  let binders' = Su $ Map.insert sym (EVar sym') sub
  return scope { binders = binders' }

-- | Assert the given expression.
smtAssert :: MonadSMT s m => Expr -> m ()
smtAssert = smtAssume . PNot

-- | Assume the given expression.
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
  sub <- liftIO $ SMT.smtGetModel ctx
  return $ smtSubstToCex sub

-- | Transform an SMT substitution, which contains SMT scoped symbols, into
-- a layered, tree-like counterexample.
smtSubstToCex :: Subst -> SMTCounterexample
smtSubstToCex (Su sub) = foldl' (flip $ uncurry insertCex) dummyCex traces
  where
    -- | Unwind SMT names.
    renames = first unscopeSym <$> Map.toList sub

    -- | Keep just the ones that were introduced by the counterexample checker.
    traces = [ (k, e) | (Just k, e) <- renames ]

    -- | A dummy counterexample to fill empty entries on insertion
    dummyCex = Counterexample mempty 0 mempty

    -- | Insert a scoped name with its expression into the SMT counterexample
    insertCex (sym, cid, trace) e = go $ reverse trace
      where 
      go :: Trace -> SMTCounterexample -> SMTCounterexample
      go (t:ts) cex = cex
        { cexFrames = Map.insertWith (const $ go ts) t dummyCex $ cexFrames cex
        }
      go _ cex@Counterexample 
        { cexEnv = Su su
        } = cex
          { cexConstraint = cid
          , cexEnv = Su . Map.insert sym e $ su
          }

-- | Returns a version of a symbol with the scope encoded into its name.
scopeSym :: Scope -> Symbol -> Symbol
scopeSym scope sym = symbol name
  where
    name = intercalate bindSep strs
    strs = symbolString <$> progPrefix : sym : cid : paths
    cid = symbol . show . constraint $ scope
    paths = symbol . show <$> path scope

-- | We encode the trace of a symbol in its name. This way,
-- we do not lose it in the SMT solver. This function translates
-- the encoding back.
unscopeSym :: Symbol -> Maybe (Symbol, SubcId, Trace)
unscopeSym sym = do
  -- Remove the escape tokens from the SMT formatted symbol
  sym' <- escapeSmt . symbolText $ sym

  -- Check if it is in the program form
  (name, cid, trace) <- case T.splitOn bindSep sym' of
    (prefix:name:cid:trace) -> do
      guard $ prefix == progPrefix
      return (name, cid, trace)
    _ -> Nothing

  let read' :: Read a => T.Text -> Maybe a
      read' = readMaybe . T.unpack

  -- Try to parse the trace and constraint id
  trace' <- sequence $ read' <$> trace
  cid' <- read' cid
  return (symbol name, cid', trace')

-- | Remove escape tokens applied to the input string when it was formatted to
-- SMT string.
escapeSmt :: T.Text -> Maybe T.Text
escapeSmt = go False . T.split (=='$')
  where
    go _ [] = return  ""
    go escape (t:ts) = txt t <> go (not escape) ts
      where
        txt | escape    = readMaybe . T.unpack >=> return . T.singleton . chr 
            | otherwise = return

-- | The separator used to encode the stack trace (of binders) inside of smt
-- symbols.
bindSep :: IsString a => a
bindSep = "@"

-- | Prefix used to show that this smt symbol was generated during a run of
-- the program.
progPrefix :: IsString a => a
progPrefix = "prog"

