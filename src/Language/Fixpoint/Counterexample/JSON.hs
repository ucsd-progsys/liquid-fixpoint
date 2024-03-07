{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Fixpoint.Counterexample.JSON
  ( Symbol
  , Refinement
  , Expr
  , Path
  , Span (..)
  , Location (..)
  , Variable (..)
  , Constraint (..)
  , Counterexample (..)
  , formatCex
  , jsonCex
  ) where

import qualified Language.Fixpoint.Utils.Files as Ext
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Misc as F
import qualified Language.Fixpoint.Types.Config as F
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, encodeFile)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as Map

import Control.Monad.IO.Class

data Counterexample = Counterexample
  { environment :: ![Variable]
  , constraint :: !Constraint
  } deriving (Generic, Show)

instance FromJSON Counterexample
instance ToJSON Counterexample

data Variable = Variable
  { symbol :: !Symbol
  , expression :: !Expr
  , refinement :: !Refinement
  , must_instance :: !(Maybe Counterexample)
  , location :: !(Maybe Location)
  } deriving (Generic, Show)

instance FromJSON Variable
instance ToJSON Variable

data Constraint = Constraint
  { concrete :: !Expr
  , synthesized :: !Refinement
  , required :: !Refinement
  , location :: !(Maybe Location)
  } deriving (Generic, Show)

instance FromJSON Constraint
instance ToJSON Constraint

data Location = Location
  { span :: !Span
  , path :: !Path
  } deriving (Generic, Show)

instance FromJSON Location
instance ToJSON Location

data Span = Span
  { start :: !Int
  , length :: !Int
  } deriving (Generic, Show)

instance FromJSON Span
instance ToJSON Span

type Symbol = Text
type Refinement = Text
type Expr = Text
type Path = Text

jsonCex :: MonadIO m => F.Config -> F.SInfo info -> F.FullCounterexample a -> m ()
jsonCex cfg si cex | F.save cfg = liftIO $ do
  let cex' = formatCex si cex
  let ext = Ext.Cex . fromIntegral . F.cexConstraint $ cex
  let file = F.queryFile ext cfg
  F.ensurePath file
  encodeFile file cex'
jsonCex _ _ _ = return ()

formatCex :: F.SInfo info -> F.FullCounterexample a -> Counterexample
formatCex si cex = Counterexample
  { environment = formatEnv si cex
  , constraint = formatConstraint si cex
  }

formatEnv :: F.SInfo info -> F.FullCounterexample a -> [Variable]
formatEnv si cex = formatVar <$> vars
  where
    vars = Map.toList . F.beBinds . F.cexEnv $ cex

    formatVar (bid, (sym, synth, (conc, _a))) = Variable
      { symbol = ppFormat sym
      , expression = formatConcrete synth conc
      , refinement = ppFormat synth
      , must_instance = must
      , location = Nothing
      }
      where
        must = formatCex si <$> F.cexFrames cex Map.!? bid

formatConstraint :: F.SInfo info -> F.FullCounterexample a -> Constraint
formatConstraint si cex = Constraint
  { concrete = formatConcrete csynth cconcrete
  , synthesized = ppFormat csynth
  , required = ppFormat ccheck
  , location = Nothing
  }
  where
    cheadId = F.cbind horn
    horn = F.cm si Map.! F.cexConstraint cex
    binds = F.beBinds . F.cexEnv $ cex

    -- Get the head of the constraint.
    (_csym, csynth, (cconcrete, _a)) = binds Map.! cheadId

    -- Get checked expr
    cexpr = F.crhs horn
    ccheck = withExpr csynth cexpr

formatConcrete :: F.SortedReft -> F.Expr -> Text
formatConcrete sr e = ppFormat sort <> "[" <> ppFormat e <> "]"
  where
    sort = F.sr_sort sr

withExpr :: F.SortedReft -> F.Expr -> F.SortedReft
withExpr sr e = sr { F.sr_reft = F.Reft (sort, e) }
  where
    F.Reft (sort, _) = F.sr_reft sr

ppFormat :: F.PPrint a => a -> Text
ppFormat = T.pack . show . F.pprint
