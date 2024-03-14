{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Fixpoint.Counterexample.JSON
  ( Symbol
  , Refinement
  , Expr
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

import Control.Exception
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

-- data RowColumn = RowColumn
--   { row :: !Int
--   , col :: !Int
--   } deriving (Generic, Show)
-- 
-- instance FromJSON RowColumn
-- instance ToJSON RowColumn
-- 
-- data Location = Location
--   { start :: !RowColumn
--   , end :: !RowColumn
--   , path :: !Path
--   } deriving (Generic, Show)

data Span = Span
  { start :: !Int
  , length :: !Int
  } deriving (Generic, Show)

instance FromJSON Span
instance ToJSON Span

data Location = Location
  { span :: !Span
  , path :: !Path
  } deriving (Generic, Show)

instance FromJSON Location
instance ToJSON Location

type Path = Text
type Symbol = Text
type Refinement = Text
type Expr = Text

-- TODO: Check liquidhaskell-boot/src/Language/Haskell/Liquid/UX/Tidy.hs
-- from LH to copy how they make the environment "tidy".
jsonCex :: (F.Loc info, MonadIO m) => F.Config -> F.SInfo info -> F.FullCounterexample (F.SubcId, info) -> m ()
jsonCex cfg si cex | F.save cfg = do
  cex' <- formatCex si cex
  let ext = Ext.Cex . fromIntegral . F.cexConstraint $ cex
  let file = F.queryFile ext cfg
  liftIO $ F.ensurePath file
  liftIO $ encodeFile file cex'
jsonCex _ _ _ = return ()

formatCex :: (F.Loc info, MonadIO m) => F.SInfo info -> F.FullCounterexample (F.SubcId, info) -> m Counterexample
formatCex si cex = do
  env <- formatEnv si cex
  cst <- formatConstraint si cex
  return Counterexample
    { environment = env
    , constraint = cst
    }

formatEnv :: (F.Loc info, MonadIO m) => F.SInfo info -> F.FullCounterexample (F.SubcId, info) -> m [Variable]
formatEnv si cex = do
  mapM formatVar vars
  where
    vars = Map.toList . F.beBinds . F.cexEnv $ cex

    formatVar (bid, (sym, synth, (conc, (_, info)))) = do
      must <- mapM (formatCex si) $ F.cexFrames cex Map.!? bid
      loc <- getLocation info
      return Variable
        { symbol = ppFormat sym
        , expression = ppFormat conc
        , refinement = ppFormat synth
        , must_instance = must
        , location = loc
        }

formatConstraint :: (F.Loc info, MonadIO m) => F.SInfo info -> F.FullCounterexample (F.SubcId, info) -> m Constraint
formatConstraint si cex = do
  let horn = F.cm si Map.! F.cexConstraint cex
  let cheadId = F.cbind horn
  let binds = F.beBinds . F.cexEnv $ cex

    -- Get the head of the constraint.
  let (_csym, csynth, (cconcrete, (_, info))) = binds Map.! cheadId

  -- Get checked expr
  let cexpr = F.crhs horn
  let ccheck = withExpr csynth cexpr

  loc <- getLocation info

  return Constraint
    { concrete = ppFormat cconcrete
    , synthesized = ppFormat csynth
    , required = ppFormat ccheck
    , location = loc
    }
  where
    withExpr sr e = sr { F.sr_reft = F.Reft (sort, e) }
      where
        F.Reft (sort, _) = F.sr_reft sr

ppFormat :: F.PPrint a => a -> Text
ppFormat = T.pack . show . F.pprintTidy F.Lossy

-- | Storing spans with columns and rows doesn't really make sense when
-- printing. The JSON format instead just stores a range. This function does
-- the conversion, though it might be a bit slow, as we are quite literally
-- counting the number of characters to reach the span.
getLocation :: MonadIO m => F.Loc info => info -> m (Maybe Location)
getLocation i = liftIO $ handle ignore $ do
  -- Helpers
  let getRow = F.unPos . F.sourceLine
  let getCol = F.unPos . F.sourceColumn

  -- The initial SourceSpan
  let F.SS { sp_start, sp_stop } = F.srcSpan i
  let path = F.sourceName sp_start
  let startRow = getRow sp_start - 1
  let endRow = getRow sp_stop
  let startCol = getCol sp_start - 1
  let endCol = getCol sp_stop

  -- Split between what comes before and the rows that actually contain the
  -- content.
  content <- lines <$> readFile path
  let (before, rest) = splitAt startRow content
  let (content', _) = splitAt (endRow - startRow) rest

  -- This part remove the start and end of the rows in which the final span
  -- lies. The start and end is dictated by the columns.
  (hs, l) <- case unsnoc content' of
    Just v -> return v
    _ -> throwIO $ userError "incorrect range"
  let content'' = hs <> [take endCol l]
  (h, ls) <- case uncons content'' of
    Just v -> return v
    _ -> throwIO $ userError "incorrect range"
  let content''' = drop startCol h : ls

  -- Calculate the final start and length, including the number of newline
  -- characters.
  let start = sum (Prelude.length <$> before) + Prelude.length before + startCol
  let len = sum (Prelude.length <$> content''') + Prelude.length content''' - 1

  return . Just $ Location
        { span = Span
          { start = start
          , length = len
          }
        , path = T.pack path
        }

ignore :: MonadIO m => IOException -> m (Maybe a)
ignore = const $ return Nothing

-- TODO: Remove these definitions of unsnoc and uncons once the GHC version is
-- high enough such that they're in Data.List. Don't forget to add them to the
-- import in this case!
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (x:xs) = Just (x, xs)
