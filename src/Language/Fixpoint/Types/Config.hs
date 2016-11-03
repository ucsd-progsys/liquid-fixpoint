{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE DeriveGeneric             #-}

module Language.Fixpoint.Types.Config (
    Config  (..)
  , defConfig
  , withPragmas

  , getOpts
  , SMTSolver (..)
  , defaultMinPartSize
  , defaultMaxPartSize
  , multicore
  , queryFile
) where

import Control.Monad
import GHC.Generics
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import System.Environment

import Language.Fixpoint.Utils.Files


withPragmas :: Config -> [String] -> IO Config
---------------------------------------------------------------------------------------
withPragmas cfg ps = foldM withPragma cfg ps

withPragma :: Config -> String -> IO Config
withPragma c s = withArgs [s] $ cmdArgsRun
          config { modeValue = (modeValue config) { cmdArgsValue = c } }

------------------------------------------------------------------------
-- Configuration Options -----------------------------------------------
------------------------------------------------------------------------

defaultMinPartSize :: Int
defaultMinPartSize = 500

defaultMaxPartSize :: Int
defaultMaxPartSize = 700

data Config
  = Config {
      srcFile     :: FilePath            -- ^ src file (*.hs, *.ts, *.c, or even *.fq or *.bfq)
    , cores       :: Maybe Int           -- ^ number of cores used to solve constraints
    , minPartSize :: Int                 -- ^ Minimum size of a partition
    , maxPartSize :: Int                 -- ^ Maximum size of a partition. Overrides minPartSize
    , solver      :: SMTSolver           -- ^ which SMT solver to use
    , linear      :: Bool                -- ^ not interpret div and mul in SMT
    , stringTheory :: Bool               -- ^ interpretation of string theory by SMT
    , defunction  :: Bool                -- ^ Allow higher order binders into fixpoint environment
    , allowHO     :: Bool                -- ^ allow higher order binders in the logic environment
    , allowHOqs   :: Bool                -- ^ allow higher order qualifiers
    , eliminate   :: Bool                -- ^ eliminate non-cut KVars
    -- , oldElim     :: Bool                -- ^ use old eliminate algorithm (deprecate)
    , elimBound   :: Maybe Int           -- ^ maximum length of KVar chain to eliminate
    , elimStats   :: Bool                -- ^ print eliminate stats
    , solverStats :: Bool                -- ^ print solver stats
    , metadata    :: Bool                -- ^ print meta-data associated with constraints
    , stats       :: Bool                -- ^ compute constraint statistics
    , parts       :: Bool                -- ^ partition FInfo into separate fq files
    , save        :: Bool                -- ^ save FInfo as .bfq and .fq file
    , minimize    :: Bool                -- ^ min .fq by delta debug (unsat with min constraints)
    , minimizeQs  :: Bool                -- ^ min .fq by delta debug (sat with min qualifiers)
    , minimizeKs  :: Bool                -- ^ min .fq by delta debug (sat with min kvars)
    -- , nontriv     :: Bool             -- ^ simplify using non-trivial sorts
    , gradual     :: Bool                -- ^ solve "gradual" constraints
    , extensionality   :: Bool           -- ^ allow function extensionality
    , alphaEquivalence :: Bool           -- ^ allow lambda alpha equivalence axioms
    , betaEquivalence  :: Bool           -- ^ allow lambda beta equivalence axioms
    , normalForm       :: Bool           -- ^ allow lambda normal-form equivalence axioms
    , autoKuts    :: Bool                -- ^ ignore given kut variables
    -- , pack        :: Bool                -- ^ Use pack annotations
    , nonLinCuts  :: Bool                -- ^ Treat non-linear vars as cuts
    } deriving (Eq,Data,Typeable,Show)

instance Default Config where
  def = defConfig

---------------------------------------------------------------------------------------

data SMTSolver = Z3 | Cvc4 | Mathsat
                 deriving (Eq, Data, Typeable, Generic)

instance Default SMTSolver where
  def = Z3

instance Show SMTSolver where
  show Z3      = "z3"
  show Cvc4    = "cvc4"
  show Mathsat = "mathsat"

---------------------------------------------------------------------------------------

defConfig :: Config
defConfig = Config {
    srcFile     = "out"   &= args    &= typFile
  , defunction  = False
           &= help "Allow higher order binders into fixpoint environment"
  , solver      = def     &= help "Name of SMT Solver"
  , linear      = False   &= help "Use uninterpreted integer multiplication and division"
  , stringTheory = False  &= help "Interpretation of String Theory by SMT"
  , allowHO     = False
          &= help "Allow higher order binders into fixpoint environment"
  , allowHOqs   = False   &= help "Allow higher order qualifiers"
  , eliminate   = False   &= help "Eliminate non-cut KVars"
  -- , oldElim     = True    &= help "(default) Use old eliminate algorithm"
  , elimBound   = Nothing &= name "elimBound"
                          &= help "(alpha) Maximum eliminate-chain depth"
  , elimStats   = False   &= help "(alpha) Print eliminate stats"
  , solverStats = False   &= help "Print solver stats"
  , save        = False   &= help "Save Query as .fq and .bfq files"
  , metadata    = False   &= help "Print meta-data associated with constraints"
  , stats       = False   &= help "Compute constraint statistics"
  , parts       = False   &= help "Partition constraints into indepdendent .fq files"
  , cores       = def     &= help "(numeric) Number of threads to use"
  , minPartSize = defaultMinPartSize &= help "(numeric) Minimum partition size when solving in parallel"
  , maxPartSize = defaultMaxPartSize &= help "(numeric) Maximum partiton size when solving in parallel."
  , minimize    = False &= help "Delta debug to minimize fq file (unsat with min constraints)"
  , minimizeQs  = False &= help "Delta debug to minimize fq file (sat with min qualifiers)"
  , minimizeKs  = False &= help "Delta debug to minimize fq file (sat with max kvars replaced by True)"
  , gradual     = False &= help "Solve gradual-refinement typing constraints"
  , extensionality = False &= help "Allow function extensionality axioms"
  , alphaEquivalence = False &= help "Allow lambda alpha equivalence axioms"
  , betaEquivalence = False &= help "Allow lambda alpha equivalence axioms"
  , normalForm     = False  &= help "Allow lambda normal-form equivalence axioms"
  , autoKuts       = False &= help "Ignore given Kut vars, compute from scratch"
  -- , pack           = False &= help "Use pack annotations"
  , nonLinCuts     = False &= help "Treat non-linear kvars as cuts"
  }
  &= verbosity
  &= program "fixpoint"
  &= help    "Predicate Abstraction Based Horn-Clause Solver"
  &= summary "fixpoint Copyright 2009-15 Regents of the University of California."
  &= details [ "Predicate Abstraction Based Horn-Clause Solver"
             , ""
             , "To check a file foo.fq type:"
             , "  fixpoint foo.fq"
             ]

config :: Mode (CmdArgs Config)
config = cmdArgsMode $ defConfig

getOpts :: IO Config
getOpts = do md <- cmdArgs defConfig
             putStrLn banner
             return md

banner :: String
banner =  "\n\nLiquid-Fixpoint Copyright 2013-15 Regents of the University of California.\n"
       ++ "All Rights Reserved.\n"

multicore :: Config -> Bool
multicore cfg = cores cfg /= Just 1

queryFile :: Ext -> Config -> FilePath
queryFile e = extFileName e . srcFile
