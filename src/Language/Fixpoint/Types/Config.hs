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

  -- * SMT Solver options
  , SMTSolver (..)

  -- REST Options
  , RESTOrdering (..)
  , restOC

  -- * Eliminate options
  , Eliminate (..)
  , useElim

  -- * Scrape options
  , Scrape (..)

  -- * parallel solving options
  , defaultMinPartSize
  , defaultMaxPartSize
  , multicore

  , queryFile
) where

import qualified Data.Store as S
import qualified Data.List as L
import Data.Serialize                (Serialize (..))
import Control.DeepSeq
import GHC.Generics
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit

import qualified Language.Fixpoint.Conditional.Z3 as Conditional.Z3
import Language.Fixpoint.Utils.Files


--------------------------------------------------------------------------------
withPragmas :: Config -> [String] -> IO Config
--------------------------------------------------------------------------------
withPragmas c s =
    processValueIO
      config { modeValue = (modeValue config) { cmdArgsValue = c } }
      s
    >>=
      cmdArgsApply

--------------------------------------------------------------------------------
-- | Configuration Options -----------------------------------------------------
--------------------------------------------------------------------------------

defaultMinPartSize :: Int
defaultMinPartSize = 500

defaultMaxPartSize :: Int
defaultMaxPartSize = 700


data Config = Config
  { srcFile     :: FilePath            -- ^ src file (*.hs, *.ts, *.c, or even *.fq or *.bfq)
  , cores       :: Maybe Int           -- ^ number of cores used to solve constraints
  , minPartSize :: Int                 -- ^ Minimum size of a partition
  , maxPartSize :: Int                 -- ^ Maximum size of a partition. Overrides minPartSize
  , solver      :: SMTSolver           -- ^ which SMT solver to use
  , linear      :: Bool                -- ^ not interpret div and mul in SMT
  , stringTheory :: Bool               -- ^ interpretation of string theory by SMT
  , defunction  :: Bool                -- ^ defunctionalize (use 'apply' for all uninterpreted applications)
  , allowHO     :: Bool                -- ^ allow higher order binders in the logic environment
  , allowHOqs   :: Bool                -- ^ allow higher order qualifiers
  , eliminate   :: Eliminate           -- ^ eliminate non-cut KVars
  , scrape      :: Scrape              -- ^ configure auto-scraping of qualifiers from constraints
  , elimBound   :: Maybe Int           -- ^ maximum length of KVar chain to eliminate
  , smtTimeout  :: Maybe Int           -- ^ smt timeout in msec
  , elimStats   :: Bool                -- ^ print eliminate stats
  , solverStats :: Bool                -- ^ print solver stats
  , metadata    :: Bool                -- ^ print meta-data associated with constraints
  , stats       :: Bool                -- ^ compute constraint statistics
  , parts       :: Bool                -- ^ partition FInfo into separate fq files
  , save        :: Bool                -- ^ save FInfo as .bfq and .fq file
  , minimize    :: Bool                -- ^ min .fq by delta debug (unsat with min constraints)
  , minimizeQs  :: Bool                -- ^ min .fq by delta debug (sat with min qualifiers)
  , minimizeKs  :: Bool                -- ^ min .fq by delta debug (sat with min kvars)
  , minimalSol  :: Bool                -- ^ shrink final solution by pruning redundant qualfiers from fixpoint
  , etaElim     :: Bool                -- ^ eta eliminate function definitions
  , gradual     :: Bool                -- ^ solve "gradual" constraints
  , ginteractive :: Bool                -- ^ interactive gradual solving
  , counterexample :: Bool             -- ^ Tries to produce a counter example if unsafe
  , autoKuts         :: Bool           -- ^ ignore given kut variables
  , nonLinCuts       :: Bool           -- ^ Treat non-linear vars as cuts
  , noslice          :: Bool           -- ^ Disable non-concrete KVar slicing
  , rewriteAxioms    :: Bool           -- ^ Allow axiom instantiation via rewriting
  , pleWithUndecidedGuards :: Bool     -- ^ Unfold invocations with undecided guards in PLE
  , interpreter      :: Bool           -- ^ Do not use the interpreter to assist PLE
  , oldPLE           :: Bool           -- ^ Use old version of PLE
  , noIncrPle        :: Bool           -- ^ Use incremental PLE
  , noEnvironmentReduction :: Bool     -- ^ Don't use environment reduction
  , inlineANFBindings :: Bool          -- ^ Inline ANF bindings.
                                       -- Sometimes improves performance and sometimes worsens it.
  , checkCstr        :: [Integer]      -- ^ Only check these specific constraints
  , extensionality   :: Bool           -- ^ Enable extensional interpretation of function equality
  , rwTerminationCheck  :: Bool        -- ^ Enable termination checking for rewriting
  , stdin               :: Bool        -- ^ Read input query from stdin
  , json                :: Bool        -- ^ Render output in JSON format
  , noLazyPLE           :: Bool
  , fuel                :: Maybe Int   -- ^ Maximum PLE "fuel" (unfold depth) (default=infinite)
  , restOrdering        :: String      -- ^ Term ordering for use in REST
  , noSmtHorn           :: Bool        -- ^ Do not use (new) SMTLIB horn parser
  } deriving (Eq,Data,Typeable,Show,Generic)

instance Default Config where
  def = defConfig

---------------------------------------------------------------------------------------

data RESTOrdering = RESTKBO | RESTLPO | RESTRPO | RESTFuel Int
                 deriving (Eq, Data, Typeable, Generic)

instance Default RESTOrdering where
  def = RESTRPO

instance Show RESTOrdering where
  show RESTKBO      = "kbo"
  show RESTLPO      = "lpo"
  show RESTRPO      = "rpo"
  show (RESTFuel n) = "fuel" ++ show n

instance Read RESTOrdering where
  readsPrec _ s | "kbo" `L.isPrefixOf` s = [(RESTKBO, drop 3 s)]
  readsPrec _ s | "lbo" `L.isPrefixOf` s = [(RESTLPO, drop 3 s)]
  readsPrec _ s | "rpo" `L.isPrefixOf` s = [(RESTRPO, drop 3 s)]
  readsPrec n s | "fuel" `L.isPrefixOf` s = do
                        (fuel', rest) <- readsPrec n (drop 4 s)
                        return (RESTFuel fuel', rest)
  readsPrec _ _ = []

---------------------------------------------------------------------------------------

data SMTSolver = Z3 | Z3mem | Cvc4 | Mathsat
                 deriving (Eq, Data, Typeable, Generic)

instance Default SMTSolver where
  def = if Conditional.Z3.builtWithZ3AsALibrary then Z3mem else Z3

instance Show SMTSolver where
  show Z3      = "z3"
  show Z3mem   = "z3 API"
  show Cvc4    = "cvc4"
  show Mathsat = "mathsat"

instance S.Store SMTSolver

---------------------------------------------------------------------------------------
-- | `Scrape` describes which (Horn) constraints to scrape qualifiers from
--   None = do not scrape, only use the supplied qualifiers
--   Head = scrape only from the constraint heads (i.e. "rhs")
--   All  = scrape all concrete predicates (i.e. "rhs" + "lhs")

data Scrape = No | Head | Both
  deriving (Eq, Data, Typeable, Generic)

instance Serialize Scrape
instance S.Store Scrape
instance NFData Scrape

instance Default Scrape where
  def = No

instance Show Scrape where
  show No   = "no"
  show Head = "head"
  show Both = "both"

---------------------------------------------------------------------------------------
-- | Eliminate describes the number of KVars to eliminate:
--   None = use PA/Quals for ALL k-vars, i.e. no eliminate
--   Some = use PA/Quals for CUT k-vars, i.e. eliminate non-cuts
--   All  = eliminate ALL k-vars, solve cut-vars to TRUE
--   Horn = eliminate kvars using the Horn solver
--   Existentials = eliminate kvars and existentials
---------------------------------------------------------------------------------------
data Eliminate
  = None
  | Some
  | All
  | Horn
  | Existentials
  deriving (Eq, Data, Typeable, Generic)

instance Serialize Eliminate
instance S.Store Eliminate
instance NFData SMTSolver
instance NFData Eliminate

instance Default Eliminate where
  def = None

instance Show Eliminate where
  show None = "none"
  show Some = "some"
  show All  = "all"
  show Horn  = "horn"
  show Existentials  = "existentials"


useElim :: Config -> Bool
useElim cfg = eliminate cfg /= None

---------------------------------------------------------------------------------------

defConfig :: Config
defConfig = Config {
    srcFile                  = "out"   &= args    &= typFile
  , defunction               = False   &= help "Allow higher order binders into fixpoint environment"
  , solver                   = def     &= help "Name of SMT Solver"
  , linear                   = False   &= help "Use uninterpreted integer multiplication and division"
  , stringTheory             = False   &= help "Interpretation of String Theory by SMT"
  , allowHO                  = False   &= help "Allow higher order binders into fixpoint environment"
  , allowHOqs                = False   &= help "Allow higher order qualifiers"
  , eliminate                = None    &= help "Eliminate KVars [none = quals for all-kvars, cuts = quals for cut-kvars, all = eliminate all-kvars (TRUE for cuts)]"
  , scrape                   = def     &= help "Scrape qualifiers from constraint (Horn format only) [ no = do not, head = scrape from heads, both = scrape from everywhere ]"
  , elimBound                = Nothing &= name "elimBound"   &= help "(alpha) Maximum eliminate-chain depth"
  , smtTimeout               = Nothing &= name "smtTimeout"  &= help "smt timeout in msec"
  , elimStats                = False   &= help "(alpha) Print eliminate stats"
  , solverStats              = False   &= help "Print solver stats"
  , save                     = False   &= help "Save Query as .fq and .bfq files"
  , metadata                 = False   &= help "Print meta-data associated with constraints"
  , stats                    = False   &= help "Compute constraint statistics"
  , etaElim                  = False   &= help "eta elimination in function definition"
  , parts                    = False   &= help "Partition constraints into indepdendent .fq files"
  , cores                    = def     &= help "(numeric) Number of threads to use"
  , minPartSize              = defaultMinPartSize &= help "(numeric) Minimum partition size when solving in parallel"
  , maxPartSize              = defaultMaxPartSize &= help "(numeric) Maximum partiton size when solving in parallel."
  , minimize                 = False &= help "Delta debug to minimize fq file (unsat with min constraints)"
  , minimizeQs               = False &= help "Delta debug to minimize fq file (sat with min qualifiers)"
  , minimizeKs               = False &= help "Delta debug to minimize fq file (sat with max kvars replaced by True)"
  , minimalSol               = False &= help "Shrink fixpoint by removing implied qualifiers"
  , gradual                  = False &= help "Solve gradual-refinement typing constraints"
  , ginteractive             = False &= help "Interactive Gradual Solving"
  , counterexample           = False &= name "counterexample" &= help "Tries to produce a counter example for unsafe clauses"
  , autoKuts                 = False &= help "Ignore given Kut vars, compute from scratch"
  , nonLinCuts               = False &= help "Treat non-linear kvars as cuts"
  , noslice                  = False &= help "Disable non-concrete KVar slicing"
  , rewriteAxioms            = False &= help "allow axiom instantiation via rewriting (PLE)"
  , pleWithUndecidedGuards   =
      False
        &= name "ple-with-undecided-guards"
        &= help "Unfold invocations with undecided guards in PLE"
        &= explicit
  , interpreter              =
      False
        &= name "interpreter"
        &= help "Use the interpreter to assist PLE"
  , oldPLE                   = False &= help "Use old version of PLE"
  , noIncrPle                = False &= help "Don't use incremental PLE"
  , noEnvironmentReduction   =
      False
        &= name "no-environment-reduction"
        &= help "Don't perform environment reduction"
  , inlineANFBindings        =
      False
        &= name "inline-anf-bindings"
        &= help (unwords
          [ "Inline ANF bindings."
          , "Sometimes improves performance and sometimes worsens it."
          , "Disabled by --no-environment-reduction"
          ])
  , checkCstr                = []    &= help "Only check these specific constraint-ids"
  , extensionality           = False &= help "Allow extensional interpretation of extensionality"
  , rwTerminationCheck       = False   &= help "Enable rewrite divergence checker"
  , stdin                    = False   &= help "Read input query from stdin"
  , json                     = False   &= help "Render result in JSON"
  , noLazyPLE                = False   &= help "Don't use lazy PLE"
  , fuel                     = Nothing &= help "Maximum fuel (per-function unfoldings) for PLE"
  , restOrdering             = "rpo"
        &= name "rest-ordering"
        &= help "Ordering Constraint Algebra to use for REST"
  , noSmtHorn                = False &= help "Do not use SMTLIB horn format"
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
config = cmdArgsMode defConfig

getOpts :: IO Config
getOpts = do
  md <- cmdArgs defConfig
  whenNormal (putStrLn banner)
  return md

banner :: String
banner =  "\n\nLiquid-Fixpoint Copyright 2013-21 Regents of the University of California.\n"
       ++ "All Rights Reserved.\n"

restOC :: Config -> RESTOrdering
restOC cfg = read (restOrdering cfg)

multicore :: Config -> Bool
multicore cfg = cores cfg /= Just 1

queryFile :: Ext -> Config -> FilePath
queryFile e = extFileName e . srcFile
