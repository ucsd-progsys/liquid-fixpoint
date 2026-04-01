{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE TemplateHaskell           #-}

module Language.Fixpoint.Types.Config (
    Config  (..)
  , defConfig
  , withPragmas

  , getOpts

  -- * SMT Solver options
  , SMTSolver (..)
  , solverFlags, mkElabFlags
  , ElabFlags (..)

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
import Development.GitRev (gitHash)
import Data.Version (showVersion)
import Paths_liquid_fixpoint (version)

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
  , autoKuts         :: Bool           -- ^ ignore given kut variables
  , nonLinCuts       :: Bool           -- ^ Treat non-linear vars as cuts
  , noslice          :: Bool           -- ^ Disable non-concrete KVar slicing
  , rewriteAxioms    :: Bool           -- ^ Allow axiom instantiation via rewriting
  , pleUndecGuards   :: Bool           -- ^ Unfold invocations with undecided guards in PLE
  , etabeta          :: Bool           -- ^ Eta expand and beta reduce terms to aid PLE
  , localRewrites    :: Bool           -- ^ Eta expand and beta reduce terms to aid PLE
  , saveBfqOnError   :: Bool           -- ^ save FInfo as .bfq only on verification failure
  , interpreter      :: Bool           -- ^ Do not use the interpreter to assist PLE
  , noEnvReduction   :: Bool     -- ^ Don't use environment reduction
  , inlineANFBinds   :: Bool          -- ^ Inline ANF bindings.
                                       -- Sometimes improves performance and sometimes worsens it.
  , checkCstr        :: [Integer]      -- ^ Only check these specific constraints
  , extensionality   :: Bool           -- ^ Enable extensional interpretation of function equality
  , rwTermination    :: Bool        -- ^ Enable termination checking for rewriting
  , stdin               :: Bool        -- ^ Read input query from stdin
  , json                :: Bool        -- ^ Render output in JSON format
  , fuel                :: Maybe Int   -- ^ Maximum PLE "fuel" (unfold depth) (default=infinite)
  , restOrdering        :: String      -- ^ Term ordering for use in REST
  , noStringTheory :: Bool             -- ^ disable interpretation of string theory by SMT
  , explicitKvars  :: Bool             -- ^ use explicitly declared kvars (horn style) which disables several "defensive simplifications"
  , sortedSolution :: Bool             -- ^ leave sorts in the solution
  , saveDir        :: Maybe FilePath    -- ^ output directory for --save generated files (default: .liquid/ next to source)
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

data SMTSolver = Z3 | Z3mem | Cvc4 | Cvc5 | Mathsat
                 deriving (Eq, Data, Typeable, Generic)

data ElabFlags = ElabFlags { elabSetBag :: Bool, elabExplicitKvars :: Bool }

mkElabFlags :: SMTSolver -> Bool -> ElabFlags
mkElabFlags slv expKvars = ElabFlags (setBag slv) expKvars
  where
    setBag Z3    = True
    setBag Z3mem = True
    setBag _     = False

solverFlags :: Config -> ElabFlags
solverFlags cfg = mkElabFlags (solver cfg) (explicitKvars cfg)

instance Default SMTSolver where
  def = if Conditional.Z3.builtWithZ3AsALibrary then Z3mem else Z3

instance Show SMTSolver where
  show Z3      = "z3"
  show Z3mem   = "z3 API"
  show Cvc4    = "cvc4"
  show Cvc5    = "cvc5"
  show Mathsat = "mathsat"

instance S.Store SMTSolver

---------------------------------------------------------------------------------------
-- | `Scrape` describes which (Horn) constraints to scrape qualifiers from
--   No   = do not scrape, only use the supplied qualifiers
--   Head = scrape only from the constraint heads (i.e. "rhs")
--   Both = scrape all concrete predicates (i.e. "rhs" + "lhs")

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
  , noStringTheory           = False   &= help "Disable use of string theory by SMT"
  , allowHO                  = False   &= help "Allow higher order binders into fixpoint environment"
  , allowHOqs                = False   &= help "Allow higher order qualifiers"
  , eliminate                = None    &= help "Eliminate KVars [none = quals for all-kvars, cuts = quals for cut-kvars, all = eliminate all-kvars (TRUE for cuts)]"
  , scrape                   = def     &= help "Scrape qualifiers from constraint (Horn format only) [ no = do not, head = scrape from heads, both = scrape from everywhere ]"
  , elimBound                = Nothing &= name "elimBound"   &= help "(alpha) Maximum eliminate-chain depth"
  , smtTimeout               = Nothing &= name "smtTimeout"  &= help "SMT timeout in msec"
  , elimStats                = False   &= help "(alpha) Print eliminate stats"
  , solverStats              = False   &= help "Print solver stats"
  , save                     = False   &= help "Save Query as .fq and .bfq files"
  , saveBfqOnError           = False   &= help "Save Query as .bfq file only when verification fails"
                                       &= name "save-bfq-on-error"
                                       &= explicit
  , saveDir                  = Nothing
      &= name "save-dir"
      &= help "Output directory for --save generated files (default: .liquid/ next to source)"
      &= opt (Nothing :: Maybe FilePath)
      &= explicit
      &= typDir
  , metadata                 = False   &= help "Print meta-data associated with constraints"
  , stats                    = False   &= help "Compute constraint statistics"
  , etaElim                  = False   &= help "Eta elimination in function definition"
  , parts                    = False   &= help "Partition constraints into indepdendent .fq files"
  , cores                    = def     &= help "(numeric) Number of threads to use"
  , minPartSize              = defaultMinPartSize &= help "(numeric) Minimum partition size when solving in parallel"
  , maxPartSize              = defaultMaxPartSize &= help "(numeric) Maximum partiton size when solving in parallel."
  , minimize                 = False &= help "Delta debug to minimize fq file (unsat with min constraints)"
  , minimizeQs               = False &= help "Delta debug to minimize fq file (sat with min qualifiers)"
  , minimizeKs               = False &= help "Delta debug to minimize fq file (sat with max kvars replaced by True)"
  , minimalSol               = False &= help "Shrink fixpoint by removing implied qualifiers"
  , autoKuts                 = False &= help "Ignore given Kut vars, compute from scratch"
  , nonLinCuts               = False &= help "Treat non-linear kvars as cuts"
  , noslice                  = False &= help "Disable non-concrete KVar slicing"
  , rewriteAxioms            = False &= name "ple" &= help "Allow axiom instantiation via rewriting (PLE)"
  , pleUndecGuards   =
      False
        &= name "ple-with-undecided-guards"
        &= help "Unfold invocations with undecided guards in PLE"
        &= explicit
  , interpreter              =
      False
        &= name "interpreter"
        &= help "Use the interpreter to assist PLE"
  , etabeta                  = False &= help "Use eta expansion and beta reduction to aid PLE"
  , localRewrites            = False &= help "Perform local rewrites inside PLE"
  , noEnvReduction           = False &= help "Don't perform environment reduction"
  , inlineANFBinds           = False &= help (unwords
          [ "Inline ANF bindings."
          , "Sometimes improves performance and sometimes worsens it."
          , "Disabled by --noenvreduction"
          ])
  , checkCstr                = []    &= help "Only check these specific constraint-ids"
  , extensionality           = False &= help "Allow extensional interpretation of extensionality"
  , rwTermination       = False   &= help "Enable rewrite divergence checker"
  , stdin                    = False   &= help "Read input query from stdin"
  , json                     = False   &= help "Render result in JSON"
  , fuel                     = Nothing &= help "Maximum fuel (per-function unfoldings) for PLE"
  , restOrdering             = "rpo"   &= help "Ordering Constraint Algebra to use for REST"
  , explicitKvars            = False &= help "Use explicitly declared kvars (horn style) which disables several defensive simplifications"
  , sortedSolution           = False &= help "Leave elaborated sorts in the solution (only for machine consumption)"
  }
  &= verbosity
  &= program "fixpoint"
  &= help    "Predicate Abstraction Based Horn-Clause Solver"
  &= summary summaryInfo
  &= details [ "Predicate Abstraction Based Horn-Clause Solver"
             , ""
             , "To check a file foo.fq type:"
             , "  fixpoint foo.fq"
             ]

summaryInfo :: String
summaryInfo = "fixpoint " ++ showVersion version ++ " " ++ "("  ++ $(gitHash) ++ ")"
config :: Mode (CmdArgs Config)
config = cmdArgsMode defConfig

getOpts :: IO Config
getOpts = do
  md <- cmdArgs defConfig
  whenNormal (putStrLn banner)
  return md

banner :: String
banner =  "\n\nLiquid-Fixpoint Copyright 2009-25 Regents of the University of California.\n"
       ++ "All Rights Reserved.\n"

restOC :: Config -> RESTOrdering
restOC cfg = read (restOrdering cfg)

multicore :: Config -> Bool
multicore cfg = cores cfg /= Just 1

queryFile :: Ext -> Config -> FilePath
queryFile e cfg = extFileNameR' (saveDir cfg) e (srcFile cfg)
