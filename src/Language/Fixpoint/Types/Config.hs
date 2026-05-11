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
import System.Console.GetOpt
import Language.Fixpoint.Verbosity   (Verbosity (..), setVerbosity, whenNormal)
import System.Environment            (getArgs)
import System.Exit                   (exitSuccess)

import qualified Language.Fixpoint.Conditional.Z3 as Conditional.Z3
import Language.Fixpoint.Utils.Files
import Development.GitRev (gitHash)
import Data.Version (showVersion)
import Paths_liquid_fixpoint (version)
import Language.Fixpoint.Types.Version (numericVersionInfo)

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
  , interpreter      :: Bool           -- ^ Use the interpreter to assist PLE
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
  } deriving (Eq,Show,Generic)

---------------------------------------------------------------------------------------

data RESTOrdering = RESTKBO | RESTLPO | RESTRPO | RESTFuel Int
                 deriving (Eq, Generic)

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
                 deriving (Eq, Generic)

data ElabFlags = ElabFlags { elabSetBag :: Bool, elabExplicitKvars :: Bool }

mkElabFlags :: SMTSolver -> Bool -> ElabFlags
mkElabFlags slv expKvars = ElabFlags (setBag slv) expKvars
  where
    setBag Z3    = True
    setBag Z3mem = True
    setBag _     = False

solverFlags :: Config -> ElabFlags
solverFlags cfg = mkElabFlags (solver cfg) (explicitKvars cfg)

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
  deriving (Eq, Generic)

instance Serialize Scrape
instance S.Store Scrape
instance NFData Scrape

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
  deriving (Eq, Generic)

instance Serialize Eliminate
instance S.Store Eliminate
instance NFData SMTSolver
instance NFData Eliminate

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
defConfig = Config
  { srcFile            = "out"
  , defunction         = False
  , solver             = if Conditional.Z3.builtWithZ3AsALibrary then Z3mem else Z3
  , linear             = False
  , noStringTheory     = False
  , allowHO            = False
  , allowHOqs          = False
  , eliminate          = None
  , scrape             = No
  , elimBound          = Nothing
  , smtTimeout         = Nothing
  , elimStats          = False
  , solverStats        = False
  , save               = False
  , saveBfqOnError     = False
  , saveDir            = Nothing
  , metadata           = False
  , stats              = False
  , etaElim            = False
  , parts              = False
  , cores              = Nothing
  , minPartSize        = defaultMinPartSize
  , maxPartSize        = defaultMaxPartSize
  , minimize           = False
  , minimizeQs         = False
  , minimizeKs         = False
  , minimalSol         = False
  , autoKuts           = False
  , nonLinCuts         = False
  , noslice            = False
  , rewriteAxioms      = False
  , pleUndecGuards     = False
  , interpreter        = False
  , etabeta            = False
  , localRewrites      = False
  , noEnvReduction     = False
  , inlineANFBinds     = False
  , checkCstr          = []
  , extensionality     = False
  , rwTermination      = False
  , stdin              = False
  , json               = False
  , fuel               = Nothing
  , restOrdering       = "rpo"
  , explicitKvars      = False
  , sortedSolution     = False
  }

-- | An individual parsed flag (modifier to Config, verbosity change, or exit).
data FxFlag
  = FxMod (Config -> Config)
  | FxVerbosity Verbosity
  | FxHelp
  | FxVersion
  | FxNumericVersion

-- | All command-line options for fixpoint.
fxOptions :: [OptDescr FxFlag]
fxOptions =
  [ Option [] ["defunction", "defunct"] (NoArg (FxMod $ \c -> c { defunction = True }))
      "Allow higher order binders into fixpoint environment"
  , opt0 "linear"                  (\c -> c { linear            = True  })
      "Use uninterpreted integer multiplication and division"
  , opt0 "no-string-theory"        (\c -> c { noStringTheory    = True  })
      "Disable use of string theory by SMT"
  , opt0 "allowho"                 (\c -> c { allowHO           = True  })
      "Allow higher order binders into fixpoint environment"
  , opt0 "allowhoqs"               (\c -> c { allowHOqs         = True  })
      "Allow higher order qualifiers"
  , Option [] ["eliminate"]        (ReqArg setEliminate "ELIM")
      ( unlines
          [ "Eliminate KVars [" ++ L.intercalate " | "
            ["none", "some", "all", "horn", "existentials"] ++ "]"
          , "    none: quals for all-kvars"
          , "    some: ??"
          , "    all: eliminate all-kvars (TRUE for cuts)"
          , "    horn: ??"
          , "    existentials: ??"
          ]
      )
  , Option [] ["scrape"]           (ReqArg setScrape "SCRAPE")
      (unlines
        [ "Scrape qualifiers from constraint [" ++ L.intercalate " | "
          ["no", "head", "both"] ++ "]"
        , "    no: do not"
        , "    head: scrape from heads"
        , "    both: scrape from everywhere"
        ]
      )
  , Option [] ["solver"]           (ReqArg setSolver "SOLVER")
      ("SMT solver [" ++ L.intercalate " | " ["z3", "z3mem", "cvc4", "cvc5", "mathsat"] ++ "]")
  , Option [] ["elimBound"]        (ReqArg (\s -> FxMod $ \c -> c { elimBound   = parseInt "elimBound" s }) "N")
      "(alpha) Maximum eliminate-chain depth"
  , Option [] ["smtTimeout"]       (ReqArg (\s -> FxMod $ \c -> c { smtTimeout  = parseInt "smtTimeout" s }) "N")
      "SMT timeout in msec"
  , opt0 "elim-stats"              (\c -> c { elimStats     = True })
      "(alpha) Print eliminate stats"
  , opt0 "solver-stats"            (\c -> c { solverStats   = True })
      "Print solver stats"
  , opt0 "save"                    (\c -> c { save          = True })
      "Save Query as .fq and .bfq files"
  , Option [] ["save-bfq-on-error"] (NoArg (FxMod $ \c -> c { saveBfqOnError = True }))
      "Save Query as .bfq file only when verification fails"
  , Option [] ["save-dir"]         (ReqArg setSaveDir "DIR")
      "Output directory for --save generated files (default: .liquid/ next to source)"
  , opt0 "metadata"                (\c -> c { metadata      = True })
      "Print meta-data associated with constraints"
  , opt0 "stats"                   (\c -> c { stats         = True })
      "Compute constraint statistics"
  , Option [] ["eta-elim", "etaelim"] (NoArg (FxMod $ \c -> c { etaElim = True }))
      "Eta elimination in function definition"
  , opt0 "parts"                   (\c -> c { parts         = True })
      "Partition constraints into independent .fq files"
  , Option [] ["cores"]            (ReqArg (\s -> FxMod $ \c -> c { cores = parseInt "cores" s }) "N")
      "(numeric) Number of threads to use"
  , Option [] ["min-part-size"]    (ReqArg (\s -> FxMod $ \c -> c { minPartSize = read s }) "N")
      "(numeric) Minimum partition size when solving in parallel"
  , Option [] ["max-part-size"]    (ReqArg (\s -> FxMod $ \c -> c { maxPartSize = read s }) "N")
      "(numeric) Maximum partition size when solving in parallel"
  , opt0 "minimize"                (\c -> c { minimize       = True })
      "Delta debug to minimize fq file (unsat with min constraints)"
  , opt0 "minimize-qs"             (\c -> c { minimizeQs    = True })
      "Delta debug to minimize fq file (sat with min qualifiers)"
  , opt0 "minimize-ks"             (\c -> c { minimizeKs    = True })
      "Delta debug to minimize fq file (sat with max kvars replaced by True)"
  , opt0 "minimal-sol"             (\c -> c { minimalSol    = True })
      "Shrink fixpoint by removing implied qualifiers"
  , opt0 "auto-kuts"               (\c -> c { autoKuts      = True })
      "Ignore given Kut vars, compute from scratch"
  , opt0 "non-lin-cuts"            (\c -> c { nonLinCuts    = True })
      "Treat non-linear kvars as cuts"
  , opt0 "noslice"                 (\c -> c { noslice       = True })
      "Disable non-concrete KVar slicing"
  , Option [] ["ple", "rewrite", "rewrite-axioms"] (NoArg (FxMod $ \c -> c { rewriteAxioms = True }))
      "Allow axiom instantiation via rewriting (PLE)"
  , Option [] ["ple-with-undecided-guards"] (NoArg (FxMod $ \c -> c { pleUndecGuards = True }))
      "Unfold invocations with undecided guards in PLE"
  -- Accept optional =true/=false for backward compatibility with cmdargs Bool encoding
  , Option [] ["interpreter"]      (OptArg (parseBoolOpt interpreter (\b c -> c { interpreter = b })) "BOOL")
      "Use the interpreter to assist PLE"
  , opt0 "etabeta"                 (\c -> c { etabeta        = True })
      "Use eta expansion and beta reduction to aid PLE"
  , Option [] ["local-rewrites", "localrewrites"] (NoArg (FxMod $ \c -> c { localRewrites = True }))
      "Perform local rewrites inside PLE"
  , opt0 "no-env-reduction"        (\c -> c { noEnvReduction = True })
      "Don't perform environment reduction"
  , opt0 "inline-anf-binds"        (\c -> c { inlineANFBinds = True })
      "Inline ANF bindings (sometimes improves performance, sometimes worsens it)"
  , Option [] ["check-cstr"]       (ReqArg (\s -> FxMod $ \c -> c { checkCstr = checkCstr c ++ [read s] }) "ID")
      "Only check these specific constraint-ids (repeat for multiple)"
  , opt0 "extensionality"          (\c -> c { extensionality = True })
      "Allow extensional interpretation of function equality"
  , opt0 "rw-termination"          (\c -> c { rwTermination  = True })
      "Enable rewrite divergence checker"
  , opt0 "stdin"                   (\c -> c { stdin          = True })
      "Read input query from stdin"
  , opt0 "json"                    (\c -> c { json           = True })
      "Render result in JSON"
  , Option [] ["fuel"]             (ReqArg (\s -> FxMod $ \c -> c { fuel = parseInt "fuel" s }) "N")
      "Maximum fuel (per-function unfoldings) for PLE"
  , Option [] ["rest-ordering"]    (ReqArg (\s -> FxMod $ \c -> c { restOrdering = s }) "ORD")
      "Ordering constraint algebra to use for REST"
  , opt0 "explicit-kvars"          (\c -> c { explicitKvars  = True })
      "Use explicitly declared kvars (horn style) which disables several defensive simplifications"
  , opt0 "sorted-solution"         (\c -> c { sortedSolution = True })
      "Leave elaborated sorts in the solution (only for machine consumption)"
  , Option "v" ["verbose"]         (NoArg (FxVerbosity Loud))
      "Be more verbose"
  , Option "q" ["quiet"]           (NoArg (FxVerbosity Quiet))
      "Be quiet (suppress normal output)"
  , Option "h?" ["help"]           (NoArg FxHelp)
      "Show this help message"
  , Option "V" ["version"]         (NoArg FxVersion)
      "Show version"
  , Option [] ["numeric-version"]  (NoArg FxNumericVersion)
      "Print numeric version and exit"
  ]
  where
    opt0 name f desc =
      Option [] [name] (NoArg (FxMod f)) desc

    -- Parse an optional =true/=false/=True/=False argument (cmdargs Bool compat)
    parseBoolOpt :: (Config -> Bool) -> (Bool -> Config -> Config) -> Maybe String -> FxFlag
    parseBoolOpt _   setter Nothing          = FxMod (setter True)
    parseBoolOpt _   setter (Just "true")    = FxMod (setter True)
    parseBoolOpt _   setter (Just "True")    = FxMod (setter True)
    parseBoolOpt _   setter (Just "false")   = FxMod (setter False)
    parseBoolOpt _   setter (Just "False")   = FxMod (setter False)
    parseBoolOpt getDef _setter (Just s)         =
      error $ "Expected true/false, got: " ++ s ++ " (current default: " ++ show (getDef defConfig) ++ ")"

    setSolver s   = FxMod $ \c -> c { solver    = parseSolver s }
    setEliminate s = FxMod $ \c -> c { eliminate = parseEliminate s }
    setScrape s   = FxMod $ \c -> c { scrape    = parseScrape s }
    setSaveDir s = FxMod $ \c -> c { saveDir   = Just s }

    parseSolver "z3"        = Z3
    parseSolver "z3mem"     = Z3mem
    parseSolver "cvc4"      = Cvc4
    parseSolver "cvc5"      = Cvc5
    parseSolver "mathsat"   = Mathsat
    parseSolver s           = error $ "Unknown solver: " ++ s

    parseEliminate "none"         = None
    parseEliminate "some"         = Some
    parseEliminate "all"          = All
    parseEliminate "horn"         = Horn
    parseEliminate "existentials" = Existentials
    parseEliminate s              = error $ "Unknown eliminate mode: " ++ s

    parseScrape "no"   = No
    parseScrape "head" = Head
    parseScrape "both" = Both
    parseScrape s      = error $ "Unknown scrape mode: " ++ s

    parseInt _    s = Just (read s)

-- | Apply a list of parsed flags to a base Config; return updated config and
--   verbosity change (if any).
applyFxFlags :: Config -> [FxFlag] -> IO Config
applyFxFlags base flags = do
  mapM_ applyVerbosity flags
  return $! L.foldl' applyMod base flags
  where
    applyMod c (FxMod f) = f c
    applyMod c _         = c
    applyVerbosity (FxVerbosity vb) = setVerbosity vb
    applyVerbosity _                = return ()

--------------------------------------------------------------------------------
withPragmas :: Config -> [String] -> IO Config
--------------------------------------------------------------------------------
withPragmas base tokens =
  case getOpt Permute fxOptions tokens of
    (flags, _, [])   -> applyFxFlags base flags
    (_, _, optErrs)  -> ioError $ userError $
        concat optErrs ++ "\nUse --help for usage information."

--------------------------------------------------------------------------------

summaryInfo :: String
summaryInfo = "fixpoint " ++ showVersion version ++ " " ++ "(" ++ $(gitHash) ++ ")"

getOpts :: IO Config
getOpts = do
  args <- getArgs
  case getOpt Permute fxOptions args of
    (flags, files, []) -> do
      cfg <- applyFxFlags defConfig flags
      let srcF = case files of { (f:_) -> f; [] -> srcFile defConfig }
          cfg' = cfg { srcFile = srcF }
      whenNormal (putStrLn banner)
      handleExits flags (formatHelp fxOptions) summaryInfo
      return cfg'
    (_, _, optErrs)    -> ioError $ userError $
        concat optErrs ++ "\nUse --help for usage information."

handleExits :: [FxFlag] -> String -> String -> IO ()
handleExits flags helpText ver = mapM_ go flags
  where
    go FxHelp           = putStr helpText >> exitSuccess
    go FxVersion        = putStrLn ver      >> exitSuccess
    go FxNumericVersion = putStrLn numericVersionInfo >> exitSuccess
    go _                = return ()

formatHelp :: [OptDescr a] -> String
formatHelp opts =
    unlines $
      [ "Usage: fixpoint [OPTIONS] FILE.fq"
      , ""
      , "    Predicate Abstraction Based Horn-Clause Solver"
      , ""
      , "Options:"
      , ""
      ]
      ++ L.intersperse "" (map fmtOpt opts)
  where
    fmtOpt :: OptDescr a -> String
    fmtOpt (Option short long argDesc desc) =
      let shortStr = case short of
                       []    -> ""
                       (c:_) -> "-" ++ [c]
          longStrs  = map ("--" ++) long
          argStr    = case argDesc of
                        NoArg  _   -> ""
                        ReqArg _ m -> " " ++ m
                        OptArg _ m -> "[=" ++ m ++ "]"
          indentedDesc = unlines $ map ("        " ++) $ lines desc
          synopsis  = L.intercalate ", " $
                        [shortStr | not (null shortStr)] ++ longStrs
      in "  " ++ synopsis ++ argStr ++ "\n\n" ++ indentedDesc

banner :: String
banner =  "\nLiquid-Fixpoint Copyright 2009-25 Regents of the University of California.\n"
       ++ "All Rights Reserved.\n"

restOC :: Config -> RESTOrdering
restOC cfg = read (restOrdering cfg)

multicore :: Config -> Bool
multicore cfg = cores cfg /= Just 1

queryFile :: Ext -> Config -> FilePath
queryFile e cfg = extFileNameR' (saveDir cfg) e (srcFile cfg)
