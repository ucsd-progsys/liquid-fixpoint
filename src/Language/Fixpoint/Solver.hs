-- | This module implements the top-level API for interfacing with Fixpoint
--   In particular it exports the functions that solve constraints supplied
--   either as .fq files or as FInfo.
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fixpoint.Solver (
    -- * Invoke Solver on an FInfo
    solve

    -- * Invoke Solver on a .fq file
  , solveFQ

    -- * Function to determine outcome
  , resultExit
  , resultExitCode

    -- * Parse Qualifiers from File
  , parseFInfo

    -- * Simplified Info
  , simplifyFInfo
) where

import           Control.Concurrent                 (setNumCapabilities)
import qualified Data.HashMap.Strict              as HashMap
import qualified Data.Store                       as S
import           Data.Aeson                         (ToJSON, encode)
import qualified Data.Text.Lazy.IO                as LT
import qualified Data.Text.Lazy.Encoding          as LT
import           System.Exit                        (ExitCode (..))
import           System.Console.CmdArgs.Verbosity   (whenNormal, whenLoud)
import           Control.Monad                      (when)
import           Control.Exception                  (SomeException, catch)
import           Control.Exception.Compat
    (ExceptionWithContext(..), displayExceptionContext, wrapExceptionWithContext)
import           Language.Fixpoint.Solver.EnvironmentReduction
  (reduceEnvironments, simplifyBindings)
import           Language.Fixpoint.Solver.Sanitize  (symbolEnv, sanitize)
import           Language.Fixpoint.Solver.UniqifyBinds (renameAll)
import           Language.Fixpoint.Defunctionalize (defunctionalize)
import           Language.Fixpoint.SortCheck            (ElabParam (..), Elaborate (..), unElab, unElabFSetBagZ3)
import           Language.Fixpoint.Solver.Extensionality (expand)
import           Language.Fixpoint.Solver.Prettify (savePrettifiedQuery)
import           Language.Fixpoint.Solver.UniqifyKVars (wfcUniqify)
import qualified Language.Fixpoint.Solver.Solve     as Sol
import qualified Language.Fixpoint.Solver.Solution  as Sol
import           Language.Fixpoint.Types.Config
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Utils.Files            hiding (Result)
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Utils.Statistics (statistics)
import           Language.Fixpoint.Graph
import           Language.Fixpoint.Parse            (rr')
import           Language.Fixpoint.Types hiding (GInfo(..), fi)
import qualified Language.Fixpoint.Types as Types (GInfo(..))
import           Language.Fixpoint.Minimize (minQuery, minQuals, minKvars)
import           Control.DeepSeq
import qualified Data.ByteString as B
import Data.Maybe (catMaybes)
import qualified Text.PrettyPrint.HughesPJ as PJ

---------------------------------------------------------------------------
-- | Solve an .fq file ----------------------------------------------------
---------------------------------------------------------------------------
solveFQ :: Config -> IO ExitCode
solveFQ cfg = do
    (fi, opts) <- readFInfo file
    cfg'       <- withPragmas cfg opts
    let fi'     = ignoreQualifiers cfg' fi
    r          <- solve cfg' fi'
    resultExitCode cfg (fst <$> r)
  where
    file    = srcFile      cfg

---------------------------------------------------------------------------
resultExitCode :: (Fixpoint a, NFData a, ToJSON a) => Config -> Result a
               -> IO ExitCode
---------------------------------------------------------------------------
resultExitCode cfg r = do
  whenNormal $ colorStrLn (colorResult stat) (statStr $!! stat)
  when (json cfg) $ LT.putStrLn jStr
  return (eCode r)
  where
    jStr    = LT.decodeUtf8 . encode $ r
    stat    = resStatus $!! r
    eCode   = resultExit . resStatus
    statStr = PJ.render . resultDoc

ignoreQualifiers :: Config -> FInfo a -> FInfo a
ignoreQualifiers cfg fi
  | eliminate cfg == All = fi { Types.quals = [] }
  | otherwise            = fi


--------------------------------------------------------------------------------
-- | Solve FInfo system of horn-clause constraints -----------------------------
--------------------------------------------------------------------------------
solve
  :: (PPrint a, NFData a, Fixpoint a, Show a, Loc a)
  => Config -> FInfo a -> IO (Result (Integer, a))
--------------------------------------------------------------------------------
solve cfg q
  | parts cfg      = partition  cfg        $!! q
  | stats cfg      = statistics cfg        $!! q
  | minimize cfg   = minQuery   cfg solve' $!! q
  | minimizeQs cfg = minQuals cfg solve'   $!! q
  | minimizeKs cfg = minKvars cfg solve'   $!! q
  | otherwise      = solve'     cfg        $!! q


solve'
  :: (PPrint a, NFData a, Fixpoint a, Show a, Loc a)
  => Config -> FInfo a -> IO (Result (Integer, a))
solve' cfg q = do
  when (save cfg) $ saveQuery   cfg q
  if multicore cfg then
    solvePar cfg q
  else
    solveNative cfg (slice cfg q)

--------------------------------------------------------------------------------
readFInfo :: FilePath -> IO (FInfo (), [String])
--------------------------------------------------------------------------------
readFInfo f
  | isBinary f = (,) <$> readBinFq f <*> return []
  | otherwise  = readFq f

readFq :: FilePath -> IO (FInfo (), [String])
readFq file = do
  str   <- readFile file
  let q  = {- SCC "parsefq" -} rr' file str :: FInfoWithOpts ()
  return (fioFI q, fioOpts q)

readBinFq :: FilePath -> IO (FInfo ())
readBinFq file = {-# SCC "parseBFq" #-} do
  bs <- B.readFile file
  case S.decode bs of
    Right fi -> return fi
    Left err' -> error ("Error decoding .bfq: " ++ show err')

--------------------------------------------------------------------------------
-- | Solve in parallel after partitioning an FInfo to indepdendant parts
--------------------------------------------------------------------------------
solvePar
  :: (Loc a, NFData a, PPrint a, Show a, Fixpoint a)
  => Config -> FInfo a -> IO (Result (Integer, a))
--------------------------------------------------------------------------------
solvePar c fi0 = do
  -- putStrLn "Using Parallel Solver \n"
  let fi    = slice c fi0
  mci      <- mcInfo c
  let fis   = partition' (Just mci) fi
  writeLoud $ "Number of partitions : " ++ show (length fis)
  writeLoud $ "number of cores      : " ++ show (cores c)
  writeLoud $ "minimum part size    : " ++ show (minPartSize c)
  writeLoud $ "maximum part size    : " ++ show (maxPartSize c)
  case fis of
    []        -> errorstar "partiton' returned empty list!"
    [onePart] -> solveNative c onePart
    _         -> inParallelUsing (f c) $ zip [1..] fis
    where
      f c' (j, fi) = solveNative (c {srcFile = queryFile (Part j) c'}) fi

--------------------------------------------------------------------------------
-- | Solve a list of FInfos using the provided solver function in parallel
--------------------------------------------------------------------------------
inParallelUsing :: (a -> IO (Result b)) -> [a] -> IO (Result b)
--------------------------------------------------------------------------------
inParallelUsing f xs = do
   setNumCapabilities (length xs)
   rs <- asyncMapM f xs
   return $ mconcat rs


--------------------------------------------------------------------------------
-- | Native Haskell Solver -----------------------------------------------------
--------------------------------------------------------------------------------
solveNative, solveNative'
  :: (NFData a, Fixpoint a, Show a, Loc a, PPrint a)
  => Config -> FInfo a -> IO (Result (Integer, a))
--------------------------------------------------------------------------------
solveNative !cfg !fi0 = solveNative' cfg fi0
                          `catch`
                             (return . crashResult (errorMap fi0) . wrapExceptionWithContext)
                          `catch`
                             (return . crashResultOther . wrapExceptionWithContext)

crashResult :: (PPrint a) => ErrorMap a -> ExceptionWithContext Error -> Result (Integer, a)
crashResult m (ExceptionWithContext ectx ex) = Result res mempty mempty mempty
  where
    res = Crash es msg
    es  = catMaybes [ findError m e | e <- ers ]
    ers = errs ex
    msg = displayExceptionContext ectx ++ "\n" ++ msg0
    msg0 | null ers = "Sorry, unexpected panic in liquid-fixpoint!\n"
                       ++ showpp ex
         | otherwise = showpp ex

crashResultOther
  :: ExceptionWithContext SomeException -> Result (Integer, a)
crashResultOther (ExceptionWithContext ectx ex) =
    Result res mempty mempty mempty
  where
    res = Crash [] msg
    msg = displayExceptionContext ectx ++ "\n" ++ msg0
    msg0 = "Sorry, unexpected panic in liquid-fixpoint!\n" ++ show ex

-- | Unpleasant hack to save meta-data that can be recovered from SrcSpan
type ErrorMap a = HashMap.HashMap SrcSpan a

findError :: ErrorMap a -> Error1 -> Maybe ((Integer, a), Maybe String)
findError m e = do
  ann <- HashMap.lookup (errLoc e) m
  let str = PJ.render (errMsg e)
  return ((-1, ann), Just str)

-- The order is important here: we want the "binders" to get the "precedence"
errorMap :: (Loc a) => FInfo a -> ErrorMap a
errorMap fi = HashMap.fromList [ (srcSpan a, a) | a <- anns ]
  where
    anns    =  [ sinfo c | (_, c) <- HashMap.toList (Types.cm fi) ]
            ++ [ winfo w | (_, w) <- HashMap.toList (Types.ws fi) ]
            ++ [ a | (_, (_,_, a)) <- bindEnvToList (Types.bs fi) ]

loudDump :: (Fixpoint a) => Int -> Config -> SInfo a -> IO ()
loudDump i cfg si = when False (writeLoud $ msg ++ PJ.render (toFixpoint cfg si))
  where
    msg           = "fq file after Uniqify & Rename " ++ show i ++ "\n"

{-# SCC simplifyFInfo #-}
simplifyFInfo :: (NFData a, Fixpoint a, Show a, Loc a)
               => Config -> FInfo a -> IO (ElabParam, SInfo a)
simplifyFInfo !cfg !fi0 = do
  -- writeLoud $ "fq file in: \n" ++ render (toFixpoint cfg fi)
  -- rnf fi0 `seq` donePhase Loud "Read Constraints"
  -- let qs   = quals fi0
  -- whenLoud $ print qs
  -- whenLoud $ putStrLn $ showFix (quals fi1)
  reducedFi <- reduceFInfo cfg fi0
  let fi1   = reducedFi { Types.quals = remakeQual <$> Types.quals reducedFi }
  let si0   = {- SCC "convertFormat" -} convertFormat fi1
  -- writeLoud $ "fq file after format convert: \n" ++ render (toFixpoint cfg si0)
  -- rnf si0 `seq` donePhase Loud "Format Conversion"
  let si1   = either die id ({- SCC "sanitize" -} sanitize cfg $!! si0)
  -- writeLoud $ "fq file after sanitize: \n" ++ render (toFixpoint cfg si1)
  -- rnf si1 `seq` donePhase Loud "Validated Constraints"
  graphStatistics cfg si1
  let si2  = {- SCC "wfcUniqify" -} wfcUniqify $!! si1
  -- writeLoud $ "fq file after wfcUniqify: \n" ++ render (toFixpoint cfg si2)
  let si3  = {- SCC "renameAll"  -} renameAll  $!! si2
  rnf si3 `seq` whenLoud $ donePhase Loud "Uniqify & Rename"
  loudDump 1 cfg si3
  let si4  = {- SCC "defunction" -} defunctionalize cfg $!! si3
  -- writeLoud $ "fq file after defunc: \n" ++ render (toFixpoint cfg si4)
  -- putStrLn $ "AXIOMS: " ++ showpp (asserts si4)
  loudDump 2 cfg si4
  let ef = solverFlags cfg
      elabParam = ElabParam
                     ef
                     (atLoc dummySpan "solver")
                     (coerceEnv ef (symbolEnv cfg si4))
      si5  = elaborate elabParam si4
  -- writeLoud $ "fq file after elaborate: \n" ++ render (toFixpoint cfg si5)
  loudDump 3 cfg si5
  let si6 = if extensionality cfg then {- SCC "expand" -} expand cfg si5 else si5
  return (elabParam, si6){- SCC "elaborate" -}

reduceFInfo :: Fixpoint a => Config -> FInfo a -> IO (FInfo a)
reduceFInfo cfg fi = do
  let simplifiedFi = {- SCC "simplifyFInfo" -} simplifyBindings cfg fi
      reducedFi = {- SCC "reduceEnvironments" -} reduceEnvironments simplifiedFi
  when (save cfg) $
    savePrettifiedQuery cfg reducedFi
  if noEnvReduction cfg then
    return fi
  else
    return reducedFi

solveNative' !cfg !fi0 = do
  (elabParam, si6) <- simplifyFInfo cfg fi0
  res0 <- {- SCC "Sol.solve" -} Sol.solve cfg elabParam $!! si6
  let res = simplifyResult cfg res0
  -- rnf soln `seq` donePhase Loud "Solve2"
  --let stat = resStatus res
  -- saveSolution cfg res
  when (save cfg) $ saveSolution cfg res
  -- writeLoud $ "\nSolution:\n"  ++ showpp (resSolution res)
  -- colorStrLn (colorResult stat) (show stat)
  return res

--------------------------------------------------------------------------------
-- | Parse External Qualifiers -------------------------------------------------
--------------------------------------------------------------------------------
parseFInfo :: [FilePath] -> IO (FInfo a)
--------------------------------------------------------------------------------
parseFInfo fs = mconcat <$> mapM parseFI fs

parseFI :: FilePath -> IO (FInfo a)
parseFI f = do
  str   <- readFile f
  let fi = rr' f str :: FInfo ()
  return $ mempty { Types.quals = Types.quals  fi
                  , Types.gLits = Types.gLits  fi
                  , Types.dLits = Types.dLits  fi }

saveSolution :: Config -> Result a -> IO ()
saveSolution cfg res = when (save cfg) $ do
  let f = queryFile Out cfg
  putStrLn $ "Saving Solution: " ++ f ++ "\n"
  ensurePath f
  writeFile f $ unlines $
    [ ""
    , "Solution:"
    , scopedRender (resSolution  res)
    ] ++
    [ ""
    , ""
    , "Non-cut kvars:"
    , ""
    , scopedRender (HashMap.map forceDelayed $ resNonCutsSolution res)
    ]
    where
      scopedRender = PJ.render . PJ.vcat . map ncDoc . scoped
      scoped sol = [ (k, scope k, e) | (k, e) <- HashMap.toList sol]
      scope k = HashMap.lookupDefault [] k $ resSorts res
      ncDoc (k, xts, e) = PJ.hsep [ pprint k PJ.<> pprint xts, ":=", pprint e ]

simplifyResult :: Config -> Result a -> Result a
simplifyResult cfg res =
    res
      { resSolution = HashMap.map simplifyKVar' (resSolution res)
      , resNonCutsSolution = HashMap.map (fmap simplifyKVar') (resNonCutsSolution res)
      }
  where
    simplifyKVar' = unElabSets . unElab . Sol.simplifyKVar
    sets          = elabSetBag . solverFlags $ cfg
    unElabSets    = if sets then unElabFSetBagZ3 else id
