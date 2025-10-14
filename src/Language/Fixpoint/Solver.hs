-- | This module implements the top-level API for interfacing with Fixpoint
--   In particular it exports the functions that solve constraints supplied
--   either as .fq files or as FInfo.
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Fixpoint.Solver (
    -- * Invoke Solver on an FInfo
    solve, Solver

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
import qualified Data.List as L
import qualified Data.Text.Lazy.IO                as LT
import qualified Data.Text.Lazy.Encoding          as LT
import           System.Exit                        (ExitCode (..))
import           System.Console.CmdArgs.Verbosity   (whenNormal, whenLoud)
import           Control.Monad                      (mplus, when)
import           Control.Exception                  (catch)
import           Language.Fixpoint.Solver.EnvironmentReduction
  (reduceEnvironments, simplifyBindings)
import           Language.Fixpoint.Solver.Sanitize  (symbolEnv, sanitize)
import           Language.Fixpoint.Solver.UniqifyBinds (renameAll)
import           Language.Fixpoint.Defunctionalize (defunctionalize)
import           Language.Fixpoint.SortCheck            (ElabParam (..), Elaborate (..), unElab)
import           Language.Fixpoint.Solver.Extensionality (expand)
import           Language.Fixpoint.Solver.Prettify (savePrettifiedQuery)
import           Language.Fixpoint.Solver.UniqifyKVars (wfcUniqify)
import qualified Language.Fixpoint.Solver.Solve     as Sol
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
import           Language.Fixpoint.Solver.Instantiate (instantiate)
import           Control.DeepSeq
import qualified Data.ByteString as B
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
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
solve :: (PPrint a, NFData a, Fixpoint a, Show a, Loc a) => Solver a
--------------------------------------------------------------------------------
solve cfg q
  | parts cfg      = partition  cfg        $!! q
  | stats cfg      = statistics cfg        $!! q
  | minimize cfg   = minQuery   cfg solve' $!! q
  | minimizeQs cfg = minQuals cfg solve'   $!! q
  | minimizeKs cfg = minKvars cfg solve'   $!! q
  | otherwise      = solve'     cfg        $!! q

solve' :: (PPrint a, NFData a, Fixpoint a, Show a, Loc a) => Solver a
solve' cfg q = do
  when (save cfg) $ saveQuery   cfg q
  configSW  cfg     solveNative cfg q

configSW :: (NFData a, Fixpoint a, Show a, Loc a) => Config -> Solver a -> Solver a
configSW cfg
  | multicore cfg = solveParWith
  | otherwise     = solveSeqWith

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
solveSeqWith :: (Fixpoint a) => Solver a -> Solver a
solveSeqWith s c fi0 = {- withProgressFI fi $ -} s c fi
  where
    fi               = slice c fi0

--------------------------------------------------------------------------------
-- | Solve in parallel after partitioning an FInfo to indepdendant parts
--------------------------------------------------------------------------------
solveParWith :: (Fixpoint a) => Solver a -> Solver a
--------------------------------------------------------------------------------
solveParWith s c fi0 = do
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
    [onePart] -> s c onePart
    _         -> inParallelUsing (f s c) $ zip [1..] fis
    where
      f s' c' (j, fi) = s' (c {srcFile = queryFile (Part j) c'}) fi

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
solveNative, solveNative' :: (NFData a, Fixpoint a, Show a, Loc a, PPrint a) => Solver a
--------------------------------------------------------------------------------
solveNative !cfg !fi0 = solveNative' cfg fi0
                          `catch`
                             (return . crashResult (errorMap fi0))

crashResult :: (PPrint a) => ErrorMap a -> Error -> Result (Integer, a)
crashResult m err' = Result res mempty mempty mempty mempty
  where
    res           = Crash es msg
    es            = catMaybes [ findError m e | e <- ers ]
    ers           = errs err'
    msg | null ers = "Sorry, unexpected panic in liquid-fixpoint!"
        --  {-dbgFalse-} True  = "Sorry, unexpected panic in liquid-fixpoint!\n" ++ crashMessage es
        | otherwise = showpp err'

_crashMessage :: [((Integer, a), Maybe String) ] -> String
_crashMessage es = L.intercalate "\n" [ msg i s | ((i,_), Just s) <- es ]
  where
    msg i s = "Error in constraint " ++ show i ++ ":\n" ++ s

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
               => Config -> FInfo a -> IO (SInfo a)
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
  let si5  = {- SCC "elaborate" -} elaborate (ElabParam (solverFlags $ solver cfg) (atLoc dummySpan "solver") (symbolEnv cfg si4)) si4
  -- writeLoud $ "fq file after elaborate: \n" ++ render (toFixpoint cfg si5)
  loudDump 3 cfg si5
  let si6 = if extensionality cfg then {- SCC "expand" -} expand cfg si5 else si5
  if rewriteAxioms cfg && noLazyPLE cfg
    then instantiate cfg si6 $!! Nothing
    else return si6

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
  si6 <- simplifyFInfo cfg fi0
  res0 <- {- SCC "Sol.solve" -} Sol.solve cfg $!! si6
  let res = simplifyResult res0
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
    , showpp (resSolution  res)
    ] ++
    ( if gradual cfg then
        ["", "", showpp $ gresSolution res]
      else
        []
    ) ++
    [ ""
    , ""
    , "Non-cut kvars:"
    , ""
    , PJ.render nonCutsDoc
    ]
    where
      nonCutsDoc = PJ.vcat (ncDoc <$> nonCuts')
      nonCuts = HashMap.toList ({- HashMap.map unElab $  -} resNonCutsSolution res)
      nonCuts' = [ (k, scope k, e) | (k, e) <- nonCuts]
      scope k = case HashMap.lookup k (resSorts res) of
                  Just xts -> L.sortBy (comparing fst) xts
                  Nothing -> []
      ncDoc :: (KVar, [(Symbol, Sort)], Expr) -> PJ.Doc
      ncDoc (k, xts, e) = PJ.hsep [ pprint k PJ.<> PJ.parens (pprint xts), ":=", pprint e ]

simplifyResult :: Result a -> Result a
simplifyResult res =
    res
      { resSolution = HashMap.mapWithKey (simplifyKVar' sorts) (resSolution res)
      , resNonCutsSolution = HashMap.mapWithKey (simplifyKVar' sorts) (resNonCutsSolution res)
      }
  where
    sorts = resSorts res

simplifyKVar' :: HashMap.HashMap KVar [(Symbol, Sort)] -> KVar -> Expr -> Expr
simplifyKVar' scope k e = unElab $ simplifyKVar xs e
  where
    xs = fst <$> HashMap.lookupDefault [] k scope

-- | Simplifies existential expressions with unused or inconsequential bindings.
--
-- For instance, in the following example, "x" is not used at all.
--
-- > simplifyKVar "exists x y. y == z && y == C" == "exists y. y == z && y == C"
--
-- And in the following example, @x@ is used but in a way that doesn't
-- contribute any useful knowledge.
--
-- > simplifyKVar "exists x y. x == C && y == z && y == C"
-- >   ==
-- > "exists y. y == z && y == C"
--
-- We require that relevant variables occur more than once, or that
-- they occur in some other place than as an argument to @==@.
-- However, we do not want to eliminate any equalities over "free" variables, e.g.
-- the parameters of the KVar, as those are important to keep.
simplifyKVar :: [Symbol] -> Expr -> Expr
simplifyKVar freeVars = go
  where
    go (POr es) = POr $ map go es
    go (PExist bs e@(PAnd es)) =
      let fvs = L.group $ L.sort $ freeVars ++ collectFreeVarOccurrences e
          esv = map (isUniqueEq fvs) es
          removed = mapMaybe fst esv
          needed = map head fvs L.\\ removed
          bs' = filter ((`elem` needed) . fst) bs
      in
          PExist bs' $ PAnd $ [ei | (Nothing, ei) <- esv]
    go e = e
-- | Determine if the expression is an equality that sets the value of
-- a variable that doesn't occur elsewhere.
--
-- In @isUniqueEq fvs e@, @fvs@ contains the occurrences of the free
-- variables, so we can infer if there is more than one occurrence
-- of a given free variable, and @e@ is the equality to analyze.
--
-- Yields @(Just v, e)@ if @v@ doesn't occur elsewhere, and @e@ has
-- the form @v == e'@.
isUniqueEq :: [[Symbol]] -> Expr -> (Maybe Symbol, Expr)
isUniqueEq fvs er = case unElab er of
  PAtom brel e0 e1
    | isEqRel brel ->
      let m = isVarToDrop fvs e0 `mplus` isVarToDrop fvs e1
       in (m, er)
  _ ->
    (Nothing, er)

-- | Tells if the binary relation is an equality.
isEqRel :: Brel -> Bool
isEqRel Eq = True
isEqRel Ueq = True
isEqRel _ = False

-- | @isVarToDrop fvs s@ yields @Just s@ if the variable @s@ doesn't occur
-- elsewhere according to @fvs@.
--
-- > isVarToDrop fvs (cast_as_int s) == isVarToDrop fvs s
--
isVarToDrop ::  [[Symbol]] -> ExprV Symbol -> Maybe Symbol
isVarToDrop fvs (EApp (EVar "cast_as_int") ei) = isVarToDrop fvs ei
isVarToDrop fvs (EVar s)
  | elem [s] fvs = Just s
isVarToDrop _fvs _ = Nothing

-- | Produces the free variables of an expressions as many times as they occur.
--
-- There are no guarantees on the order in which the variables are produced. For
-- instance,
--
-- > collectFreeVarOccurrences "z (y x) (y x)" == ["z", "y", "x", "y", "x"]
--
collectFreeVarOccurrences :: Expr -> [Symbol]
collectFreeVarOccurrences = go []
  where
    go acc e0 = case e0 of
      ESym _ -> acc
      ECon _ -> acc
      EVar v -> v : acc
      PKVar _ (Su m) -> foldr (flip go) acc $ HashMap.elems m
      PGrad _ (Su m) _ e -> foldr (flip go) acc $ e : HashMap.elems m
      ENeg e -> go acc e
      PNot p -> go acc p
      ECst e _t -> go acc e
      PAll _xts p -> go acc p
      ELam (b, _) e -> go acc e L.\\ [b]
      ECoerc _a _t e -> go acc e
      PExist _xts p -> go acc p
      ETApp e _s -> go acc e
      ETAbs e _s -> go acc e
      EApp g e -> go (go acc e) g
      EBin _o e1 e2 -> go (go acc e2) e1
      PImp p1 p2 -> go (go acc p2) p1
      PIff p1 p2 -> go (go acc p2) p1
      PAtom _r e1 e2 -> go (go acc e2) e1
      ELet x e1 e2 -> go (go acc e2 L.\\ [x]) e1
      EIte p e1 e2 -> go (go (go acc e2) e1) p
      PAnd ps -> foldr (flip go) acc ps
      POr ps -> foldr (flip go) acc ps
