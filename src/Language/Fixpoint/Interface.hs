-- | This module implements the top-level API for interfacing with Fixpoint
--   In particular it exports the functions that solve constraints supplied
--   either as .fq files or as FInfo.
{-# LANGUAGE CPP #-}

module Language.Fixpoint.Interface (

    -- * Containing Constraints
    FInfo (..)

    -- * Invoke Solver on an FInfo
  , solve

    -- * Invoke Solver on a .fq file
  , solveFQ

    -- * Function to determine outcome
  , resultExit

    -- * Parse Qualifiers from File
  , parseFInfo
) where

#if __GLASGOW_HASKELL__ < 710
import           Data.Functor ((<$>))
import           Data.Monoid (mconcat, mempty)
#endif


import qualified Data.HashMap.Strict                as M
import           Data.List                          hiding (partition)
import           System.Exit                        (ExitCode (..))
import           System.Console.CmdArgs.Verbosity   hiding (Loud)
import           Text.PrettyPrint.HughesPJ          (render)
import           Text.Printf                        (printf)
import           Control.Monad                      (when)

import           Language.Fixpoint.Solver.Validate  (validate)
import           Language.Fixpoint.Solver.Eliminate (eliminateAll)
import           Language.Fixpoint.Solver.Deps      (deps, Deps (..))
import           Language.Fixpoint.Solver.Uniqify   (renameAll)
import qualified Language.Fixpoint.Solver.Solve     as S
import           Language.Fixpoint.Config           (Config (..), command, withTarget)
import           Language.Fixpoint.Files            hiding (Result)
import           Language.Fixpoint.Misc
import           Language.Fixpoint.Statistics       (statistics)
import           Language.Fixpoint.Partition        (partition, partition')
import           Language.Fixpoint.Parse            (rr, rr')
import           Language.Fixpoint.Types
import           Language.Fixpoint.Errors           (exit)
import           Language.Fixpoint.PrettyPrint      (showpp)
import           Language.Fixpoint.Parallel         (inParallelUsing)

---------------------------------------------------------------------------
-- | Solve .fq File -------------------------------------------------------
---------------------------------------------------------------------------
solveFQ :: Config -> IO ExitCode
---------------------------------------------------------------------------
solveFQ cfg
  | native cfg    = solveWith cfg (solve    cfg)
  | multicore cfg = solveWith cfg (solvePar cfg)
  | otherwise     = solveFile cfg

multicore :: Config -> Bool
multicore cfg = cores cfg /= Just 1

---------------------------------------------------------------------------
-- | Solve FInfo system of horn-clause constraints ------------------------
---------------------------------------------------------------------------
solve :: (Fixpoint a) => Config -> FInfo a -> IO (Result a)
solve cfg
  | parts cfg     = partition cfg
  | stats cfg     = statistics cfg
  | native cfg    = solveNativeWithFInfo cfg
  | multicore cfg = solvePar cfg
  | otherwise     = solveExt cfg

---------------------------------------------------------------------------
-- | Native Haskell Solver
---------------------------------------------------------------------------
solveWith :: Config -> (FInfo () -> IO (Result ())) -> IO ExitCode
solveWith cfg s = exit (ExitFailure 2) $ do
  let file  = inFile cfg
  str      <- readFile file
  let fi    = rr' file str :: FInfo ()
  let fi'   = fi { fileName = file }
  res      <- s fi'
  return    $ resultExit (resStatus res)

solveNativeWithFInfo :: (Fixpoint a) => Config -> FInfo a -> IO (Result a)
solveNativeWithFInfo cfg fi = do
  writeLoud $ "fq file in: \n" ++ render (toFixpoint cfg fi)
  donePhase Loud "Read Constraints"
  --FIXME: inefficient since toFixpoint and rr are mostly inverses - better to
  -- replace this by the net effect of rr . toFixpoint,
  -- and the correct solution is to make toFixpoint and rr actually inverses.
  let fi' = rr $ render $ toFixpoint cfg fi :: FInfo ()
  let si = convertFormat fi'
  writeLoud $ "fq file after format convert: \n" ++ render (toFixpoint cfg si)
  donePhase Loud "Format Conversion"
  let Right si' = validate cfg si
  writeLoud $ "fq file after validate: \n" ++ render (toFixpoint cfg si')
  donePhase Loud "Validated Constraints"
  when (elimStats cfg) $ printElimStats (deps si')
  let si''  = renameAll si'
  writeLoud $ "fq file after uniqify: \n" ++ render (toFixpoint cfg si'')
  donePhase Loud "Uniqify"
  si'''     <- elim cfg si''
  Result stat soln <- S.solve cfg si'''
  donePhase Loud "Solve"
  let stat' = sid <$> stat
  putStrLn  $ "Solution:\n"  ++ showpp soln
  -- render (pprintKVs $ hashMapToAscList soln) -- showpp soln
  colorStrLn (colorResult stat') (show stat')
  return    $ Result (WrapC . (\i -> mlookup (cm fi) (mfromJust "WAT" i)) <$> stat') soln

printElimStats :: Deps -> IO ()
printElimStats d = do
  let postElims = length $ depCuts d
  let total = postElims + (length $ depNonCuts d)
  putStrLn $ "TOTAL KVars: " ++ show total
          ++ "\nPOST-ELIMINATION KVars: " ++ show postElims

elim :: (Fixpoint a) => Config -> SInfo a -> IO (SInfo a)
elim cfg fi
  | eliminate cfg = do let fi' = eliminateAll fi
                       writeLoud $ "fq file after eliminate: \n" ++ render (toFixpoint cfg fi')
                       donePhase Loud "Eliminate"
                       return fi'
  | otherwise     = return fi

---------------------------------------------------------------------------
-- | External Ocaml Solver
---------------------------------------------------------------------------
solveExt :: (Fixpoint a) => Config -> FInfo a -> IO (Result a)
solveExt cfg fi =   {-# SCC "Solve"  #-} execFq cfg fn fi
                >>= {-# SCC "exitFq" #-} exitFq fn (cm fi)
  where
    fn          = fileName fi -- srcFile cfg

-- | Partitions an FInfo into 1 or more independent parts, then
--   calls solveExt on each in parallel
solvePar :: (Fixpoint a) => Config -> FInfo a -> IO (Result a)
solvePar c fi = do
   mci <- mcInfo c
   let (_, fis) = partition' (Just mci) fi
   writeLoud $ "Number of partitions: " ++ show (length fis)
   writeLoud $ "number of cores: " ++ show (cores c)
   writeLoud $ "minimum part size: " ++ show (minPartSize c)
   writeLoud $ "maximum part size: " ++ show (maxPartSize c)
   case fis of
      [] -> errorstar "partiton' returned empty list!"
      [onePart] -> solveExt c onePart
      _ -> inParallelUsing fis (solveExt c)

execFq :: (Fixpoint a) => Config -> FilePath -> FInfo a -> IO ExitCode
execFq cfg fn fi
  = do writeFile fq $ render $ {-# SCC "FixPointify" #-} toFixpoint cfg fi
       solveFile $ cfg `withTarget` fq
    where
       fq   = extFileName Fq fn

solveFile :: Config -> IO ExitCode
solveFile cfg
  = do fp  <- getFixpointPath
       z3  <- getZ3LibPath
       v   <- (\b -> if b then "-v 1" else "") <$> isLoud
       {-# SCC "sysCall:Fixpoint" #-} executeShellCommand "fixpoint" $ fixCommand fp z3 v
    where
      fixCommand fp z3 verbosity
        = printf "LD_LIBRARY_PATH=%s %s %s %s -notruekvars -refinesort -nosimple -strictsortcheck -sortedquals %s %s"
          z3 fp verbosity rf newcheckf (command cfg) 
        where
          rf  = if real cfg then realFlags else ""
          newcheckf = if newcheck cfg then "-newcheck" else ""

realFlags :: String
realFlags =  "-no-uif-multiply "
          ++ "-no-uif-divide "


exitFq :: (Fixpoint a) => FilePath -> M.HashMap Integer (SubC a) -> ExitCode -> IO (Result a)
exitFq _ _ (ExitFailure n) | n /= 1
  = return $ Result (Crash [] "Unknown Error") M.empty
exitFq fn z _
  = do str <- {-# SCC "readOut" #-} readFile (extFileName Out fn)
       let (x, y) = parseFixpointOutput str
       let x'     = fmap (mlookup z) x
       return     $ Result (WrapC <$> x') y

parseFixpointOutput :: String -> (FixResult Integer, FixSolution)
parseFixpointOutput str = {-# SCC "parseFixOut" #-} rr ({-# SCC "sanitizeFixpointOutput" #-} sanitizeFixpointOutput str)

sanitizeFixpointOutput :: String -> String
sanitizeFixpointOutput
  = unlines
  . filter (not . ("//"     `isPrefixOf`))
  . chopAfter ("//QUALIFIERS" `isPrefixOf`)
  . lines

chopAfter ::  (a -> Bool) -> [a] -> [a]
chopAfter f xs
  = case findIndex f xs of
      Just n  -> take n xs
      Nothing -> xs

---------------------------------------------------------------------------
resultExit :: FixResult a -> ExitCode
---------------------------------------------------------------------------
resultExit Safe        = ExitSuccess
resultExit (Unsafe _)  = ExitFailure 1
resultExit _           = ExitFailure 2


---------------------------------------------------------------------------
-- | Parse External Qualifiers --------------------------------------------
---------------------------------------------------------------------------
parseFInfo :: [FilePath] -> IO (FInfo a) -- [Qualifier]
---------------------------------------------------------------------------
parseFInfo fs = mconcat <$> mapM parseFI fs

parseFI :: FilePath -> IO (FInfo a) --[Qualifier]
parseFI f = do
  str   <- readFile f
  let fi = rr' f str :: FInfo ()
  return $ mempty { quals = quals  fi
                  , lits  = lits   fi }
