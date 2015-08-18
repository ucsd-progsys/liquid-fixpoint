-- | This module implements the top-level API for interfacing with Fixpoint
--   In particular it exports the functions that solve constraints supplied
--   either as .fq files or as FInfo.
{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

import qualified Data.HashMap.Strict              as M
import           Data.Hashable
import           Data.List hiding (partition)
import           Data.Maybe

#if __GLASGOW_HASKELL__ < 710
import           Data.Functor
import           Data.Monoid (mconcat, mempty)
#endif


import           System.Exit
import           System.IO                        (IOMode (..), hPutStr, withFile)
import           Text.Printf

import           Language.Fixpoint.Solver.Eliminate (eliminateAll)
import           Language.Fixpoint.Solver.Uniqify   (renameAll)
import           Language.Fixpoint.Solver.Deps
import qualified Language.Fixpoint.Solver.Solve  as S
import           Language.Fixpoint.Config          hiding (solver)
import           Language.Fixpoint.Files           hiding (Result)
import           Language.Fixpoint.Misc
-- import           Language.Fixpoint.Solver.TrivialSort     (nontrivsorts)
import           Language.Fixpoint.Statistics     (statistics)
import           Language.Fixpoint.Partition      (partition)
import           Language.Fixpoint.Parse          (rr, rr')
import           Language.Fixpoint.Types          hiding (kuts, lits)
import           Language.Fixpoint.Errors (exit)
import           Language.Fixpoint.PrettyPrint (showpp)
import           Language.Fixpoint.Names (renameSymbol)
import           Language.Fixpoint.Visitor (lhsKVars, rhsKVars)
import           System.Console.CmdArgs.Verbosity hiding (Loud)
import           Text.PrettyPrint.HughesPJ
import           Control.Monad
import           Control.Arrow
import           GHC.Exts (groupWith)

---------------------------------------------------------------------------
-- | Solve .fq File -------------------------------------------------------
---------------------------------------------------------------------------
solveFQ :: Config -> IO ExitCode
---------------------------------------------------------------------------
solveFQ cfg
  | native cfg = solveNative cfg (solve cfg)
  | otherwise  = solveFile   cfg

---------------------------------------------------------------------------
-- | Solve FInfo system of horn-clause constraints ------------------------
---------------------------------------------------------------------------

solve :: (Fixpoint a) => Config -> FInfo a -> IO (Result a)
solve cfg
  -- -- | nontriv cfg = nontrivsorts cfg
  | parts cfg   = partition    cfg
  | stats cfg   = statistics   cfg
  | native cfg  = solveNativeWithFInfo cfg
  | otherwise   = solveExt   cfg

---------------------------------------------------------------------------
-- | Native Haskell Solver
---------------------------------------------------------------------------
solveNative :: Config -> (FInfo () -> IO (Result ())) -> IO ExitCode
solveNative cfg s = exit (ExitFailure 2) $ do
  let file  = inFile cfg
  str      <- readFile file
  let fi    = rr' file str :: FInfo ()
  res      <- s fi
  return    $ resultExit (resStatus res)

solveNativeWithFInfo :: (Fixpoint a) => Config -> FInfo a -> IO (Result a)
solveNativeWithFInfo cfg fi = do
  whenLoud  $ putStrLn $ "fq file in: \n" ++ render (toFixpoint cfg fi)
  donePhase Loud "Read Constraints"
  let fi'   = renameAll fi
  whenLoud  $ putStrLn $ "fq file after uniqify: \n" ++ render (toFixpoint cfg fi')
  donePhase Loud "Uniqify"
  fi''     <- elim cfg fi'
  _ <- interp cfg fi'
  Result stat soln <- S.solve cfg fi''
  donePhase Loud "Solve"
  let stat' = sid <$> stat
  putStrLn  $ "Solution:\n" ++ showpp soln
  putStrLn  $ "Result: "    ++ show   stat'
  return    $ Result stat soln


elim :: (Fixpoint a) => Config -> FInfo a -> IO (FInfo a)
elim cfg fi
  | eliminate cfg = do let fi' = eliminateAll fi
                       whenLoud $ putStrLn $ "fq file after eliminate: \n" ++ render (toFixpoint cfg fi')
                       donePhase Loud "Eliminate"
                       return fi'
  | otherwise     = return fi


interpSym = symbol "InterpolatedQu"

interp :: (Fixpoint a) => Config -> FInfo a -> IO (FInfo a)
interp cfg fi
  | interpolate cfg = do let fc = failCons cfg
                         let fi' = unroll fi fc
                         whenLoud $ putStrLn $ "fq file after unrolling: \n" ++ render (toFixpoint cfg fi')
                         let fi'' = eliminateAll fi'
                         whenLoud $ putStrLn $ "fq file after unrolled elimination: \n" ++ render (toFixpoint cfg fi'')
                         donePhase Loud "Unroll"
                         q <- buildQual cfg fi'' $ mlookup (cm fi'') (failCons cfg)
                         return fi'' { quals = q:quals fi'' }
  | otherwise     = return fi

buildQual :: Config -> FInfo a -> SubC a -> IO Qualifier
buildQual cfg fi c = qualify <$> S.interpolation cfg fi env p q
  where env  = envCs (bs fi) $ senv c
        qenv = map (second sr_sort) $ predSorts env p
        p = prop $ slhs c
        q = PNot $ prop $ srhs c
        qualify p = Q interpSym qenv p (dummyPos "interp")

predSorts :: [(Symbol,SortedReft)] -> Pred -> [(Symbol,SortedReft)]
predSorts env p = filter ((`elem` ss).fst) env
  where ss = predSymbols p

data Node a b = Node a [Node b a]

unroll :: FInfo a -> Integer -> FInfo a
unroll fi start = fi -- {cm = M.fromList $ extras ++ cons'}
  where m = cm fi
        mlookup v = M.lookupDefault (error $"cons # "++show v++" not found") v m
        kidsm = M.fromList $ (fst.head &&& (snd <$>)) <$> groupWith fst pairs
          where pairs = [(k,i)|(i,ks) <- second rhs <$> M.toList m, k<-ks]
        klookup k = M.lookupDefault (error $"kids for "++show k++" not found") k kidsm

        rhs, lhs :: SubC a -> [KVar]
        rhs = rhsKVars
        lhs = lhsKVars (bs fi)

        cons' = hylo (prune . index (M.empty)) =<< lhs (mlookup start)
        extras = M.toList $ M.filter ((==[]).lhs) m

        hylo f = cata.f.ana
        ana k = Node k [Node v $ ana <$> rhs (mlookup v) | v <- klookup k]
        cata (Node _ bs) = join $ join [[b]:(cata<$>ns) | Node b ns <- bs]

        prune :: Node (KVar, Int) Integer -> Node KVar (Integer, SubC _)
        prune (Node (a,i) l) = Node (renameKv a i) $
          if i>depth
             then []
             else [Node (rename a i v) (fmap prune ns) | Node v ns <- l]

        rename :: KVar -> Int -> Integer -> (Integer, SubC _)
        -- adds `i` primes to the kvar `a`
        -- then subsitutes the new kvar for the old in the SubC #`v`
        -- also gives us a new number for `v`, since it's now a different SubC
        rename a i v = (num v i, undefined) --, subst (mkSubst [(a, renameKv a i)]) (mlookup v))
        num a i = cantor a i $ M.size m

renameKv :: Integral i => KVar -> i -> KVar
renameKv a i = KV $ renameSymbol (kv a) $ fromIntegral i

substKV :: KVar -> KVar -> SubC a
substKV = undefined

cantor :: Integer -> Int -> Int -> Integer
-- The Cantor pairing function, offset by s when i/=0
cantor v i' s' = if i==0
                  then v
                  else s + i + quot ((v+i)*(v+i+1)) 2
  where s = fromIntegral s'
        i = fromIntegral i'

index :: (Eq a, Hashable a) => M.HashMap a Int -> Node a b -> Node (a,Int) b
index m (Node a bs) = Node (a,i) [Node b (index m' <$> ns) | Node b ns <- bs]
  where i = M.lookupDefault 0 a m
        m' = M.insertWith (+) a 1 m

depth :: Int
-- @TODO justify me
depth = 4

---------------------------------------------------------------------------
-- | External Ocaml Solver
---------------------------------------------------------------------------
solveExt :: (Fixpoint a) => Config -> FInfo a -> IO (Result a)
solveExt cfg fi =   {-# SCC "Solve"  #-} execFq cfg fn fi
                >>= {-# SCC "exitFq" #-} exitFq fn (cm fi)
  where
    fn          = srcFile cfg

execFq :: (Fixpoint a) => Config -> FilePath -> FInfo a -> IO ExitCode
execFq cfg fn fi
  = do writeFile fq qstr
       withFile fq AppendMode (\h -> {-# SCC "HPrintDump" #-} hPutStr h (render d))
       solveFile $ cfg `withTarget` fq
    where
       fq   = extFileName Fq fn
       d    = {-# SCC "FixPointify" #-} toFixpoint cfg fi
       qstr = render (vcat (toFix <$> quals fi) $$ text "\n")

solveFile :: Config -> IO ExitCode
solveFile cfg
  = do fp  <- getFixpointPath
       z3  <- getZ3LibPath
       v   <- (\b -> if b then "-v 1" else "") <$> isLoud
       {-# SCC "sysCall:Fixpoint" #-} executeShellCommand "fixpoint" $ fixCommand fp z3 v
    where
      fixCommand fp z3 verbosity
        = printf "LD_LIBRARY_PATH=%s %s %s %s -notruekvars -refinesort -nosimple -strictsortcheck -sortedquals %s"
          z3 fp verbosity rf (command cfg)
        where
          rf  = if real cfg then realFlags else ""

realFlags :: String
realFlags =  "-no-uif-multiply "
          ++ "-no-uif-divide "


exitFq :: FilePath -> M.HashMap Integer (SubC a) -> ExitCode -> IO (Result a)
exitFq _ _ (ExitFailure n) | n /= 1
  = return $ Result (Crash [] "Unknown Error") M.empty
exitFq fn z _
  = do str <- {-# SCC "readOut" #-} readFile (extFileName Out fn)
       let (x, y) = parseFixpointOutput str
       let x'     = fmap (mlookup z) x
       return     $ Result x' y

parseFixpointOutput :: String -> (FixResult Integer, FixSolution)
parseFixpointOutput str = {-# SCC "parseFixOut" #-} rr ({-# SCC "sanitizeFixpointOutput" #-} sanitizeFixpointOutput str)

sanitizeFixpointOutput :: String -> String
sanitizeFixpointOutput
  = unlines
  . filter (not . ("//"     `isPrefixOf`))
  . chopAfter ("//QUALIFIERS" `isPrefixOf`)
  . lines

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
                  , gs    = gs     fi }
