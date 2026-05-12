{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import qualified Control.Concurrent.STM as STM
import qualified Data.Functor.Compose   as Functor
import qualified Data.IntMap            as IntMap
import Control.Monad (when)
import qualified Control.Monad.State    as State
import Control.Monad.Trans.Class (lift)
import Data.List (dropWhileEnd, isSuffixOf)
import Prelude hiding (log)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..))
import Data.Proxy
import Data.Tagged
import Control.Applicative
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Error
import System.Process
import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Options
import Test.Tasty.Runners
import Test.Tasty.Runners.AntXML

main :: IO ()
main    = do
  lfDir <- findLiquidFixpointDir
  run lfDir =<< group "Tests" [unitTests lfDir]
  where
    run lfDir = defaultMainWithIngredients
              [ testRunner lfDir
              , includingOptions [ Option (Proxy :: Proxy FixpointOpts) ]
              ]

-- | Searches for the directory of liquid-fixpoint.cabal and changes to it
findLiquidFixpointDir :: IO FilePath
findLiquidFixpointDir = do
    dir0 <- getCurrentDirectory
    let candidates = [dir0, dir0 </> "liquid-fixpoint"]
        findCabalDir :: [FilePath] -> IO (Maybe FilePath)
        findCabalDir [] = return Nothing
        findCabalDir (d:xs) = do
          let cabalFile = d </> "liquid-fixpoint.cabal"
          exists <- doesFileExist cabalFile
          if exists then
            return (Just d)
           else
            findCabalDir xs
    mDir <- findCabalDir candidates
    case mDir of
      Just d  -> return d
      Nothing -> error "Could not find liquid-fixpoint.cabal"

testRunner :: FilePath -> Ingredient
testRunner lfDir = rerunningTests
               [ listingTests
               , combineReporters (myConsoleReporter lfDir) antXMLRunner
               , myConsoleReporter lfDir
               ]

myConsoleReporter :: FilePath -> Ingredient
myConsoleReporter lfDir = combineReporters consoleTestReporter (loggingTestReporter lfDir)

-- | Combine two @TestReporter@s into one.
--
-- Runs the reporters in sequence, so it's best to start with the one
-- that will produce incremental output, e.g. 'consoleTestReporter'.
combineReporters :: Ingredient -> Ingredient -> Ingredient
combineReporters (TestReporter opts1 run1) (TestReporter opts2 run2)
  = TestReporter (opts1 ++ opts2) $ \opts tree -> do
      f1 <- run1 opts tree
      f2 <- run2 opts tree
      return $ \smap -> f1 smap >> f2 smap
combineReporters _ _ = error "combineReporters needs TestReporters"

unitTests :: FilePath -> IO TestTree
unitTests lfDir =
    group "All"
      [ group "original"
        [ dirTests "native-pos"           nativeCmd   "tests/pos"              posOptions skipNativePos  ExitSuccess
        , dirTests "native-neg"           nativeCmd   "tests/neg"              [] ["float.fq"]  (ExitFailure 1)
        , dirTests "elim-crash"           nativeCmd   "tests/crash"            posOptions []            (ExitFailure 1)
        , dirTests "elim-pos1"            elimCmd     "tests/pos"              posOptions []             ExitSuccess
        , dirTests "elim-pos2"            elimCmd     "tests/elim"             posOptions []             ExitSuccess
        , dirTests "elim-neg"             elimCmd     "tests/neg"              [] ["float.fq"]  (ExitFailure 1)
        , dirTests "elim-crash"           elimCmd     "tests/crash"            []                      []            (ExitFailure 1)
        , dirTests "cvc5-pos"             cvc5Cmd     "tests/pos"              posOptions skipNativePos  ExitSuccess
        , dirTests "cvc5-spec"            cvc5Cmd     "tests/cvc5"             posOptions skipNativePos  ExitSuccess
        , dirTests "proof"                elimCmd     "tests/proof"            posOptions []             ExitSuccess
        , dirTests "rankN"                elimCmd     "tests/rankNTypes"       posOptions []             ExitSuccess
        , dirTests "horn-pos-el"          elimSaveCmd "tests/horn/pos"         posOptions []             ExitSuccess
        , dirTests "horn-pos-cvc5"        cvc5Cmd     "tests/horn/pos"         posOptions []             ExitSuccess
        , dirTests "horn-neg-el"          elimSaveCmd "tests/horn/neg"         []         []            (ExitFailure 1)
        , dirTests "horn-neg-cvc5"        cvc5Cmd     "tests/horn/neg"         []         []            (ExitFailure 1)
        , dirTests "horn-pos-na"          nativeCmd   "tests/horn/pos"         posOptions []             ExitSuccess
        , dirTests "horn-neg-na"          nativeCmd   "tests/horn/neg"         []         []            (ExitFailure 1)
        ]
      , after AllSucceed "original" <$> group "saved"
        [ dirJsonTests "horn-json-pos-el" elimCmd     "tests/logs/cur/horn-pos-el" []         []             ExitSuccess
        , dirJsonTests "horn-json-neg-el" elimCmd     "tests/logs/cur/horn-neg-el" []         []            (ExitFailure 1)
        , dirHornTests "horn-smt2-pos-el" elimCmd     "tests/logs/cur/horn-pos-el" []         []             ExitSuccess
        , dirHornTests "horn-smt2-neg-el" elimCmd     "tests/logs/cur/horn-neg-el" []         []            (ExitFailure 1)
        ]
      , return $ testGroup "flags"
        [ testCase "--numeric-version" $ do
            (code, out, _) <- readProcessWithExitCode "fixpoint" ["--numeric-version"] ""
            assertEqual "Wrong exit code" ExitSuccess code
            let ver = dropWhileEnd (== '\n') out
            assertBool ("Expected a version number like X.Y.Z, got: " ++ show ver)
                       (not (null ver) && all isNumericSegment (splitOn '.' ver))
        ]
      ]
  where
    posOptions = ["--save-bfq-on-error"]

    dirTests     n a b c d e = testGroup n <$> dirTests' n isTest a b c d e
    dirJsonTests n a b c d e = testGroup n <$> dirTests' n ("horn.json" `isSuffixOf`) a b c d e
    dirHornTests n a b c d e = testGroup n <$> dirTests' n ("horn.smt2" `isSuffixOf`) a b c d e

    dirTests' :: String -> (FilePath -> Bool) -> TestCmd -> FilePath -> [String] -> [FilePath] -> ExitCode -> IO [TestTree]
    dirTests' testName isT testCmd root extraOpts ignored code = do
      let absRoot = lfDir </> root
      files    <- walkDirectory absRoot
      let tests = [ rel | f <- files, isT f, let rel = makeRelative absRoot f, rel `notElem` ignored ]
          saveDir = "--save-dir=" ++ lfDir </> "tests" </> "logs" </> "cur" </> testName
      return $ mkTest testName testCmd code (saveDir : extraOpts) absRoot <$> tests

isTest   :: FilePath -> Bool
isTest f = takeExtension f `elem` [".fq", ".smt2"]

skipNativePos :: [FilePath]
skipNativePos = ["NonLinear-pack.fq"]

newtype FixpointOpts = LO String deriving (Show, Read, Eq, Ord)

instance Semigroup FixpointOpts where
  (LO "") <> y       = y
  x       <> (LO "") = x
  (LO x)  <> (LO y)  = LO $ x ++ (' ' : y)

instance Monoid FixpointOpts where
  mempty = LO ""
  mappend = (<>)

instance IsOption FixpointOpts where
  defaultValue = LO ""
  parseValue = Just . LO
  optionName = return "fixpoint-opts"
  optionHelp = return "Extra options to pass to fixpoint"
  optionCLParser =
    option (fmap LO str)
      (  long (untag (optionName :: Tagged FixpointOpts String))
      <> help (untag (optionHelp :: Tagged FixpointOpts String))
      )

---------------------------------------------------------------------------
mkTest :: String -> TestCmd -> ExitCode -> [String] -> FilePath -> FilePath -> TestTree
---------------------------------------------------------------------------
mkTest testName testCmd code extraOpts dir file
  =
    askOption $ \opts ->
    testCase file $
      if test `elem` knownToFail
      then do
        printf "%s is known to fail: SKIPPING" test
        assertEqual "" True True
      else do
        createDirectoryIfMissing True $ takeDirectory log
        c <- withFile log WriteMode $ \h -> do
          let cmd     = testCmd (LO (unwords extraOpts) <> opts) "fixpoint" dir file
          (_,_,_,ph) <- createProcess $ (shell cmd) {std_out = UseHandle h, std_err = UseHandle h}
          waitForProcess ph
        when (code /= c) $
          readFile log >>= putStrLn
        assertEqual "Wrong exit code" code c

  where
    test = dir </> file
    -- select a file name that is unique to the test, as the tests might run
    -- in parallel.
    log  = let (d,f) = splitFileName file in dir </> d </> ".liquid" </> testName </> f <.> "harness.log"

knownToFail :: [a]
knownToFail = []
---------------------------------------------------------------------------
type TestCmd = FixpointOpts -> FilePath -> FilePath -> FilePath -> String

nativeCmd :: TestCmd
nativeCmd (LO opts) bin dir file =
  printf "cd %s && %s %s %s" dir bin opts file

elimCmd :: TestCmd
elimCmd (LO opts) bin dir file =
  printf "cd %s && %s --eliminate=some %s %s" dir bin opts file

elimSaveCmd :: TestCmd
elimSaveCmd (LO opts) bin dir file =
  printf "cd %s && %s --save --eliminate=some %s %s" dir bin opts file

cvc5Cmd :: TestCmd
cvc5Cmd (LO opts) bin dir file =
  printf "cd %s && %s --solver=cvc5 %s %s" dir bin opts file

----------------------------------------------------------------------------------------
-- Generic Helpers
----------------------------------------------------------------------------------------

group :: Monad f => TestName -> [f TestTree] -> f TestTree
group n xs = testGroup n <$> sequence xs

-- | Split a string on a delimiter character.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn d (c:cs)
  | c == d    = "" : splitOn d cs
  | otherwise = let (w:ws) = splitOn d cs in (c:w) : ws

-- | A numeric version segment is a non-empty string of digits.
isNumericSegment :: String -> Bool
isNumericSegment s = not (null s) && all (\c -> c >= '0' && c <= '9') s

----------------------------------------------------------------------------------------
walkDirectory :: FilePath -> IO [FilePath]
----------------------------------------------------------------------------------------
walkDirectory root
  = do (ds,fs) <- partitionM doesDirectoryExist . candidates =<< (getDirectoryContents root `catchIOError` const (return []))
       (fs++) <$> concatMapM walkDirectory ds
  where
    candidates fs = [root </> f | f@(c:_) <- fs, not (isExtSeparator c)]

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f = go [] []
  where
    go ls rs []     = return (ls,rs)
    go ls rs (x:xs) = do b <- f x
                         if b then go (x:ls) rs xs
                              else go ls (x:rs) xs

-- isDirectory :: FilePath -> IO Bool
-- isDirectory = fmap Posix.isDirectory . Posix.getFileStatus

concatMapM :: Applicative m => (a -> m [b]) -> [a] -> m [b]
concatMapM _ []     = pure []
concatMapM f (x:xs) = (++) <$> f x <*> concatMapM f xs



-- this is largely based on ocharles' test runner at
-- https://github.com/ocharles/tasty-ant-xml/blob/master/Test/Tasty/Runners/AntXML.hs#L65
loggingTestReporter :: FilePath -> Ingredient
loggingTestReporter lfDir = TestReporter [] $ \opts tree -> Just $ \smap -> do
  let
    runTest _ testName _ = Traversal $ Functor.Compose $ do
        i <- State.get

        summary <- lift $ STM.atomically $ do
          status <- STM.readTVar $
            fromMaybe (error "Attempted to lookup test by index outside bounds") $
              IntMap.lookup i smap

          let mkSuccess time = [(testName, time, True)]
              mkFailure time = [(testName, time, False)]

          case status of
            -- If the test is done, generate a summary for it
            Done result
              | resultSuccessful result
                  -> pure (mkSuccess (resultTime result))
              | otherwise
                  -> pure (mkFailure (resultTime result))
            -- Otherwise the test has either not been started or is currently
            -- executing
            _ -> STM.retry

        Const summary <$ State.modify (+ 1)

    runGroup _ group' children = Traversal $ Functor.Compose $ do
      Const soFar <- Functor.getCompose $ getTraversal $ mconcat children
      pure $ Const $ map (\(n,t,s) -> (group' </> n,t,s)) soFar

    computeFailures :: StatusMap -> IO Int
    computeFailures = fmap getSum . getApp . foldMap (\var -> Ap $
      (\r -> Sum $ if resultSuccessful r then 0 else 1) <$> getResultFromTVar var)

    getResultFromTVar :: STM.TVar Status -> IO Result
    getResultFromTVar var =
      STM.atomically $ do
        status <- STM.readTVar var
        case status of
          Done r -> return r
          _ -> STM.retry

  (Const summary, _tests) <-
     flip State.runStateT 0 $ Functor.getCompose $ getTraversal $
      foldTestTree
        trivialFold { foldSingle = runTest, foldGroup = runGroup }
        opts
        tree

  return $ \_elapsedTime -> do
    -- don't use the `time` package, major api differences between ghc 708 and 710
    -- build header
    ref <- gitRef
    timestamp <- gitTimestamp
    epochTime <- gitEpochTimestamp
    hash <- gitHash
    let hdr = unlines [ref ++ " : " ++ hash,
                       "Timestamp: " ++ timestamp,
                       "Epoch Timestamp: " ++ epochTime,
                       headerDelim,
                       "test, time(s), result"]


    let smry = lfDir </> "tests" </> "logs" </> "cur" </> "summary.csv"
    writeFile smry $ unlines
                   $ hdr
                   : map (\(n, t, r) -> printf "%s, %0.4f, %s" n t (show r)) summary
    (==0) <$> computeFailures smap


gitTimestamp :: IO String
gitTimestamp = do
   res <- gitProcess ["show", "--format=\"%ci\"", "--quiet"]
   return $ filter notNoise res

gitEpochTimestamp :: IO String
gitEpochTimestamp = do
   res <- gitProcess ["show", "--format=\"%ct\"", "--quiet"]
   return $ filter notNoise res

gitHash :: IO String
gitHash = do
   res <- gitProcess ["show", "--format=\"%H\"", "--quiet"]
   return $ filter notNoise res

gitRef :: IO String
gitRef = do
   res <- gitProcess ["show", "--format=\"%d\"", "--quiet"]
   return $ filter notNoise res

-- | Calls `git` for info; returns `"plain"` if we are not in a git directory.
gitProcess :: [String] -> IO String
gitProcess args = readProcess "git" args [] `catchIOError` const (return "plain")

notNoise :: Char -> Bool
notNoise a = a /= '\"' && a /= '\n' && a /= '\r'

headerDelim :: String
headerDelim = replicate 80 '-'
