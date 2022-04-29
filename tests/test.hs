{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Control.Concurrent.STM as STM
import qualified Data.Functor.Compose   as Functor
import qualified Data.IntMap            as IntMap
import qualified Control.Monad.State    as State
import Control.Monad.Trans.Class (lift)

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
  run =<< group "Tests" [unitTests]
  where
    run = defaultMainWithIngredients
              [ testRunner
              , includingOptions [ Option (Proxy :: Proxy FixpointOpts) ]
              ]

testRunner :: Ingredient
testRunner = rerunningTests
               [ listingTests
               , combineReporters myConsoleReporter antXMLRunner
               , myConsoleReporter
               ]

myConsoleReporter :: Ingredient
myConsoleReporter = combineReporters consoleTestReporter loggingTestReporter

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

unitTests
  = group "Unit" [
      testGroup "native-pos" <$> dirTests nativeCmd "tests/pos"    skipNativePos  ExitSuccess
    , testGroup "native-neg" <$> dirTests nativeCmd "tests/neg"    ["float.fq"]   (ExitFailure 1)
    , testGroup "elim-crash" <$> dirTests nativeCmd "tests/crash"  []             (ExitFailure 1)
    , testGroup "elim-pos1"  <$> dirTests elimCmd   "tests/pos"    []             ExitSuccess
    , testGroup "elim-pos2"  <$> dirTests elimCmd   "tests/elim"   []             ExitSuccess
    , testGroup "elim-neg"   <$> dirTests elimCmd   "tests/neg"    ["float.fq"]   (ExitFailure 1)
    , testGroup "elim-crash" <$> dirTests elimCmd   "tests/crash"  []             (ExitFailure 1)
    , testGroup "proof"      <$> dirTests elimCmd   "tests/proof"     []          ExitSuccess
    , testGroup "rankN"      <$> dirTests elimCmd   "tests/rankNTypes" []         ExitSuccess
    , testGroup "horn-pos-el" <$> dirTests elimCmd   "tests/horn/pos"  []          ExitSuccess
    , testGroup "horn-neg-el" <$> dirTests elimCmd   "tests/horn/neg"  []          (ExitFailure 1)
    , testGroup "horn-pos-na" <$> dirTests nativeCmd "tests/horn/pos"  []          ExitSuccess
    , testGroup "horn-neg-na" <$> dirTests nativeCmd "tests/horn/neg"  []          (ExitFailure 1)

    -- , testGroup "todo"       <$> dirTests elimCmd   "tests/todo"   []            (ExitFailure 1)
    -- , testGroup "todo-crash" <$> dirTests elimCmd   "tests/todo-crash" []        (ExitFailure 2)
   ]


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
dirTests :: TestCmd -> FilePath -> [FilePath] -> ExitCode -> IO [TestTree]
---------------------------------------------------------------------------
dirTests testCmd root ignored code = do
  files    <- walkDirectory root
  let tests = [ rel | f <- files, isTest f, let rel = makeRelative root f, rel `notElem` ignored ]
  return    $ mkTest testCmd code root <$> tests

isTest   :: FilePath -> Bool
isTest f = takeExtension f `elem` [".fq", ".smt2"]

---------------------------------------------------------------------------
mkTest :: TestCmd -> ExitCode -> FilePath -> FilePath -> TestTree
---------------------------------------------------------------------------
mkTest testCmd code dir file
  =
    askOption $ \opts ->
    testCase file $
      if test `elem` knownToFail
      then do
        printf "%s is known to fail: SKIPPING" test
        assertEqual "" True True
      else do
        createDirectoryIfMissing True $ takeDirectory log
        withFile log WriteMode $ \h -> do
          let cmd     = testCmd opts "fixpoint" dir file
          (_,_,_,ph) <- createProcess $ (shell cmd) {std_out = UseHandle h, std_err = UseHandle h}
          c          <- waitForProcess ph
          assertEqual "Wrong exit code" code c
  where
    test = dir </> file
    log  = let (d,f) = splitFileName file in dir </> d </> ".liquid" </> f <.> "log"

knownToFail = []
---------------------------------------------------------------------------
type TestCmd = FixpointOpts -> FilePath -> FilePath -> FilePath -> String

nativeCmd :: TestCmd
nativeCmd (LO opts) bin dir file =
  printf "cd %s && %s %s %s" dir bin opts file

elimCmd :: TestCmd
elimCmd (LO opts) bin dir file =
  printf "cd %s && %s --eliminate=some %s %s" dir bin opts file

----------------------------------------------------------------------------------------
-- Generic Helpers
----------------------------------------------------------------------------------------

group n xs = testGroup n <$> sequence xs

----------------------------------------------------------------------------------------
walkDirectory :: FilePath -> IO [FilePath]
----------------------------------------------------------------------------------------
walkDirectory root
  = do (ds,fs) <- partitionM doesDirectoryExist . candidates =<< (getDirectoryContents root `catchIOError` const (return []))
       (fs++) <$> concatMapM walkDirectory ds
  where
    candidates fs = [root </> f | f <- fs, not (isExtSeparator (head f))]

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
loggingTestReporter :: Ingredient
loggingTestReporter = TestReporter [] $ \opts tree -> Just $ \smap -> do
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

    runGroup _ group children = Traversal $ Functor.Compose $ do
      Const soFar <- Functor.getCompose $ getTraversal children
      pure $ Const $ map (\(n,t,s) -> (group</>n,t,s)) soFar

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
    time <- head . lines <$> readProcess "date" ["+%Y-%m-%dT%H-%M-%S"] []
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


    let smry = "tests" </> "logs" </> "cur" </> "summary.csv"
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
