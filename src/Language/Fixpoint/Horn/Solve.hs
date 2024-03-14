-------------------------------------------------------------------------------
-- | This module defines a function to solve NNF constraints,
--   by reducing them to the standard FInfo.
-------------------------------------------------------------------------------

module Language.Fixpoint.Horn.Solve (solveHorn, solve) where

import System.Exit ( ExitCode )
import Control.DeepSeq ( NFData )
import Control.Monad (when)
import qualified Language.Fixpoint.Misc         as Misc
import qualified Language.Fixpoint.Utils.Files  as Files
import qualified Language.Fixpoint.Solver       as Solver
import qualified Language.Fixpoint.Parse        as Parse
import qualified Language.Fixpoint.Types        as F
import qualified Language.Fixpoint.Types.Config as F
import qualified Language.Fixpoint.Horn.Types   as H

import qualified Language.Fixpoint.Horn.Parse   as H
import qualified Language.Fixpoint.Horn.SMTParse   as SH

import qualified Language.Fixpoint.Horn.Transformations as Tx
import Text.PrettyPrint.HughesPJ.Compat ( render )
import Language.Fixpoint.Horn.Info ( hornFInfo )

import System.Console.CmdArgs.Verbosity ( whenLoud )
import qualified Data.Aeson as Aeson
-- import Debug.Trace (traceM)

----------------------------------------------------------------------------------
solveHorn :: F.Config -> IO ExitCode
----------------------------------------------------------------------------------
solveHorn baseCfg = do
  q <- parseQuery baseCfg

  -- If you want to set --eliminate=none, you better make it a pragma
  cfgElim <- if F.eliminate baseCfg == F.None
           then pure (baseCfg { F.eliminate =  F.Some })
           else pure baseCfg

  cfgPragmas <- F.withPragmas cfgElim (H.qOpts q)

  when (F.save cfgPragmas) (saveHornQuery cfgPragmas q)

  r <- solve cfgPragmas q
  Solver.resultExitCode cfgPragmas r

parseQuery :: F.Config -> IO H.TagQuery
parseQuery cfg
  | F.stdin cfg = Parse.parseFromStdIn hornP
  | json        = loadFromJSON file
  | otherwise   = Parse.parseFromFile hornP file
  where
    json  = Files.isExtFile Files.Json file
    file  = F.srcFile cfg
    hornP = if F.noSmtHorn cfg then H.hornP else SH.hornP

loadFromJSON :: FilePath -> IO H.TagQuery
loadFromJSON f = do
  r <- Aeson.eitherDecodeFileStrict f
  case r of
    Right v -> return v
    Left err -> error ("Error in loadFromJSON: " ++ err)

saveHornQuery :: F.Config -> H.Query H.Tag -> IO ()
saveHornQuery cfg q = do
  saveHornSMT2 cfg q
  saveHornJSON cfg q

saveHornSMT2 :: H.ToHornSMT a => F.Config -> a -> IO ()
saveHornSMT2 cfg q = do
  let hq   = F.queryFile Files.HSmt2 cfg
  putStrLn $ "Saving Horn Query: " ++ hq ++ "\n"
  Misc.ensurePath hq
  writeFile hq $ render ({- F.pprint -} H.toHornSMT q)

saveHornJSON :: F.Config -> H.Query H.Tag -> IO ()
saveHornJSON cfg q = do
  let hjson   = F.queryFile Files.HJSON cfg
  putStrLn $ "Saving Horn Query: " ++ hjson ++ "\n"
  Misc.ensurePath hjson
  Aeson.encodeFile hjson q

----------------------------------------------------------------------------------
eliminate :: (F.Fixpoint a, F.PPrint a) => F.Config -> H.Query a -> IO (H.Query a)
----------------------------------------------------------------------------------
eliminate cfg q
  | F.eliminate cfg == F.Existentials = do
    Tx.solveEbs cfg q
  | F.eliminate cfg == F.Horn = do
    let c = Tx.elim $ H.qCstr q
    whenLoud $ putStrLn "Horn Elim:"
    whenLoud $ putStrLn $ F.showpp c
    pure $ q { H.qCstr = c }
  | otherwise = pure q

----------------------------------------------------------------------------------
solve :: (F.PPrint a, NFData a, F.Loc a, Show a, F.Fixpoint a) => F.Config -> H.Query a
       -> IO (F.Result (Integer, a))
----------------------------------------------------------------------------------
solve cfg qry = do
  let c = Tx.uniq $ Tx.flatten $ H.qCstr qry
  whenLoud $ putStrLn "Horn Uniq:"
  whenLoud $ putStrLn $ F.showpp c
  q <- eliminate cfg ({- void $ -} qry { H.qCstr = c })
  Solver.solve cfg (hornFInfo cfg q)
