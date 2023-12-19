{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE DoAndIfThenElse           #-}

-- | This module contains an SMTLIB2 interface for
--   1. checking the validity, and,
--   2. computing satisfying assignments
--   for formulas.
--   By implementing a binary interface over the SMTLIB2 format defined at
--   http://www.smt-lib.org/
--   http://www.grammatech.com/resource/smt/SMTLIBTutorial.pdf

module Language.Fixpoint.Smt.Interface (

    -- * Commands
      Command  (..)

    -- * Responses
    , Response (..)

    -- * Typeclass for SMTLIB2 conversion
    , SMTLIB2 (..)

    -- * Creating and killing SMTLIB2 Process
    , Context (..)
    , makeContext
    , makeContextNoLog
    , makeContextWithSEnv
    , cleanupContext

    -- * Execute Queries
    , command
    , smtSetMbqi

    -- * Query API
    , smtDecl
    , smtDecls
    , smtDefineFunc
    , smtAssert
    , smtFuncDecl
    , smtAssertAxiom
    , smtCheckUnsat
    , smtCheckSat
    , smtBracket, smtBracketAt
    , smtDistinct
    , smtPush, smtPop

    -- * Check Validity
    , checkValid
    , checkValid'
    , checkValidWithContext
    , checkValids

    ) where

import           Language.Fixpoint.Types.Config ( SMTSolver (..)
                                                , Config
                                                , solver
                                                , smtTimeout
                                                , gradual
                                                , stringTheory)
import qualified Language.Fixpoint.Misc          as Misc
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Utils.Files
import           Language.Fixpoint.Types         hiding (allowHO)
import qualified Language.Fixpoint.Types         as F
import           Language.Fixpoint.Smt.Types
import qualified Language.Fixpoint.Smt.Theories as Thy
import           Language.Fixpoint.Smt.Serialize ()
import           Control.Applicative      ((<|>))
import           Control.Monad
import           Control.Exception
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as Char8
import           Data.Char
import qualified Data.HashMap.Strict      as M
import           Data.Maybe              (fromMaybe)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.IO
-- import           Data.Text.Format
import qualified Data.Text.Lazy.IO        as LTIO
import           System.Directory
import           System.Console.CmdArgs.Verbosity
import           System.FilePath
import           System.IO
import qualified Data.Attoparsec.Text     as A
-- import qualified Data.HashMap.Strict      as M
import           Data.Attoparsec.Internal.Types (Parser)
import           Text.PrettyPrint.HughesPJ (text)
import           Language.Fixpoint.SortCheck
import           Language.Fixpoint.Utils.Builder as Builder
-- import qualified Language.Fixpoint.Types as F
-- import           Language.Fixpoint.Types.PrettyPrint (tracepp)
import qualified SMTLIB.Backends
import qualified SMTLIB.Backends.Process as Process
import qualified Language.Fixpoint.Conditional.Z3 as Conditional.Z3
import Control.Concurrent.Async (async)

{-
runFile f
  = readFile f >>= runString

runString str
  = runCommands $ rr str

runCommands cmds
  = do me   <- makeContext Z3
       mapM_ (T.putStrLn . smt2) cmds
       zs   <- mapM (command me) cmds
       return zs
-}

checkValidWithContext :: Context -> [(Symbol, Sort)] -> Expr -> Expr -> IO Bool
checkValidWithContext me xts p q =
  smtBracket me "checkValidWithContext" $
    checkValid' me xts p q

-- | type ClosedPred E = {v:Pred | subset (vars v) (keys E) }
-- checkValid :: e:Env -> ClosedPred e -> ClosedPred e -> IO Bool
checkValid :: Config -> FilePath -> [(Symbol, Sort)] -> Expr -> Expr -> IO Bool
checkValid cfg f xts p q = do
  me <- makeContext cfg f
  checkValid' me xts p q

checkValid' :: Context -> [(Symbol, Sort)] -> Expr -> Expr -> IO Bool
checkValid' me xts p q = do
  smtDecls me xts
  smtAssert me $ pAnd [p, PNot q]
  smtCheckUnsat me

-- | If you already HAVE a context, where all the variables have declared types
--   (e.g. if you want to make MANY repeated Queries)

-- checkValid :: e:Env -> [ClosedPred e] -> IO [Bool]
checkValids :: Config -> FilePath -> [(Symbol, Sort)] -> [Expr] -> IO [Bool]
checkValids cfg f xts ps
  = do me <- makeContext cfg f
       smtDecls me xts
       forM ps $ \p ->
          smtBracket me "checkValids" $
            smtAssert me (PNot p) >> smtCheckUnsat me

-- debugFile :: FilePath
-- debugFile = "DEBUG.smt2"

--------------------------------------------------------------------------------
-- | SMT IO --------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
{-# SCC command #-}
command              :: Context -> Command -> IO Response
--------------------------------------------------------------------------------
command Ctx {..} !cmd       = do
  -- whenLoud $ do LTIO.appendFile debugFile (s <> "\n")
  --               LTIO.putStrLn ("CMD-RAW:" <> s <> ":CMD-RAW:DONE")
  forM_ ctxLog $ \h -> do
    BS.hPutBuilder h cmdBS
    LBS.hPutStr h "\n"
  case cmd of
    CheckSat   -> commandRaw
    GetValue _ -> commandRaw
    _          -> SMTLIB.Backends.command_ ctxSolver cmdBS >> return Ok
  where
    commandRaw      = do
      resp <- SMTLIB.Backends.command ctxSolver cmdBS
      let respTxt =
            TE.decodeUtf8With (const $ const $ Just ' ') $
            LBS.toStrict resp
      parse respTxt
    cmdBS = {-# SCC "Command-runSmt2" #-} runSmt2 ctxSymEnv cmd
    parse resp      = do
      case A.parseOnly responseP resp of
        Left e  -> Misc.errorstar $ "SMTREAD:" ++ e
        Right r -> do
          let textResponse = "; SMT Says: " <> T.pack (show r)
          forM_ ctxLog $ \h ->
            Data.Text.IO.hPutStrLn h textResponse
          when ctxVerbose $
            Data.Text.IO.putStrLn textResponse
          return r

smtSetMbqi :: Context -> IO ()
smtSetMbqi me = interact' me SetMbqi

type SmtParser a = Parser T.Text a

responseP :: SmtParser Response
responseP = {- SCC "responseP" -} A.char '(' *> sexpP
         <|> A.string "sat"     *> return Sat
         <|> A.string "unsat"   *> return Unsat
         <|> A.string "unknown" *> return Unknown

sexpP :: SmtParser Response
sexpP = {- SCC "sexpP" -} A.string "error" *> (Error <$> errorP)
     <|> Values <$> valuesP

errorP :: SmtParser T.Text
errorP = A.skipSpace *> A.char '"' *> A.takeWhile1 (/='"') <* A.string "\")"

valuesP :: SmtParser [(Symbol, T.Text)]
valuesP = A.many1' pairP <* A.char ')'

pairP :: SmtParser (Symbol, T.Text)
pairP = {- SCC "pairP" -}
  do A.skipSpace
     _ <- A.char '('
     !x <- symbolP
     A.skipSpace
     !v <- valueP
     _ <- A.char ')'
     return (x,v)

symbolP :: SmtParser Symbol
symbolP = {- SCC "symbolP" -} symbol <$> A.takeWhile1 (not . isSpace)

valueP :: SmtParser T.Text
valueP = {- SCC "valueP" -} negativeP
      <|> A.takeWhile1 (\c -> not (c == ')' || isSpace c))

negativeP :: SmtParser T.Text
negativeP
  = do v <- A.char '(' *> A.takeWhile1 (/=')') <* A.char ')'
       return $ "(" <> v <> ")"

--------------------------------------------------------------------------
-- | SMT Context ---------------------------------------------------------
--------------------------------------------------------------------------

--------------------------------------------------------------------------
makeContext :: Config -> FilePath -> IO Context
--------------------------------------------------------------------------
makeContext cfg f
  = do createDirectoryIfMissing True $ takeDirectory smtFile
       hLog <- openFile smtFile WriteMode
       hSetBuffering hLog $ BlockBuffering $ Just $ 1024 * 1024 * 64
       me   <- makeContext' cfg $ Just hLog
       pre  <- smtPreamble cfg (solver cfg) me
       mapM_ (SMTLIB.Backends.command_ (ctxSolver me)) pre
       return me
    where
       smtFile = extFileName Smt2 f

makeContextWithSEnv :: Config -> FilePath -> SymEnv -> IO Context
makeContextWithSEnv cfg f env = do
  ctx     <- makeContext cfg f
  let ctx' = ctx {ctxSymEnv = env}
  declare ctx'
  return ctx'
  -- where msg = "makeContextWithSEnv" ++ show env

makeContextNoLog :: Config -> IO Context
makeContextNoLog cfg
  = do me  <- makeContext' cfg Nothing
       pre <- smtPreamble cfg (solver cfg) me
       mapM_ (SMTLIB.Backends.command_ (ctxSolver me)) pre
       return me

makeProcess
  :: Maybe Handle
  -> Process.Config
  -> IO (SMTLIB.Backends.Backend, IO ())
makeProcess ctxLog cfg
  = do handl@Process.Handle {hMaybeErr = Just hErr, ..} <- Process.new cfg
       case ctxLog of
         Nothing -> return ()
         Just hLog -> void $ async $ forever
           (do errTxt <- LTIO.hGetLine hErr
               LTIO.hPutStrLn hLog $ "OOPS, SMT solver error:" <> errTxt
           ) `catch` \ SomeException {} -> return ()
       let backend = Process.toBackend handl
       hSetBuffering hOut $ BlockBuffering $ Just $ 1024 * 1024 * 64
       hSetBuffering hIn $ BlockBuffering $ Just $ 1024 * 1024 * 64
       return (backend, Process.close handl)

makeContext' :: Config -> Maybe Handle -> IO Context
makeContext' cfg ctxLog
  = do (backend, closeIO) <- case solver cfg of
         Z3      ->
           {- "z3 -smt2 -in"                   -}
           {- "z3 -smtc SOFT_TIMEOUT=1000 -in" -}
           {- "z3 -smtc -in MBQI=false"        -}
           makeProcess ctxLog $ Process.defaultConfig
                             { Process.exe = "z3"
                             , Process.args = ["-smt2", "-in"] }
         Z3mem   -> Conditional.Z3.makeZ3
         Mathsat -> makeProcess ctxLog $ Process.defaultConfig
                             { Process.exe = "mathsat"
                             , Process.args = ["-input=smt2"] }
         Cvc4    -> makeProcess ctxLog $
                      Process.defaultConfig
                             { Process.exe = "cvc4"
                             , Process.args = ["--incremental", "-L", "smtlib2"] }
       solver <- SMTLIB.Backends.initSolver SMTLIB.Backends.Queuing backend
       loud <- isLoud
       return Ctx { ctxSolver  = solver
                  , ctxClose   = closeIO
                  , ctxLog     = ctxLog
                  , ctxVerbose = loud
                  , ctxSymEnv  = mempty
                  }

-- | Close file handles and release the solver backend's resources.
cleanupContext :: Context -> IO ()
cleanupContext Ctx {..} = do
  maybe (return ()) (hCloseMe "ctxLog") ctxLog
  ctxClose

hCloseMe :: String -> Handle -> IO ()
hCloseMe msg h = hClose h `catch` (\(exn :: IOException) -> putStrLn $ "OOPS, hClose breaks: " ++ msg ++ show exn)

smtPreamble :: Config -> SMTSolver -> Context -> IO [Builder]
smtPreamble cfg s me
  | s == Z3 || s == Z3mem
    = do v <- getZ3Version me
         checkValidStringFlag Z3 v cfg
         return $ z3_options ++ makeMbqi cfg ++ makeTimeout cfg ++ Thy.preamble cfg Z3
  | otherwise
    = checkValidStringFlag s [] cfg >> return (Thy.preamble cfg s)

getZ3Version :: Context -> IO [Int]
getZ3Version me
  = do -- resp is like (:version "4.8.15")
       resp <- SMTLIB.Backends.command (ctxSolver me) "(get-info :version)"
       case Char8.split '"' resp of
         _:rText:_ -> do
           -- strip off potential " - build hashcode ..." suffix
           let vText = Char8.takeWhile (not . isSpace) rText
           let parsedComponents = [ reads (Char8.unpack cText) | cText <- Char8.split '.' vText ]
           sequence
             [ case pComponent of
                 [(c, "")] -> return c
                 xs -> error $ "Can't parse z3 version: " ++ show xs
             | pComponent <- parsedComponents
             ]
         xs -> error $ "Can't parse z3 (get-info :version): " ++ show xs

checkValidStringFlag :: SMTSolver -> [Int] -> Config -> IO ()
checkValidStringFlag smt v cfg
  = when (noString smt v cfg) $
      die $ err dummySpan (text "stringTheory is only supported by z3 version >=4.2.2")

noString :: SMTSolver -> [Int] -> Config -> Bool
noString smt v cfg
  =  stringTheory cfg
  && not (smt == Z3 && (v >= [4, 4, 2]))

-----------------------------------------------------------------------------
-- | SMT Commands -----------------------------------------------------------
-----------------------------------------------------------------------------

smtPush, smtPop   :: Context -> IO ()
smtPush me        = interact' me Push
smtPop me         = interact' me Pop

smtDecls :: Context -> [(Symbol, Sort)] -> IO ()
smtDecls = mapM_ . uncurry . smtDecl

smtDecl :: Context -> Symbol -> Sort -> IO ()
smtDecl me x t = interact' me ({- notracepp msg $ -} Declare (symbolSafeText x) ins' out')
  where
    ins'       = sortSmtSort False env <$> ins
    out'       = sortSmtSort False env     out
    (ins, out) = deconSort t
    _msg        = "smtDecl: " ++ showpp (x, t, ins, out)
    env        = seData (ctxSymEnv me)

smtFuncDecl :: Context -> T.Text -> ([SmtSort],  SmtSort) -> IO ()
smtFuncDecl me x (ts, t) = interact' me (Declare x ts t)

smtDataDecl :: Context -> [DataDecl] -> IO ()
smtDataDecl me ds = interact' me (DeclData ds)

deconSort :: Sort -> ([Sort], Sort)
deconSort t = case functionSort t of
                Just (_, ins, out) -> (ins, out)
                Nothing            -> ([], t)

-- hack now this is used only for checking gradual condition.
smtCheckSat :: Context -> Expr -> IO Bool
smtCheckSat me p
 = smtAssert me p >> (ans <$> command me CheckSat)
 where
   ans Sat = True
   ans _   = False

smtAssert :: Context -> Expr -> IO ()
smtAssert me p  = interact' me (Assert Nothing p)

smtDefineFunc :: Context -> Symbol -> [(Symbol, F.Sort)] -> F.Sort -> Expr -> IO ()
smtDefineFunc me name symList rsort e =
  let env = seData (ctxSymEnv me)
   in interact' me $
        DefineFunc
          name
          (map (sortSmtSort False env <$>) symList)
          (sortSmtSort False env rsort)
          e

-----------------------------------------------------------------

smtAssertAxiom :: Context -> Triggered Expr -> IO ()
smtAssertAxiom me p  = interact' me (AssertAx p)

smtDistinct :: Context -> [Expr] -> IO ()
smtDistinct me az = interact' me (Distinct az)

smtCheckUnsat :: Context -> IO Bool
smtCheckUnsat me  = respSat <$> command me CheckSat

smtBracketAt :: SrcSpan -> Context -> String -> IO a -> IO a
smtBracketAt sp x y z = smtBracket x y z `catch` dieAt sp

smtBracket :: Context -> String -> IO a -> IO a
smtBracket me _msg a   = do
  smtPush me
  r <- a
  smtPop me
  return r

respSat :: Response -> Bool
respSat Unsat   = True
respSat Sat     = False
respSat Unknown = False
respSat r       = die $ err dummySpan $ text ("crash: SMTLIB2 respSat = " ++ show r)

interact' :: Context -> Command -> IO ()
interact' me cmd  = void $ command me cmd


makeTimeout :: Config -> [Builder]
makeTimeout cfg
  | Just i <- smtTimeout cfg = [ "\n(set-option :timeout " <> fromString (show i) <> ")\n"]
  | otherwise                = [""]


makeMbqi :: Config -> [Builder]
makeMbqi cfg
  | gradual cfg = [""]
  | otherwise   = ["\n(set-option :smt.mbqi false)"]

z3_options :: [Builder]
z3_options
  = [ "(set-option :auto-config false)"
    , "(set-option :model true)" ]



--------------------------------------------------------------------------------
declare :: Context -> IO ()
--------------------------------------------------------------------------------
declare me = do
  forM_ dss    $           smtDataDecl me
  forM_ thyXTs $ uncurry $ smtDecl     me
  forM_ qryXTs $ uncurry $ smtDecl     me
  forM_ ats    $ uncurry $ smtFuncDecl me
  forM_ ess    $           smtDistinct me
  forM_ axs    $           smtAssert   me
  where
    env        = ctxSymEnv me
    dss        = dataDeclarations          env
    lts        = F.toListSEnv . F.seLits $ env
    ess        = distinctLiterals  lts
    axs        = Thy.axiomLiterals lts
    thyXTs     =                    filter (isKind 1) xts
    qryXTs     = Misc.mapSnd tx <$> filter (isKind 2) xts
    isKind n   = (n ==)  . symKind env . fst
    xts        = {- tracepp "symbolSorts" $ -} symbolSorts (F.seSort env)
    tx         = elaborate    "declare" env
    ats        = funcSortVars env

symbolSorts :: F.SEnv F.Sort -> [(F.Symbol, F.Sort)]
symbolSorts env = [(x, tx t) | (x, t) <- F.toListSEnv env ]
 where
  tx t@(FObj a) = fromMaybe t (F.lookupSEnv a env)
  tx t          = t

dataDeclarations :: SymEnv -> [[DataDecl]]
dataDeclarations = orderDeclarations . map snd . F.toListSEnv . F.seData

funcSortVars :: F.SymEnv -> [(T.Text, ([F.SmtSort], F.SmtSort))]
funcSortVars env  = [(var applyName  t       , appSort t) | t <- ts]
                 ++ [(var coerceName t       , ([t1],t2)) | t@(t1, t2) <- ts]
                 ++ [(var lambdaName t       , lamSort t) | t <- ts]
                 ++ [(var (lamArgSymbol i) t , argSort t) | t@(_,F.SInt) <- ts, i <- [1..Thy.maxLamArg] ]
  where
    var n         = F.symbolAtSmtName n env ()
    ts            = M.keys (F.seAppls env)
    appSort (s,t) = ([F.SInt, s], t)
    lamSort (s,t) = ([s, t], F.SInt)
    argSort (s,_) = ([]    , s)

-- | 'symKind' returns {0, 1, 2} where:
--   0 = Theory-Definition,
--   1 = Theory-Declaration,
--   2 = Query-Binder

symKind :: F.SymEnv -> F.Symbol -> Int
symKind env x = case F.tsInterp <$> F.symEnvTheory x env of
                  Just F.Theory   -> 0
                  Just F.Ctor     -> 0
                  Just F.Test     -> 0
                  Just F.Field    -> 0
                  Just F.Uninterp -> 1
                  Nothing         -> 2
              -- Just t  -> if tsInterp t then 0 else 1


-- assumes :: [F.Expr] -> SolveM ()
-- assumes es = withContext $ \me -> forM_  es $ smtAssert me

-- | `distinctLiterals` is used solely to determine the set of literals
--   (of each sort) that are *disequal* to each other, e.g. EQ, LT, GT,
--   or string literals "cat", "dog", "mouse". These should only include
--   non-function sorted values.
distinctLiterals :: [(F.Symbol, F.Sort)] -> [[F.Expr]]
distinctLiterals xts = [ es | (_, es) <- tess ]
   where
    tess             = Misc.groupList [(t, F.expr x) | (x, t) <- xts, notFun t]
    notFun           = not . F.isFunctionSortedReft . (`F.RR` F.trueReft)
    -- _notStr          = not . (F.strSort ==) . F.sr_sort . (`F.RR` F.trueReft)
