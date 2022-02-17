{-# LANGUAGE CPP                       #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE PatternGuards             #-}

-- | This module contains an SMTLIB2 interface for
--   1. checking the validity, and,
--   2. computing satisfying assignments
--   for formulas.
--   By implementing a binary interface over the SMTLIB2 format defined at
--   http://www.smt-lib.org/
--   http://www.grammatech.com/resource/smt/SMTLIBTutorial.pdf

-- Note [Async SMT API]
--
-- The SMT solver is started in a separate process and liquid-fixpoint
-- communicates with it via pipes. This mechanism introduces some latency
-- since the queries need to reach the buffers in a separate process and
-- the OS has to switch contexts.
--
-- A remedy we currently try for this is to send multiple queries
-- together without waiting for the reply to each one, i.e. asynchronously.
-- We then collect the multiple answers after sending all of the queries.
--
-- The functions named @smt*Async@ implement this scheme.
--
-- An asynchronous thread is used to write the queries to prevent the
-- caller from blocking on IO, should the write buffer be full or should
-- an 'hFlush' call be necessary.

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
    , smtExit
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
    , smtAssertAsync
    , smtCheckUnsatAsync
    , readCheckUnsat
    , smtBracketAsyncAt
    , smtPushAsync
    , smtPopAsync

    -- * Check Validity
    , checkValid
    , checkValid'
    , checkValidWithContext
    , checkValids

    ) where

import           Control.Concurrent.Async (async, cancel)
import           Control.Concurrent.STM
  (TVar, atomically, modifyTVar, newTVarIO, readTVar, retry, writeTVar)
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
import           Data.Char
import qualified Data.HashMap.Strict      as M
import           Data.Maybe              (fromMaybe)
#if !MIN_VERSION_base(4,14,0)
import           Data.Semigroup          (Semigroup (..))
#endif

import qualified Data.Text                as T
-- import           Data.Text.Format
import qualified Data.Text.IO             as TIO
import qualified Data.Text.Lazy           as LT
import qualified Data.Text.Lazy.IO        as LTIO
import           System.Directory
import           System.Console.CmdArgs.Verbosity
import           System.Exit              hiding (die)
import           System.FilePath
import           System.IO
import           System.Process
import qualified Data.Attoparsec.Text     as A
-- import qualified Data.HashMap.Strict      as M
import           Data.Attoparsec.Internal.Types (Parser)
import           Text.PrettyPrint.HughesPJ (text)
import           Language.Fixpoint.SortCheck
import           Language.Fixpoint.Utils.Builder as Builder
-- import qualified Language.Fixpoint.Types as F
-- import           Language.Fixpoint.Types.PrettyPrint (tracepp)

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
command me !cmd       = say >> hear cmd
  where
    env               = ctxSymEnv me
    say               = smtWrite me ({-# SCC "Command-runSmt2" #-} Builder.toLazyText (runSmt2 env cmd))
    hear CheckSat     = smtRead me
    hear (GetValue _) = smtRead me
    hear _            = return Ok

smtExit :: Context -> IO ()
smtExit me = asyncCommand me Exit

smtSetMbqi :: Context -> IO ()
smtSetMbqi me = asyncCommand me SetMbqi

smtWrite :: Context -> Raw -> IO ()
smtWrite me !s = smtWriteRaw me s

smtRead :: Context -> IO Response
smtRead me = {- SCC "smtRead" #-} do
  when (ctxVerbose me) $ LTIO.putStrLn "SMT READ"
  ln  <- smtReadRaw me
  res <- A.parseWith (smtReadRaw me) responseP ln
  case A.eitherResult res of
    Left e  -> Misc.errorstar $ "SMTREAD:" ++ e
    Right r -> do
      maybe (return ()) (\h -> LTIO.hPutStrLn h $ blt ("; SMT Says: " <> (bShow r))) (ctxLog me)
      when (ctxVerbose me) $ LTIO.putStrLn $ blt ("SMT Says: " <> bShow r)
      return r



type SmtParser a = Parser T.Text a

responseP :: SmtParser Response
responseP = {- SCC "responseP" #-} A.char '(' *> sexpP
         <|> A.string "sat"     *> return Sat
         <|> A.string "unsat"   *> return Unsat
         <|> A.string "unknown" *> return Unknown

sexpP :: SmtParser Response
sexpP = {- SCC "sexpP" #-} A.string "error" *> (Error <$> errorP)
     <|> Values <$> valuesP

errorP :: SmtParser T.Text
errorP = A.skipSpace *> A.char '"' *> A.takeWhile1 (/='"') <* A.string "\")"

valuesP :: SmtParser [(Symbol, T.Text)]
valuesP = A.many1' pairP <* A.char ')'

pairP :: SmtParser (Symbol, T.Text)
pairP = {- SCC "pairP" #-}
  do A.skipSpace
     A.char '('
     !x <- symbolP
     A.skipSpace
     !v <- valueP
     A.char ')'
     return (x,v)

symbolP :: SmtParser Symbol
symbolP = {- SCC "symbolP" #-} symbol <$> A.takeWhile1 (not . isSpace)

valueP :: SmtParser T.Text
valueP = {- SCC "valueP" #-} negativeP
      <|> A.takeWhile1 (\c -> not (c == ')' || isSpace c))

negativeP :: SmtParser T.Text
negativeP
  = do v <- A.char '(' *> A.takeWhile1 (/=')') <* A.char ')'
       return $ "(" <> v <> ")"

smtWriteRaw      :: Context -> Raw -> IO ()
smtWriteRaw me !s = {- SCC "smtWriteRaw" #-} do
  -- whenLoud $ do LTIO.appendFile debugFile (s <> "\n")
  --               LTIO.putStrLn ("CMD-RAW:" <> s <> ":CMD-RAW:DONE")
  hPutStrLnNow (ctxCout me) s
  maybe (return ()) (`LTIO.hPutStrLn` s) (ctxLog me)

smtReadRaw       :: Context -> IO T.Text
smtReadRaw me    = {- SCC "smtReadRaw" #-} TIO.hGetLine (ctxCin me)
{-# SCC smtReadRaw  #-}

hPutStrLnNow     :: Handle -> LT.Text -> IO ()
hPutStrLnNow h !s = LTIO.hPutStrLn h s >> hFlush h
{-# SCC hPutStrLnNow #-}

--------------------------------------------------------------------------
-- | SMT Context ---------------------------------------------------------
--------------------------------------------------------------------------

--------------------------------------------------------------------------
makeContext   :: Config -> FilePath -> IO Context
--------------------------------------------------------------------------
makeContext cfg f
  = do me   <- makeProcess cfg
       pre  <- smtPreamble cfg (solver cfg) me
       createDirectoryIfMissing True $ takeDirectory smtFile
       hLog <- openFile smtFile WriteMode
       hSetBuffering hLog $ BlockBuffering $ Just $ 1024*1024*64
       let me' = me { ctxLog = Just hLog }
       mapM_ (smtWrite me') pre
       return me'
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
  = do me  <- makeProcess cfg
       pre <- smtPreamble cfg (solver cfg) me
       mapM_ (smtWrite me) pre
       return me

makeProcess :: Config -> IO Context
makeProcess cfg
  = do (hOut, hIn, _ ,pid) <- runInteractiveCommand $ smtCmd (solver cfg)
       loud <- isLoud
       hSetBuffering hOut $ BlockBuffering $ Just $ 1024*1024*64
       hSetBuffering hIn $ BlockBuffering $ Just $ 1024*1024*64
       -- See Note [Async SMT API]
       queueTVar <- newTVarIO mempty
       writerAsync <- async $ forever $ do
         t <- atomically $ do
           builder <- readTVar queueTVar
           let t = Builder.toLazyText builder
           when (LT.null t) retry
           writeTVar queueTVar mempty
           return t
         LTIO.hPutStr hOut t
         hFlush hOut
       return Ctx { ctxPid     = pid
                  , ctxCin     = hIn
                  , ctxCout    = hOut
                  , ctxLog     = Nothing
                  , ctxVerbose = loud
                  , ctxSymEnv  = mempty
                  , ctxAsync   = writerAsync
                  , ctxTVar    = queueTVar
                  }

-- | Close file handles and wait for the solver process to terminate.
cleanupContext :: Context -> IO ExitCode
cleanupContext (Ctx {..}) = do
  cancel ctxAsync
  hCloseMe "ctxCin"  ctxCin
  hCloseMe "ctxCout" ctxCout
  maybe (return ()) (hCloseMe "ctxLog") ctxLog
  waitForProcess ctxPid

hCloseMe :: String -> Handle -> IO ()
hCloseMe msg h = hClose h `catch` (\(exn :: IOException) -> putStrLn $ "OOPS, hClose breaks: " ++ msg ++ show exn)

{- "z3 -smt2 -in"                   -}
{- "z3 -smtc SOFT_TIMEOUT=1000 -in" -}
{- "z3 -smtc -in MBQI=false"        -}

smtCmd         :: SMTSolver -> String --  T.Text
smtCmd Z3      = "z3 -smt2 -in"
smtCmd Mathsat = "mathsat -input=smt2"
smtCmd Cvc4    = "cvc4 --incremental -L smtlib2"

-- DON'T REMOVE THIS! z3 changed the names of options between 4.3.1 and 4.3.2...
smtPreamble :: Config -> SMTSolver -> Context -> IO [LT.Text]
smtPreamble cfg Z3 me
  = do smtWrite me "(get-info :version)"
       v:_ <- T.words . (!!1) . T.splitOn "\"" <$> smtReadRaw me
       checkValidStringFlag Z3 v cfg
       if T.splitOn "." v `versionGreaterEq` ["4", "3", "2"]
         then return $ z3_432_options ++ makeMbqi cfg ++ makeTimeout cfg ++ Thy.preamble cfg Z3
         else return $ z3_options     ++ makeMbqi cfg ++ makeTimeout cfg ++ Thy.preamble cfg Z3
smtPreamble cfg s _
  = checkValidStringFlag s "" cfg >> return (Thy.preamble cfg s)

checkValidStringFlag :: SMTSolver -> T.Text -> Config -> IO ()
checkValidStringFlag smt v cfg
  = when (noString smt v cfg) $
      die $ err dummySpan (text "stringTheory is only supported by z3 version >=4.2.2")

noString :: SMTSolver -> T.Text -> Config -> Bool
noString smt v cfg
  =  stringTheory cfg
  && not (smt == Z3 && (T.splitOn "." v `versionGreaterEq` ["4", "4", "2"]))


versionGreaterEq :: Ord a => [a] -> [a] -> Bool
versionGreaterEq (x:xs) (y:ys)
  | x >  y = True
  | x == y = versionGreaterEq xs ys
  | x <  y = False
versionGreaterEq _  [] = True
versionGreaterEq [] _  = False
versionGreaterEq _ _ = Misc.errorstar "Interface.versionGreater called with bad arguments"

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
                Nothing            -> ([] , t  )

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
smtDefineFunc me name params rsort e =
  let env = seData (ctxSymEnv me)
   in interact' me $
        DefineFunc
          name
          (map (sortSmtSort False env <$>) params)
          (sortSmtSort False env rsort)
          e

-----------------------------------------------------------------
-- Async calls to the smt
--
-- See Note [Async SMT API]
-----------------------------------------------------------------

asyncCommand :: Context -> Command -> IO ()
asyncCommand me cmd = do
  let env = ctxSymEnv me
      cmdText = {-# SCC "asyncCommand-runSmt2" #-} Builder.toLazyText $ runSmt2 env cmd
  asyncPutStrLn (ctxTVar me) cmdText
  maybe (return ()) (`LTIO.hPutStrLn` cmdText) (ctxLog me)
  where
    asyncPutStrLn :: TVar Builder.Builder -> LT.Text -> IO ()
    asyncPutStrLn tv t = atomically $
      modifyTVar tv (`mappend` (Builder.fromLazyText t `mappend` Builder.fromString "\n"))

smtAssertAsync :: Context -> Expr -> IO ()
smtAssertAsync me p  = asyncCommand me $ Assert Nothing p

smtCheckUnsatAsync :: Context -> IO ()
smtCheckUnsatAsync me = asyncCommand me CheckSat

smtBracketAsyncAt :: SrcSpan -> Context -> String -> IO a -> IO a
smtBracketAsyncAt sp x y z = smtBracketAsync x y z `catch` dieAt sp

smtBracketAsync :: Context -> String -> IO a -> IO a
smtBracketAsync me _msg a   = do
  smtPushAsync me
  r <- a
  smtPopAsync me
  return r

smtPushAsync, smtPopAsync   :: Context -> IO ()
smtPushAsync me = asyncCommand me Push
smtPopAsync me = asyncCommand me Pop

-----------------------------------------------------------------

{-# SCC readCheckUnsat #-}
readCheckUnsat :: Context -> IO Bool
readCheckUnsat me = respSat <$> smtRead me

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


makeTimeout :: Config -> [LT.Text]
makeTimeout cfg
  | Just i <- smtTimeout cfg = [ LT.pack ("\n(set-option :timeout " ++ (show i) ++ ")\n")]
  | otherwise                = [""]


makeMbqi :: Config -> [LT.Text]
makeMbqi cfg
  | gradual cfg = [""]
  | otherwise   = ["\n(set-option :smt.mbqi false)"]

-- DON'T REMOVE THIS! z3 changed the names of options between 4.3.1 and 4.3.2...
z3_432_options :: [LT.Text]
z3_432_options
  = [ "(set-option :auto-config false)"
    , "(set-option :model true)"
    , "(set-option :model.partial false)"]

z3_options :: [LT.Text]
z3_options
  = [ "(set-option :auto-config false)"
    , "(set-option :model true)"
    , "(set-option :model-partial false)"]



--------------------------------------------------------------------------------
declare :: Context -> IO () -- SolveM ()
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

