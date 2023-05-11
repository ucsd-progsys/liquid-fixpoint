{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Fixpoint.CounterExample
  ( hornToProg
  , mainName

  , counterExample
  , dbg

  , Prog
  , Func (..)
  , Decl
  , Body
  , Statement (..)
  ) where

import Language.Fixpoint.Types
import Language.Fixpoint.Types.Config (Config, srcFile, queryFile, save)
import Language.Fixpoint.Solver.Sanitize (symbolEnv)
import Language.Fixpoint.Misc (ensurePath)
import Language.Fixpoint.SortCheck (elaborate)

import qualified Language.Fixpoint.Utils.Files as Ext
import qualified Language.Fixpoint.Smt.Interface as SMT

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Control.Monad.State
import Control.Monad.Reader

import Text.PrettyPrint.HughesPJ ((<+>), ($+$))
import qualified Text.PrettyPrint.HughesPJ as PP

-- | A program, containing multiple function definitions
-- mapped by their name.
data Prog = Prog (HashMap Name Func)
  deriving Show

-- | Identifier of a function. All KVars are translated
-- into functions, so it is just an alias.
type Name = KVar

-- | A function symbol corresponding to a Name.
data Func = Func Signature [Body]
  deriving Show

-- | A declaration of a Symbol with a Sort.
data Decl = Decl Symbol Sort
  deriving Show

-- | A sequence of statements.
data Body = Body [Statement]
  deriving Show

-- | A statement used to introduce/check constraints.
data Statement
  = Let Decl
  -- ^ Introduces a new variable.
  | Assume Expr
  -- ^ Constraints a variable.
  | Assert Expr
  -- ^ Checks whether a predicate follows given prior constraints.
  | Call Name Subst
  -- ^ Call to function.
  deriving Show

-- | Arguments to a function.
type Args = Subst
-- | Signature of a function.
type Signature = [Decl]

-- | The enviroment used to build a program.
data BuildEnv info = BuildEnv
  { info :: SInfo info
  -- ^ The horn constraints from which we build the program.
  , symbols :: SymEnv
  -- ^ Contains the sorts of symbols, which we need for declarations.
  }

-- | The monad used to convert a set of horn constraints to
-- the imperative function format. See Prog for the format.
type MonadBuild info m = (MonadState Prog m, MonadReader (BuildEnv info) m, MonadIO m)

-- | Environment for the counter example generation.
data CheckEnv = CheckEnv
  { program :: Prog
  -- ^ The program we are checking
  , context :: SMT.Context
  -- ^ The SMT context we write the constraints from the program to.
  }

-- | Unique identifier used to avoid clashing names.
type UniqueId = Int

-- | The monad used to generate counter examples from a Prog.
type MonadCheck m = (MonadReader CheckEnv m, MonadState UniqueId m, MonadIO m)

-- | Try to get a counter example for the given constraints.
counterExample :: (MonadIO m, Fixpoint info) => Config -> SInfo info -> m [Maybe Subst]
counterExample cfg si = do
  prog <- hornToProg cfg si
  checkProg cfg si prog

-- | Checks the given program, returning a counter example
-- (if it can find one).
--
-- TODO: Actually return a counter example!
checkProg :: MonadIO m => Config -> SInfo info -> Prog -> m [Maybe Subst]
checkProg cfg si prog = withContext cfg si check
  where
    check ctx = runCheck CheckEnv
      { program = prog
      , context = ctx
      }

-- | Run the checker with the SMT solver context.
withContext :: MonadIO m => Config -> SInfo info -> (SMT.Context -> m a) -> m a
withContext cfg si inner = do
  let file = srcFile cfg <> ".prog"
  let env = symbolEnv cfg si
  ctx <- liftIO $ SMT.makeContextWithSEnv cfg file env

  !result <- inner ctx

  liftIO $ SMT.cleanupContext ctx
  return result

-- | Runs the program checker with the monad stack
-- unwrapped.
runCheck :: MonadIO m => CheckEnv -> m [Maybe Subst]
runCheck = runReaderT $ evalStateT checkMain 0

-- | Check the main function. Each branch in the main
-- function needs to be checked separately.
checkMain :: MonadCheck m => m [Maybe Subst]
checkMain = do
  Func sig bodies <- getFunc mainName
  forM bodies $ smtScope . checkBody sig

-- | Perform a satisfiability check over the body, producing
-- a counter example if the model is not valid.
checkBody :: MonadCheck m => Signature -> Body -> m (Maybe Subst)
checkBody sig body = do
  -- Produce the assertions for this body.
  -- Substitution map contains (prog symbols |-> unique symbols)
  Su sub <- runBody sig body
  -- Get the variables of interest (the ones declared in the body).
  let smtSyms = [sym | (_, EVar sym) <- Map.toList sub]

  -- Check satisfiability
  ctx <- reader context
  valid <- liftIO $ SMT.smtCheckUnsat ctx

  -- Get a counter example if not valid.
  ex <- if | not valid -> SMT.smtGetValues ctx smtSyms >>= return . Just
           | otherwise -> return Nothing

  -- Remap the counter example to program symbols. We got it in
  -- smt "safe" symbols, which we cannot directly translate back.
  -- Hence, we use the substitution maps we have at hand.
  return $ do
    -- Map from (unique safe symbols |-> instance)
    Su instances <- ex
    -- Rename a symbol to its smt2 version.
    let rename = symbol . symbolSafeText
    -- Does remapping from smt name to prog name.
    let remap (EVar sym) = Map.lookup (rename sym) instances
        remap _          = Nothing
    -- Final map from (prog symbols |-> instance)
    return $ Su (Map.mapMaybe remap sub)

-- TODO: Go up to k depth (per function) to avoid infinite recursion
-- with cyclic kvars.
runFunc :: MonadCheck m => Name -> Args -> m ()
runFunc name args = do
  -- Get the function to check.
  Func sig bodies <- getFunc name

  -- Check all bodies of this function in a breath first manner.
  -- Each body generates a fresh set of signature variables.
  subs <- forM bodies $ runBody sig

  -- Joins all of these fresh variables to their argument.
  joinSubs args sig subs

-- | Join all substitution mappings made by running over all
-- bodies.
--
-- The join is a disjunct of all possible assignments.
joinSubs :: MonadCheck m => Args -> Signature -> [Subst] -> m ()
joinSubs args sig subs = do
  possible <- forM subs $ conjunctSub args sig
  smtAssert $ POr possible

-- | Map the arguments to a substitution of a single function body.
--
-- To elaborate, given the following variables:
-- The mapping of the arguments.
-- -- ^ (sym |-> arg) :: Args
--
-- The arguments themselves.
-- -- ^ [sym] :: Signature
--
-- The unique mapping generated for a body.
-- -- ^ (sym |-> unique) :: Subst
--
-- We generate a conjunct of (arg == unique) for every symbol in
-- the signature.
conjunctSub :: MonadCheck m => Args -> Signature -> Subst -> m Expr
conjunctSub (Su args) sig (Su sub) = do
  -- Generate symbol as shorthand for the conjunct.
  bool <- smtFresh boolSort
  let bool' = EVar bool

  -- Generate the conjunct of the argument mapping.
  let eq (Decl sym _) = PAtom Eq (args Map.! sym) (sub Map.! sym)
  let conjunct = PAnd $ map eq sig

  -- Assert equality between shorthand and conjunct.
  smtAssert $ PAtom Eq bool' conjunct
  return bool'

-- | Get a function from the program given its name.
getFunc :: MonadCheck m => Name -> m Func
getFunc name = do
  Prog prog <- reader program
  return $ prog Map.! name

-- | Run the checker over a body. Creating new instances
-- of the signature and elaborating the statements in
-- it to the smt solver.
--
-- The returned substitution map contains all variables
-- that were renamed during the run. This includes the
-- signature as well as all the declarations in the body.
runBody :: MonadCheck m => Signature -> Body -> m Subst
runBody sig (Body body) = do
  sub <- uniqueSig sig
  foldM runStatement sub body

-- | Write a statement to smt solver, possibly recursing
-- if the statement was a call.
--
-- Declarations will change the substitution map, as
-- declarations get name mangled to avoid name clashes.
runStatement :: MonadCheck m => Subst -> Statement -> m Subst
runStatement sub stmt = do
  let stmt' = subst sub stmt
  -- Run over the program and assert statements in SMT solver.
  case stmt' of
    Call name app -> runFunc name app
    Assume e      -> smtAssume $ subst sub e
    Assert e      -> smtAssert $ subst sub e
    _             -> return ()

  -- A declaration will also adjust the substitution
  -- map and is thus separated here.
  case stmt of
    Let decl -> smtDeclare sub decl
    _        -> return sub

-- | Generate unique symbols for a function signature.
uniqueSig :: MonadCheck m => Signature -> m Subst
uniqueSig = foldM smtDeclare (Su Map.empty)

-- | Returns a unique version of the received symbol.
uniqueSym :: MonadCheck m => Symbol -> m Symbol
uniqueSym sym = do
  -- Get unique number
  unique <- get
  put $ unique + 1

  -- Apply unique number to identifier
  let (<.>) = suffixSymbol
  let unique' = symbol . show $ unique
  return $ sym <.> "@" <.> unique'

-- | Declare a new symbol, returning an updated substitution
-- given with this new symbol in it. The substitution map is
-- required to avoid duplicating variable names.
smtDeclare :: MonadCheck m => Subst -> Decl -> m Subst
smtDeclare (Su sub) (Decl sym sort) = do
  ctx <- reader context
  sym' <- uniqueSym sym
  liftIO $ SMT.smtDecl ctx sym' sort
  return (Su $ Map.insert sym (EVar sym') sub)

-- | Declare a fresh symbol, not derived from a declaration.
smtFresh :: MonadCheck m => Sort -> m Symbol
smtFresh sort = do
  ctx <- reader context
  sym <- uniqueSym "fresh"
  liftIO $ SMT.smtDecl ctx sym sort
  return sym

-- | Assume the given expression.
smtAssert :: MonadCheck m => Expr -> m ()
smtAssert = smtAssume . PNot

-- | Assert the given expression.
smtAssume :: MonadCheck m => Expr -> m ()
smtAssume e = do
  ctx <- reader context
  liftIO $ SMT.smtAssert ctx e

-- | Run the checker within a scope (i.e. a push/pop pair).
smtScope :: MonadCheck m => m a -> m a
smtScope inner = do
  ctx <- reader context
  liftIO $ SMT.smtPush ctx
  !result <- inner
  liftIO $ SMT.smtPop ctx
  return result

-- TODO: remove this on code cleanup
dbg :: (MonadIO m, PPrint a) => a -> m ()
dbg = liftIO . print . pprint

-- | Make an imperative program from horn clauses. This
-- can be used to generate a counter example.
hornToProg :: MonadIO m => Config -> SInfo info -> m Prog
hornToProg cfg si = do
  -- Initial program is just an empty main.
  let initial = Prog $ Map.singleton mainName (Func [] [])
  let env = BuildEnv
       { info = si
       , symbols = symbolEnv cfg si
       }

  -- Run monad that adds all horn clauses
  prog <- evalStateT (runReaderT buildProg env) initial

  -- Save the program in a file
  liftIO . when (save cfg) $ do
    let file = queryFile Ext.Prog cfg
    ensurePath file
    writeFile file $ PP.render (pprint prog)

  -- Return the generated program
  return prog

-- | Build the entire program structure from the constraints
-- inside the monad
buildProg :: MonadBuild info m => m Prog
buildProg = do
  constraints <- reader $ cm . info
  mapM_ addHorn constraints
  get

-- | Given a horn clause, generates a body for a function.
--
-- The body is generated from the lhs of the horn clause.
--
-- This body is added to the function given by the kvar
-- on the rhs of the horn clause. If there was no kvar,
-- it is added to the main function.
addHorn :: MonadBuild info m => SimpC info -> m ()
addHorn horn = do
  (name, func) <- case crhs horn of
    PKVar kvar sub -> do
      decl <- getSig kvar
      body <- hornLhsToBody horn
      body' <- substToBody sub
      return (kvar, Func decl [body <> body'])
    rhs@_ -> do
      Body stmts <- hornLhsToBody horn
      let body = Body $ stmts <> [Assert rhs]
      return (mainName, Func [] [body])
  addFunc name func

-- | Gets a signature of a KVar from its well foundedness constraint
getSig :: MonadBuild info m => Name -> m Signature
getSig kvar = do
  -- Get the well foundedness constraint of the kvar
  wfcs <- reader $ ws . info
  let wfc = wfcs Map.! kvar

  -- Get the bind environment and bindings of the wfc
  bindEnv <- reader $ bs . info
  let ibinds = elemsIBindEnv . wenv $ wfc

  -- Lookup all Decl from the wfc using the ibinds
  let asDecl (sym, sr, _) = Decl sym (sr_sort sr)
  let decls = map (asDecl . flip lookupBindEnv bindEnv) ibinds

  -- Get the last Decl from the head of the wfc
  let rhs = let (sym, sort, _) = wrft wfc in Decl sym sort

  -- Return the head + bindings as argument map
  return $ rhs:decls

-- | Defines some equalities between local variables
-- and the passed arguments given some substitution map.
substToBody :: MonadBuild info m => Subst -> m Body
substToBody (Su sub) = do
  let asEq (ksym, e) = Assume $ PAtom Eq (EVar ksym) e
  return . Body . map asEq . Map.toList $ sub

-- | Converts the left hand side of the horn clause to a list
-- of assumptions (or calls given by a Name)
hornLhsToBody :: MonadBuild info m => SimpC info -> m Body
hornLhsToBody horn = do
  bindEnv <- reader $ bs . info
  let lhs = clhs bindEnv horn
  bodies <- forM lhs $ uncurry reftToBody
  return $ mconcat bodies

-- | Map a refinement to a declaration and constraint pair
reftToBody :: MonadBuild info m => Symbol -> SortedReft -> m Body
reftToBody sym RR
  { sr_sort = sort
  , sr_reft = Reft (v, e)
  } = do
    -- Make constraint with proper substitution
    let sub = Su . Map.singleton v $ EVar sym
    let constraint = case e of
          PKVar kvar (Su app) -> Call kvar (Su $ subst sub app)
          _ -> Assume $ subst sub e

    sort' <- elaborateSort sort
    return $ Body
     [ Let $ Decl sym sort'
     , constraint
     ]

-- | The sorts for the apply monomorphization only match if
-- we do this elaborate on the sort. Not sure why...
--
-- This elaboration also happens inside the declaration
-- of the symbol environment, so that where I got the idea.
elaborateSort :: MonadBuild info m => Sort -> m Sort
elaborateSort sym = do
  symbols' <- reader symbols
  return $ elaborate "elaborateSort" symbols' sym

-- | Add a function to the function map with index by its name.
-- If an entry already exists, it will merge the function
-- bodies.
addFunc :: MonadBuild info m => Name -> Func -> m ()
addFunc kvar func = do
  let merge (Func _ b) (Func d b') = Func d (b <> b')
  Prog prog <- get
  put . Prog $ Map.insertWith merge kvar func prog

-- | The main function, which any horn clause without a
-- KVar on the rhs will be added to.
mainName :: Name
mainName = KV "main"

instance PPrint Prog where
  pprintTidy tidy (Prog funcs) = PP.vcat
                               . PP.punctuate (PP.text "\n")
                               . map (uncurry $ ppfunc tidy)
                               . Map.toList
                               $ funcs

instance PPrint Func where
  pprintTidy tidy = ppfunc tidy anonymous
    where
      anonymous = KV "_"

ppfunc :: Tidy -> Name -> Func -> PP.Doc
ppfunc tidy name (Func sig bodies) = pdecl $+$ pbody $+$ PP.rbrace
  where
    pdecl = fn <+> pname <+> psig <+> PP.lbrace
    fn = PP.text "fn"
    pname = pprintTidy tidy name
    psig = PP.parens
           . PP.hsep
           . PP.punctuate PP.comma
           . map (pprintTidy tidy)
           $ sig
    pbody = vpunctuate (PP.text "||")
          . map (PP.nest 4 . pprintTidy tidy)
          $ bodies

    vpunctuate _ [] = mempty
    vpunctuate _ (d:[]) = d
    vpunctuate p (d:ds) = d $+$ p $+$ vpunctuate p ds

instance PPrint Decl where
  pprintTidy tidy (Decl sym sort) = (psym <> PP.colon) <+> psort
    where
      psym = pprintTidy tidy sym
      psort = pprintTidy tidy sort

instance PPrint Body where
  pprintTidy tidy (Body stmts) = PP.vcat
                               . map (pprintTidy tidy)
                               $ stmts

instance PPrint Statement where
  pprintTidy tidy (Let decl) = PP.text "let" <+> pprintTidy tidy decl
  pprintTidy tidy (Assume exprs) = PP.text "assume" <+> pprintTidy tidy exprs
  pprintTidy tidy (Assert exprs) = PP.text "assert" <+> pprintTidy tidy exprs
  pprintTidy tidy (Call kvar sub) = pprintTidy tidy kvar <+> pprintTidy tidy sub

instance Semigroup Body where
  Body b <> Body b' = Body $ b <> b'

instance Monoid Body where
  mempty = Body []

instance Subable Statement where
  syms (Let decl) = syms decl
  syms (Assume e) = syms e
  syms (Assert e) = syms e
  syms (Call _ (Su sub)) = syms sub

  substa f (Let decl) = Let $ substa f decl
  substa f (Assume e) = Assume $ substa f e
  substa f (Assert e) = Assert $ substa f e
  substa f (Call name (Su sub)) = Call name (Su $ substa f sub)

  substf f (Let decl) = Let $ substf f decl
  substf f (Assume e) = Assume $ substf f e
  substf f (Assert e) = Assert $ substf f e
  substf f (Call name (Su sub)) = Call name (Su $ substf f sub)

  subst sub (Let decl) = Let $ subst sub decl
  subst sub (Assume e) = Assume $ subst sub e
  subst sub (Assert e) = Assert $ subst sub e
  subst sub (Call name (Su sub')) = Call name (Su $ subst sub sub')

instance Subable Decl where
  syms (Decl sym _) = [sym]

  substa f (Decl sym sort) = Decl (substa f sym) sort

  substf f (Decl sym sort) = Decl (substf f sym) sort

  subst sub (Decl sym sort) = Decl (subst sub sym) sort
