{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Language.Fixpoint.CounterExample
  ( tryCounterExample
  , dbg
  ) where

import Language.Fixpoint.Types hiding (exit)
import Language.Fixpoint.Types.Config (Config, srcFile, queryFile, save)
import Language.Fixpoint.Solver.Sanitize (symbolEnv)
import Language.Fixpoint.Misc (ensurePath)
import Language.Fixpoint.SortCheck (elaborate)

import qualified Language.Fixpoint.Utils.Files as Ext
import qualified Language.Fixpoint.Smt.Interface as SMT

import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont

import Text.PrettyPrint.HughesPJ ((<+>), ($+$))
import qualified Text.PrettyPrint.HughesPJ as PP

-- | Multiple counter examples indexed per constraint id.
type CounterExamples = HashMap SubcId CounterExample

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
data Body = Body SubcId [Statement]
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
-- | A counter example for a model.
type CounterExample = Subst

-- | The enviroment used to build a program.
data BuildEnv info = BuildEnv
  { info :: SInfo info
  -- ^ The horn constraints from which we build the program.
  , symbols :: SymEnv
  -- ^ Contains the sorts of symbols, which we need for declarations.
  }

-- | The monad used to convert a set of horn constraints to
-- the imperative function format. See Prog for the format.
type MonadBuild info m = (MonadReader (BuildEnv info) m, MonadState Prog m, MonadIO m)

-- | Environment for the counter example generation.
data CheckEnv = CheckEnv
  { program :: Prog
  -- ^ The program we are checking
  , context :: SMT.Context
  -- ^ The SMT context we write the constraints from the program to.
  , maxDepth :: Int
  -- ^ The maximum number of functions to traverse (to avoid state blow-up).
  }

-- | State tracked when checking a program.
data CheckState = CheckState
  { uniqueId :: Int
  -- ^ Unique identifier used to avoid clashing names.
  , depth :: Int
  -- ^ Current depth (i.e. number of functions traversed)
  }

-- | The monad used to generate counter examples from a Prog.
type MonadCheck m = (MonadReader CheckEnv m, MonadState CheckState m, MonadCont m, MonadIO m)

-- TODO: remove this on code cleanup
dbg :: (MonadIO m, PPrint a) => a -> m ()
dbg = liftIO . print . pprint

-- TODO: Perhaps split the two parts (building and checking)
-- into two separate files, as they share no functions.
-- (except for the program types, which I suppose goes into a
-- separate file then.)

-- TODO: Remove variables from the counter example that got mapped to
-- the "wrong" type in smt format (e.g. to an int while not being one).

-- | Try to get a counter example for the given unsafe clauses (if any).
tryCounterExample
  :: (MonadIO m, Fixpoint info)
  => Config
  -> SInfo info
  -> Result (SubcId, info)
  -> m (Result (SubcId, info))
tryCounterExample cfg si res@Result
  { resStatus = Unsafe _ cids'
  , resCntExs = cexs'
  } = do
    let cids = map fst cids'
    prog <- hornToProg cfg si
    subs <- checkProg cfg si prog cids
    let cexs = cexBindIds si <$> subs
    return res { resCntExs = cexs <> cexs' }
tryCounterExample _ _ res@_ = return res

-- | Map a counter example to use the BindId instead of the
-- variable name as the key.
--
-- In other words, we go from a mapping of Symbol |-> Expr to
-- BindId |-> Expr
cexBindIds :: SInfo info -> CounterExample -> BindMap Expr
cexBindIds si (Su cex) = Map.compose cex bindings 
  where
    bindings = fst' <$> beBinds (bs si)
    fst' (sym, _, _) = sym

-- | Check the given constraints to try and find a counter example.
checkProg :: MonadIO m => Config -> SInfo info -> Prog -> [SubcId] -> m CounterExamples
checkProg cfg si prog cids = withContext cfg si check
  where
    check ctx = runCheck cids CheckEnv
      { program = prog
      , context = ctx
      , maxDepth = 100 -- TODO: Perhaps this should be a parameter for the user?
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
runCheck :: MonadIO m => [SubcId] -> CheckEnv -> m CounterExamples
runCheck cids env = rd . st . ct $ checkAll cids
  where
    st = flip evalStateT $ CheckState 0 0
    rd = flip runReaderT env
    ct = flip runContT return

-- | Try to find a counter example for all the given constraints.
checkAll :: MonadCheck m => [SubcId] -> m CounterExamples
checkAll cids = do
  cexs <- forM cids checkConstraint
  return $ Map.fromList [(cid, cex) | (cid, Just cex) <- zip cids cexs]

-- | Check a specific constraint id. This will only do actual
-- checks for constraints without a KVar on the rhs, as we cannot
-- really generate a counter example for these constraints.
checkConstraint :: MonadCheck m => SubcId -> m (Maybe CounterExample)
checkConstraint cid = do
  Func sig bodies <- getFunc mainName
  let cmp (Body bid _) = bid == cid
  case find cmp bodies of
    Just body -> smtScope $ checkBody sig body
    Nothing -> return Nothing

-- | Perform a satisfiability check over the body, producing
-- a counter example if the model is not valid.
checkBody :: MonadCheck m => Signature -> Body -> m (Maybe CounterExample)
checkBody sig body = callCC $ \exit -> do
  -- Produce the assertions for this body.
  Su sub <- runBody sig body
  -- Get the variables of interest (the ones declared in the body).
  --
  -- TODO: We just get the main variables here, but this causes us
  -- to sometimes miss the essential assignment of a counter example.
  --
  -- Instead, I want to report on the first assignment of all variables
  -- (including the ones in kvars). I stress the first, as there might
  -- be many assignments to the same variable otherwise due to recursive
  -- calls. The first assignment is likely then one that produces all
  -- other assignments.
  let smtSyms = [sym | (_, EVar sym) <- Map.toList sub]

  -- Check satisfiability
  ctx <- reader context
  valid <- liftIO $ SMT.smtCheckUnsat ctx

  -- Early return if the formula was valid (thus no counter example).
  when valid $ exit Nothing

  -- Counter example (unique safe symbols |-> instance)
  Su ex <- SMT.smtGetValues ctx smtSyms

  -- From here, remap the counter example to program symbols. We got
  -- it in smt "safe" symbols, which we cannot directly translate
  -- back. Hence, we use the substitution maps we have at hand.

  -- Rename a symbol to its safe version, if applicable.
  let rename (EVar sym) = Just . symbol . symbolSafeText $ sym
      rename _          = Nothing
  -- Rename all symbols (prog symbols |-> unique safe symbols)
  let sub' = (Map.mapMaybe rename sub)
  -- Final map (prog symbols |-> instance)
  return $ Just (Su $ Map.compose ex sub')

-- | Add constraints by "running" over the function. This function
-- does so in a depth first way, joining the assignments of
-- the arguments.
--
-- To avoid infinite recursion for cyclic constraints, we stop
-- recursing on a function after running it 'maxDepth' times.
runFunc :: MonadCheck m => Name -> Args -> m ()
runFunc name args = unlessMaxDepth $ do
  -- Get the function to check.
  Func sig bodies <- getFunc name

  -- Check all bodies of this function in a breath first manner.
  -- Each body generates a fresh set of signature variables.
  subs <- forM bodies $ runBody sig

  -- Joins all of these fresh variables to their argument.
  joinSubs args sig subs

-- | Track the depth and only call the given monad if we do not
-- exceed the maximum depth. Used to avoid infinite recursion
-- and general explosion of calls.
unlessMaxDepth :: MonadCheck m => m () -> m ()
unlessMaxDepth m = do
  limit <- reader maxDepth
  cur <- gets depth

  modify $ \s -> s { depth = cur + 1 }
  when (limit > cur) $ m
  modify $ \s -> s { depth = cur }

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
runBody sig (Body _ body) = do
  sub <- uniqueSig sig
  foldM runStatement sub body

-- | Write a statement to smt solver, possibly recursing
-- if the statement was a call.
--
-- Declarations will change the substitution map, as
-- declarations get name mangled to avoid name clashes.
runStatement :: MonadCheck m => Subst -> Statement -> m Subst
runStatement sub stmt = callCC $ \exit -> do
  let stmt' = subst sub stmt
  -- Run over the program and assert statements in SMT solver.
  case stmt' of
    Call name app -> runFunc name app
    Assume e      -> smtAssume $ subst sub e
    Assert e      -> smtAssert $ subst sub e
    -- We early return a new substitution map here with the continuation.
    Let decl      -> smtDeclare sub decl >>= exit
  return sub

-- | Generate unique symbols for a function signature.
uniqueSig :: MonadCheck m => Signature -> m Subst
uniqueSig = foldM smtDeclare (Su Map.empty)

-- | Returns a unique version of the received symbol.
uniqueSym :: MonadCheck m => Symbol -> m Symbol
uniqueSym sym = do
  -- Get unique number
  unique <- gets uniqueId
  modify $ \s -> s { uniqueId = unique + 1 }

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
  -- Make the lhs of the clause into statements
  lhs <- hornLhsToStmts horn

  -- The rhs has a special case depending on
  -- if it is a kvar or not.
  (name, decl, rhs) <- case crhs horn of
    PKVar kvar sub -> do
      decl <- getSig kvar
      rhs <- substToStmts sub
      return (kvar, decl, rhs)
    e@_ -> return (mainName, [], [Assert e])

  let cid = fromMaybe (-1) $ sid horn
  let body = Body cid $ lhs <> rhs
  addFunc name $ Func decl [body]

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
substToStmts :: MonadBuild info m => Subst -> m [Statement]
substToStmts (Su sub) = do
  let asEq (ksym, e) = Assume $ PAtom Eq (EVar ksym) e
  return $ map asEq (Map.toList sub)

-- | Converts the left hand side of the horn clause to a list
-- of assumptions (or calls given by a Name)
hornLhsToStmts :: MonadBuild info m => SimpC info -> m [Statement]
hornLhsToStmts horn = do
  bindEnv <- reader $ bs . info
  let lhs = clhs bindEnv horn
  stmts <- forM lhs $ uncurry reftToStmts
  return $ mconcat stmts

-- | Map a refinement to a declaration and constraint pair
reftToStmts :: MonadBuild info m => Symbol -> SortedReft -> m [Statement]
reftToStmts sym RR
  { sr_sort = sort
  , sr_reft = Reft (v, e)
  } = do
    -- Make constraint with proper substitution
    let sub = Su . Map.singleton v $ EVar sym

    sort' <- elaborateSort sort
    let decl = Let $ Decl sym sort'

    let constraints = case predKs e of
          [] -> [Assume e]
          ks -> map (\(name, app) -> Call name app) ks

    return $ decl : subst sub constraints

-- | Get the kvars from an expression.
--
-- I think this should be the only way in which kvars appear?
-- Otherwise, this should be changed!
predKs :: Expr -> [(KVar, Subst)]
predKs (PAnd ps)    = mconcat $ map predKs ps
predKs (PKVar k su) = [(k, su)]
predKs _            = []

-- | The sorts for the apply monomorphization only match if
-- we do this elaborate on the sort. Not sure why...
--
-- This elaboration also happens inside the declaration
-- of the symbol environment, so that's where I got the idea.
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
  pprintTidy tidy (Body cid stmts) = pcid $+$ pstmts
    where
      pcid = PP.text "// id" <+> pprintTidy tidy cid
      pstmts = PP.vcat
             . map (pprintTidy tidy)
             $ stmts

instance PPrint Statement where
  pprintTidy tidy (Let decl) = PP.text "let" <+> pprintTidy tidy decl
  pprintTidy tidy (Assume exprs) = PP.text "assume" <+> pprintTidy tidy exprs
  pprintTidy tidy (Assert exprs) = PP.text "assert" <+> pprintTidy tidy exprs
  pprintTidy tidy (Call kvar sub) = pprintTidy tidy kvar <+> pprintTidy tidy sub

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
