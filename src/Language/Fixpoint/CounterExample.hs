{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Language.Fixpoint.Types.Config (Config)
--import Language.Fixpoint.Types.Config (Config, srcFile)
import Language.Fixpoint.Solver.Sanitize (symbolEnv)

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

-- | The monad used to convert a set of horn constraints to
-- the imperative function format. See Prog for the format.
type MonadBuild info m = (MonadState Prog m, MonadReader (SInfo info) m, Fixpoint info, MonadIO m)

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

counterExample :: Config -> SInfo info -> Prog -> IO ()
counterExample cfg si prog = do
  -- let file = srcFile cfg
  let file = "tests/pos/testfile"
  let env = symbolEnv cfg si
  ctx <- SMT.makeContextWithSEnv cfg file env

  runCheck CheckEnv
    { program = prog
    , context = ctx
    }
  valid <- SMT.smtCheckUnsat ctx

  putStr "Valid: "
  print valid
  when (not valid) (SMT.smtGetModel ctx >>= print)

  SMT.cleanupContext ctx

-- | Runs the program checker with the monad stack
-- unwrapped.
runCheck :: CheckEnv -> IO ()
runCheck = runReaderT $ evalStateT checkProg 0

-- | Check the main function.
checkProg :: MonadCheck m => m ()
checkProg = checkFunc mainName (Su Map.empty)

-- TODO: Go up to k depth (per function) to avoid infinite recursion.
checkFunc :: MonadCheck m => Name -> Args -> m ()
checkFunc name args = do
  -- Apply arguments to function
  Func sig bodies <- getFunc name

  -- Each body generates a fresh set of signature variables
  subs <- forM bodies $ checkBody sig

  -- Joins all of these fresh variables to their argument.
  joinSubs args sig subs

-- | Join all substitution mappings made by running over all
-- bodies.
--
-- The join is a conjunct of all possible assignments.
joinSubs :: MonadCheck m => Args -> Signature -> [Subst] -> m ()
joinSubs args sig subs = do
  possible <- forM subs $ conjunctSub args sig
  smtAssert $ POr possible

-- | Map the arguments to a substitution of a single function body.
conjunctSub :: MonadCheck m => Args -> Signature -> Subst -> m Expr
conjunctSub (Su args) sig (Su sub) = do
  -- Generate symbol as shorthand for the conjunct
  bool <- smtFresh boolSort
  let bool' = EVar bool

  -- Generate the conjunct of the argument mapping
  let eq (Decl sym _) = PAtom Eq (args Map.! sym) (sub Map.! sym)
  let conjunct = PAnd $ map eq sig

  -- Assert equality between shorthand and conjunct
  smtAssert $ PAtom Eq bool' conjunct
  return bool'

-- | Get a function from the program given its name.
getFunc :: MonadCheck m => Name -> m Func
getFunc name = do
  Prog prog <- reader program
  return $ prog Map.! name

-- | Run the checker of a body. Creating new instances
-- of the signature
checkBody :: MonadCheck m => Signature -> Body -> m Subst
checkBody sig (Body body) = do
  sub <- uniqueSig sig
  _ <- foldM statementSMT sub body
  return sub

-- | Write a statement to smt solver, possibly recursing
-- if the statement was a call.
--
-- Declarations will change the substitution map, as
-- declarations get name mangled to avoid name clashes.
statementSMT :: MonadCheck m => Subst -> Statement -> m Subst
statementSMT sub stmt = do
  let stmt' = subst sub stmt
  -- Run over the program and assert statements in SMT solver.
  case stmt' of
    Call name app -> checkFunc name app
    Assume e      -> smtAssert $ subst sub e
    Assert e      -> smtAssume $ subst sub e
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
  sym <- uniqueSym "fresh@"
  liftIO $ SMT.smtDecl ctx sym sort
  return sym

-- | Assume the given expression.
smtAssume :: MonadCheck m => Expr -> m ()
smtAssume = smtAssert . PNot

-- | Assert the given expression.
smtAssert :: MonadCheck m => Expr -> m ()
smtAssert e = do
  ctx <- reader context
  liftIO $ SMT.smtAssert ctx e

-- TODO: remove this on code cleanup
dbg :: (MonadIO m, PPrint a) => a -> m ()
dbg = liftIO . print . pprint

-- | Make an imperative program from horn clauses. This
-- can be used to generate a counter example.
hornToProg :: (MonadIO m, Fixpoint info) => SInfo info -> m Prog
hornToProg si = execStateT (runReaderT go si) prog
  where
    -- Initial program has empty main
    prog = Prog $ Map.singleton mainName (Func [] [])
    -- Add all horn clauses in SInfo to the function map
    go = reader cm >>= mapM_ addHorn

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
  wfcs <- reader ws
  let wfc = wfcs Map.! kvar

  -- Get the bind environment and bindings of the wfc
  bindEnv <- reader bs
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
  bindEnv <- reader bs
  let lhs = clhs bindEnv horn
  return $ lhsToBody lhs

-- | Map a complete lhs of a horn clause to a function body
lhsToBody :: [(Symbol, SortedReft)] -> Body
lhsToBody = mconcat . map (uncurry reftToBody)

-- | Map a refinement to a declaration and constraint pair
reftToBody :: Symbol -> SortedReft -> Body
reftToBody sym RR
  { sr_sort = sort
  , sr_reft = Reft (v, e)
  } = Body
    [ Let (Decl sym sort)
    , constraint
    ]
  where
    -- Make constraint with proper substitution
    constraint = case e of
      PKVar kvar (Su app) -> Call kvar (Su $ subst sub app)
      _ -> Assume $ subst sub e

    sub = Su . Map.singleton v $ EVar sym

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
  syms (Let (Decl sym _)) = [sym]
  syms (Assume e) = syms e
  syms (Assert e) = syms e
  syms (Call _ (Su sub)) = syms sub

  substa f (Assume e) = Assume $ substa f e
  substa f (Assert e) = Assert $ substa f e
  substa f (Call name (Su sub)) = Call name (Su $ substa f sub)
  substa _ decl@_ = decl

  substf f (Assume e) = Assume $ substf f e
  substf f (Assert e) = Assert $ substf f e
  substf f (Call name (Su sub)) = Call name (Su $ substf f sub)
  substf _ decl@_ = decl

  subst sub (Assume e) = Assume $ subst sub e
  subst sub (Assert e) = Assert $ subst sub e
  subst sub (Call name (Su sub')) = Call name (Su $ subst sub sub')
  subst _ decl@_ = decl

-- fn k1(x: Int) {
--   assume x == 11
-- ||
--   assume x == 12
-- }
-- 
-- fn k2(x: Int) {
--   assume x == 21
-- ||
--   assume x == 22
-- }
-- 
-- fn main() {
--   let y : Int
--   let z : Int
--   k1(y)
--   k2(z)
--   assert y != z
-- }
-- 
-- -- BFS vs DFS
-- -- - BFS: brings much more in scope at a time
-- -- - DFS: has much more calls
-- --
-- -- - BFS: Name clashing is much worse
-- -- - DFS: The way to explore the program is very non-obvious...
-- --
-- -- For now do BFS as the impl seems easier
-- 
-- -- Breadth First Search
-- (declare-const y Int)
-- (declare-const z Int)
-- 
-- -- k1 body 0
-- (declare-const x$b0$k1 Int)
-- (assert (= x$b0$k1 11))
-- 
-- -- k1 body 1
-- (declare-const x$b1$k1 Int)
-- (assert (= x$b1$k1 12))
-- 
-- -- Union of k1 call (Not sure if this is sound)
-- (assert (or (= x$b0$k1 y) (= x$b1$k1 y)))
-- 
-- -- k2 body 0
-- (declare-const x$b0$k2 Int)
-- (assert (= x$b0$k1 21))
-- 
-- -- k2 body 1
-- (declare-const x$b1$k2 Int)
-- (assert (= x$b1$k1 22))
-- 
-- -- Union of k2 call
-- (assert (or (= x$b0$k2 z) (= x$b1$k2 z)))
-- 
-- -- Final assertion
-- (assert (not (!= y z)))
-- (check-sat)
-- 
-- -- Depth First Search
-- (declare-const y Int)
-- (declare-const z Int)
-- 
-- (push 1) -- call k1
--   (declare-const x$k1 Int)
--   (assert (= x$k1 11))
--   (push 1) -- call k2
--     (declare-const x$k2 Int)
--     (assert (= x$k2 21))
--     (assert (= x$k1 y))
--     (assert (= x$k2 z))
--     (assert (not (!= y z))) -- final assertion
--     (check-sat)
--   (pop 1)
--   (push 1) -- call k2
--     (declare-const x$k2 Int)
--     (assert (= x$k2 22))
--     (assert (= x$k1 y))
--     (assert (= x$k2 z))
--     (assert (not (!= y z))) -- final assertion
--     (check-sat)
--   (pop 1)
-- (pop 1)
-- 
-- (push 1) -- call k1
--   (declare-const x$k1 Int)
--   (assert (= x$k1 12)) --
--   (push 1) -- call k2
--     (declare-const x$k2 Int)
--     (assert (= x$k2 21))
--     (assert (= x$k1 y))
--     (assert (= x$k2 z))
--     (assert (not (!= y z))) -- final assertion
--     (check-sat)
--   (pop 1)
--   (push 1) -- call k2
--     (declare-const x$k2 Int)
--     (assert (= x$k2 22))
--     (assert (= x$k1 y))
--     (assert (= x$k2 z))
--     (assert (not (!= y z))) -- final assertion
--     (check-sat)
--   (pop 1)
-- (pop 1)
--
-- fn $k1 (lq_karg$v0: Int) {
--     let v : Int
--     assume v == 1
--     assume v == lq_karg$v0
-- ||
--     let x : Int
--     assume v == 2
--     assume v == lq_karg$v0
-- }
--
-- fn $main () {
--     let z : Int
--     $k1 [lq_karg$v0 := z]
--     assert z > 0
-- }
--
-- (declare-var z-->0 Int)            -- From main
-- (declare-var lq_karg$v0-->1 Int)   -- Unique karg from k1 call
-- (assert (= lq_karg$v0-->1 z))      -- From call to k1 substitution
--
-- (declare-var lq_karg$v0-->2 Int)   -- From first body
-- [..]
--
-- (declare-var lq_karg$v0-->3 Int)   -- From second body
-- [..]
--
-- -- Assertion combining all versions of the karg
-- (assert (or (= lq_karg$v0-->1 lq_karg$v0-->2) (= lq_karg$v0-->1 lq_karg$v0-->3))
