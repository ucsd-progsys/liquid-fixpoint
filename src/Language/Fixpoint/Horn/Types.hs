
-------------------------------------------------------------------------------
-- | This module formalizes the key datatypes needed to represent Horn NNF
--   constraints as described in "Local Refinement Typing", ICFP 2017
-------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Fixpoint.Horn.Types
  ( -- * Horn Constraints and their components
    Query (..)
  , Cstr  (..)
  , Pred  (..)
  , Bind  (..)
  , Var   (..)

    -- * Raw Query
  , Tag (..)
  , TagVar
  , TagQuery

    -- * accessing constraint labels
  , cLabel

    -- * invariants (refinements) on constraints
  , okCstr

    -- * extract qualifiers
  , quals

  )
  where

import           Data.Generics             (Data)
import           Data.Typeable             (Typeable)
import           GHC.Generics              (Generic)
import           Control.DeepSeq ( NFData )
import qualified Data.Text               as T
import           Data.Maybe (fromMaybe)
import qualified Data.List               as L
import qualified Language.Fixpoint.Types as F
import qualified Text.PrettyPrint.HughesPJ.Compat as P
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import           Data.Aeson
import           Data.Aeson.Types

-------------------------------------------------------------------------------
-- | @HVar@ is a Horn variable
-------------------------------------------------------------------------------
data Var a = HVar
  { hvName :: !F.Symbol                         -- ^ name of the variable $k1, $k2 etc.
  , hvArgs :: ![F.Sort] {- len hvArgs > 0 -}    -- ^ sorts of its parameters i.e. of the relation defined by the @HVar@
  , hvMeta :: a                                 -- ^ meta-data
  }
  deriving (Eq, Ord, Data, Typeable, Generic, Functor, ToJSON, FromJSON)



-------------------------------------------------------------------------------
-- | @HPred@ is a Horn predicate that appears as LHS (body) or RHS (head) of constraints
-------------------------------------------------------------------------------
data Pred
  = Reft  !F.Expr                               -- ^ r
  | Var   !F.Symbol ![F.Expr]                   -- ^ $k(y1..yn)
  | PAnd  ![Pred]                               -- ^ p1 /\ .../\ pn
  deriving (Data, Typeable, Generic, Eq, ToJSON, FromJSON)

instance F.ToHornSMT (Var a) where
  toHornSMT (HVar k ts _) = P.parens ("var" P.<+> "$" P.<-> F.pprint k P.<+> F.toHornSMT ts)
instance F.ToHornSMT Pred where
  toHornSMT (Reft p)   = P.parens (F.toHornSMT p)
  toHornSMT (Var k xs) = F.toHornMany (F.toHornSMT (F.KV k) : (F.toHornSMT <$> xs))
  toHornSMT (PAnd ps)  = F.toHornMany ("and" : (F.toHornSMT <$> ps))

instance F.Subable Pred where
  syms (Reft e)   = F.syms e
  syms (Var _ xs) = F.syms xs
  syms (PAnd ps)  = F.syms ps

  substr ns su (Reft  e)  = Reft  (F.substr ns su      e)
  substr ns su (PAnd  ps) = PAnd  (F.substr ns su <$> ps)
  substr ns su (Var k xs) = Var k (F.substr ns su <$> xs)

-------------------------------------------------------------------------------
quals :: Cstr a -> [F.Qualifier]
-------------------------------------------------------------------------------
quals = F.notracepp "horn.quals" . cstrQuals F.emptySEnv F.vv_

cstrQuals :: F.SEnv F.Sort -> F.Symbol -> Cstr a -> [F.Qualifier]
cstrQuals = go
  where
    go env v (Head p _)  = predQuals env v p
    go env v (CAnd   cs) = concatMap (go env v) cs
    go env _ (All  b c)  = bindQuals env b c

bindQuals  :: F.SEnv F.Sort -> Bind a -> Cstr a -> [F.Qualifier]
bindQuals env b c = predQuals env' bx (bPred b) ++ cstrQuals env' bx c
  where
    env'          = F.insertSEnv bx bt env
    bx            = bSym b
    bt            = bSort b

predQuals :: F.SEnv F.Sort -> F.Symbol -> Pred -> [F.Qualifier]
predQuals env v (Reft p)  = exprQuals env v p
predQuals env v (PAnd ps) = concatMap (predQuals env v) ps
predQuals _   _ _         = []

exprQuals :: F.SEnv F.Sort -> F.Symbol -> F.Expr -> [F.Qualifier]
exprQuals env v e = mkQual env v <$> F.conjuncts e

mkQual :: F.SEnv F.Sort -> F.Symbol -> F.Expr -> F.Qualifier
mkQual env v p = case envSort env <$> (v:xs) of
                   (_,so):xts -> F.mkQ "Auto" ((v, so) : xts) p junk
                   _          -> F.panic "impossible"
  where
    xs         = S.toList $ S.delete v $ F.syms p
    junk       = F.dummyPos "mkQual"

envSort :: F.SEnv F.Sort -> F.Symbol -> (F.Symbol, F.Sort)
envSort env x = case F.lookupSEnv x env of
                   Just t -> (x, t)
                   _      -> F.panic $ "unbound symbol in scrape: " ++ F.showpp x
{-
  | Just _ <- lookupSEnv x lEnv = Nothing
  | otherwise                   = Just (x, ai)
  where
    ai             = trace msg $ fObj $ Loc l l $ tempSymbol "LHTV" i
    msg            = "Unknown symbol in qualifier: " ++ show x
-}


--------------------------------------------------------------------------------
-- | @Cst@ is an NNF Horn Constraint.
-------------------------------------------------------------------------------
-- Note that a @Bind@ is a simplified @F.SortedReft@ ...
data Bind a = Bind
  { bSym  :: !F.Symbol
  , bSort :: !F.Sort
  , bPred :: !Pred
  , bMeta :: !a
  }
  deriving (Data, Typeable, Generic, Functor, Eq, ToJSON, FromJSON)

instance F.ToHornSMT (Bind a) where
  toHornSMT (Bind x t p _) = P.parens (F.toHornSMT (x, t) P.<+> F.toHornSMT p)

instance F.Subable (Bind a) where
    syms     (Bind x _ p _) = S.insert x $ F.syms p
    substr ns su (Bind v t p a) =
      let (ns', v') = F.freshInNS v ns
       in
          Bind v t (F.substr ns' (F.extendSubst su v (F.EVar v')) p) a

-- Can we enforce the invariant that CAnd has len > 1?
data Cstr a
  = Head  !Pred !a                  -- ^ p
  | CAnd  ![Cstr a]                 -- ^ c1 /\ ... /\ cn
  | All   !(Bind a)  !(Cstr a)      -- ^ \all x:t. p => c
  deriving (Data, Typeable, Generic, Functor, Eq, ToJSON, FromJSON)

instance F.ToHornSMT (Cstr a) where
  toHornSMT = toHornCstr

toHornCstr :: Cstr a -> P.Doc
toHornCstr (Head p _) = F.toHornSMT p
toHornCstr (CAnd cs)  = F.toHornAnd toHornCstr cs
toHornCstr (All b c)  = P.parens (P.vcat ["forall" P.<+> F.toHornSMT b
                                         , P.nest 1 (toHornCstr c)])

cLabel :: Cstr a -> a
cLabel cstr = case go cstr of
  [] -> F.panic "everything is true!!!"
  label:_ -> label
  where
    go (Head _ l)   = [l]
    go (CAnd cs)    = mconcat $ go <$> cs
    go (All _ c)    = go c

-- We want all valid constraints to start with a binding at the top

okCstr :: Cstr a -> Bool
okCstr All {} = True
okCstr _      = False


-------------------------------------------------------------------------------
-- | @Query@ is an NNF Horn Constraint.
-------------------------------------------------------------------------------

data Query a = Query
  { qQuals :: ![F.Qualifier]             -- ^ qualifiers over which to solve cstrs
  , qVars  :: ![Var a]                   -- ^ kvars, with parameter-sorts
  , qCstr  :: !(Cstr a)                  -- ^ list of constraints
  , qCon   :: M.HashMap F.Symbol F.Sort  -- ^ list of constants (un/interpreted functions)
  , qDis   :: M.HashMap F.Symbol F.Sort  -- ^ list of *distinct* constants (uninterpreted functions)
  , qEqns  :: ![F.Equation]              -- ^ list of equations
  , qDefs  :: ![F.Equation]              -- ^ list of equations to be sent to SMT as define-fun
  , qMats  :: ![F.Rewrite]               -- ^ list of match-es
  , qData  :: ![F.DataDecl]              -- ^ list of data-declarations
  , qOpts  :: ![String]                  -- ^ list of fixpoint options
  , qNums  :: ![F.Symbol]                -- ^ list of numeric TyCon (?)
  , qKuts  :: ![F.KVar]                  -- ^ list of cut variables
  }
  deriving (Data, Typeable, Generic, Functor, ToJSON, FromJSON)

-- | Tag each query with a possible string denoting "provenance"

type TagVar   = Var Tag
type TagQuery = Query Tag
data Tag      = NoTag | Tag String
  deriving (Data, Typeable, Generic, Show)

instance NFData Tag

instance F.Loc Tag where
  srcSpan _ = F.dummySpan

instance F.Fixpoint Tag where
  toFix NoTag   = "\"\""
  toFix (Tag s) = "\"" <> P.text s <> "\""

instance F.PPrint Tag where
  pprintPrec _ _ NoTag   = mempty
  pprintPrec _ _ (Tag s) = P.ptext s

instance ToJSON Tag where
  toJSON NoTag   = Null
  toJSON (Tag s) = String (T.pack s)

instance FromJSON Tag where
  parseJSON Null       = pure NoTag
  parseJSON (String t) = pure (Tag (T.unpack t))
  parseJSON invalid    = prependFailure "parsing `Tag` failed, " (typeMismatch "Object" invalid)

instance F.ToHornSMT Tag where
  toHornSMT NoTag   = mempty
  toHornSMT (Tag s) = P.text s



instance F.PPrint (Query a) where
  pprintPrec prec t q = P.vcat $ L.intersperse " "
    [ P.vcat   (ppQual <$> qQuals q)
    , P.vcat   [ppVar k   | k <- qVars q]
    , P.vcat   [ppCon x (F.pprint sort') | (x, sort') <- M.toList (qCon q)]
    , ppThings Nothing (qEqns  q)
    , ppThings (Just "data ") (qData  q)
    , P.parens (P.vcat ["constraint", F.pprintPrec (prec+2) t (qCstr q)])
    ]




ppThings :: F.PPrint a => Maybe P.Doc -> [a] -> P.Doc
ppThings pfx qs = P.vcat [ P.parens $ prefix P.<-> F.pprint q | q <- qs]
  where
    prefix      = fromMaybe "" pfx

ppCon :: F.Symbol -> P.Doc -> P.Doc
ppCon x td = P.parens ("constant" P.<+> F.pprint x P.<+> P.parens td)

ppQual :: F.Qualifier -> P.Doc
ppQual (F.Q n xts p _) =  P.parens ("qualif" P.<+> F.pprint n P.<+> ppBlanks (ppArg <$> xts) P.<+> P.parens (F.pprint p))
  where
    ppArg qp    = P.parens $ F.pprint (F.qpSym qp) P.<+> P.parens (F.pprint (F.qpSort qp))

ppVar :: Var a -> P.Doc
ppVar (HVar k ts _)  = P.parens ("var" P.<+> "$" P.<-> F.pprint k P.<+> ppBlanks (P.parens . F.pprint <$> ts))


ppBlanks :: [P.Doc] -> P.Doc
ppBlanks ds = P.parens (P.hcat (L.intersperse " " ds))

-------------------------------------------------------------------------------
-- Pretty Printing
-------------------------------------------------------------------------------
parens :: String -> String
parens s = "(" ++ s ++ ")"

instance Show (Var a) where
  show (HVar k xs _) = show k ++ parens (unwords (show <$> xs))

instance Show Pred where
  show (Reft p)   = parens $ F.showpp p
  show (Var x xs) = parens $ "$" ++ unwords (F.symbolString x : (parens . F.showpp <$> xs))
  show (PAnd ps)  = parens $ unwords $ "and": map show ps

instance Show (Cstr a) where
  show (Head p _) = parens $ show p
  show (All b c)  = parens $ unwords ["forall" , show b , show c]
  show (CAnd cs)  = parens $ unwords $ "and" : map show cs

instance Show (Bind a) where
  show (Bind x t p _) = parens $ unwords [parens $ unwords [F.symbolString x, F.showpp t], show p]

instance F.PPrint (Var a) where
  pprintPrec _ _ v = P.ptext $ show v

instance F.PPrint Pred where
  pprintPrec k t (Reft p)   = P.parens $ F.pprintPrec k t p
  pprintPrec k t (Var x xs) = P.parens $ P.ptext "$" <> P.hsep (P.ptext (F.symbolString x) : (P.parens. F.pprintPrec k t <$> xs))
  pprintPrec k t (PAnd ps)  = P.parens $ P.vcat $ P.ptext "and" : map (F.pprintPrec (k+2) t) ps

instance F.PPrint (Cstr a) where
  pprintPrec k t (Head p _) = P.parens $ F.pprintPrec k t p
  pprintPrec k t (All b c)  = P.parens $ P.vcat [ P.ptext "forall" P.<+> F.pprintPrec (k+2) t b
                                                , F.pprintPrec (k+1) t c
                                                ]
  pprintPrec k t (CAnd cs) = P.parens $ P.vcat  $ P.ptext "and" : map (F.pprintPrec (k+2) t) cs

instance F.PPrint (Bind a) where
  pprintPrec _ _ b = P.ptext $ show b

instance F.ToHornSMT (Query a) where
  toHornSMT q = P.vcat $ L.intersperse " "
    [ P.vcat   (toHornOpt <$> qOpts q)
    , P.vcat   (toHornNum <$> qNums q)
    , P.vcat   (F.toHornSMT <$> qQuals q)
    , P.vcat   (F.toHornSMT <$> qVars q)
    , P.vcat   [toHornCon x t | (x, t) <- M.toList (qCon q)]
    , P.vcat   (F.eqnToHornSMT "define"     <$> qEqns q)
    , P.vcat   (F.eqnToHornSMT "define_fun" <$> qDefs q)
    , P.vcat   (F.toHornSMT <$> qData q)
    , P.vcat   (F.toHornSMT <$> qMats q)
    , P.parens (P.vcat ["constraint", P.nest 1 (F.toHornSMT (qCstr q))])
    ]
    where
      toHornNum x   = F.toHornMany ["numeric", F.toHornSMT x]
      toHornOpt str = F.toHornMany ["fixpoint", P.text ("\"" ++ str ++ "\"")]
      toHornCon x t = F.toHornMany ["constant", F.toHornSMT x, F.toHornSMT t]
