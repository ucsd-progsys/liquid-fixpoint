{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE PatternGuards              #-}

{-# OPTIONS_GHC -Wno-name-shadowing     #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module contains the top-level QUERY data types and elements,
--   including (Horn) implication & well-formedness constraints and sets.
module Language.Fixpoint.Types.Constraints (

   -- * Top-level Queries
    FInfo, SInfo, GInfo (..), FInfoWithOpts(..)
  , convertFormat
  , sinfoToFInfo

   -- * Serializing
  , toFixpoint
  , writeFInfo
  , saveQuery
  , saveBinaryQuery
  , saveSInfo

   -- * Constructing Queries
  , fi

  -- * Constraints
  , WfC (..)
  , SubC, SubcId
  , mkSubC, subcId, sid, senv, updateSEnv, slhs, srhs, stag, subC, wfC
  , SimpC (..)
  , Tag
  , TaggedC, clhs, crhs
  -- , strengthenLhs
  , strengthenHyp
  , strengthenBinds

  -- * Accessing Constraints
  , addIds
  , sinfo
  , shiftVV

  -- * Qualifiers
  , Qualifier
  , QualifierV  (..)
  , QualParam   (..)
  , QualPattern (..)
  , trueQual
  , qualifier
  , mkQual
  , remakeQual
  , mkQ
  , qualBinds

  -- * Results
  , FixSolution
  , FixDelayedSolution
  , Delayed (..)
  , Result (..), ResultSorts
  , unsafe, isUnsafe, isSafe ,safe

  -- * Cut KVars
  , Kuts (..)
  , ksMember

  -- * Higher Order Logic
  , HOInfo (..)
  , allowHO
  , allowHOquals
  , cfgHoInfo

  -- * Axioms
  , AxiomEnv (..)
  , Equation
  , DefinedFuns (..)
  , EquationV (..)
  , mkEquation
  , Rewrite  (..)
  , AutoRewrite (..)
  , dedupAutoRewrites
  , LocalRewritesEnv (..)
  , LocalRewrites (..)
  , lookupRewrite
  , lookupLocalRewrites
  , insertRewrites
  , eqnToHornSMT

  -- * Misc  [should be elsewhere but here due to dependencies]
  , substVars
  , sortVars
  , gSorts

  ) where

import qualified Data.Store as S
import           Data.Generics             (Data)
import           Data.Aeson                hiding (Result)
import qualified Data.Set                  as Set
import           Data.Typeable             (Typeable)
import           Data.Hashable
import           GHC.Generics              (Generic)
import qualified Data.List                 as L -- (sort, nub, delete)
import           Data.Maybe                (catMaybes)
import           Control.DeepSeq
import           Control.Monad             (when, void)
import           Language.Fixpoint.Types.PrettyPrint
import           Language.Fixpoint.Types.SMTPrint
import qualified Language.Fixpoint.Types.Config as C
import           Language.Fixpoint.Types.Triggers
import           Language.Fixpoint.Types.Names
import           Language.Fixpoint.Types.Errors
import           Language.Fixpoint.Types.Spans
import           Language.Fixpoint.Types.Sorts
import           Language.Fixpoint.Types.Refinements
import           Language.Fixpoint.Types.Substitutions
import           Language.Fixpoint.Types.Environments
import qualified Language.Fixpoint.Utils.Files as Files
import qualified Language.Fixpoint.Solver.Stats as Solver

import           Language.Fixpoint.Misc
import           Text.PrettyPrint.HughesPJ.Compat
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import qualified Data.ByteString           as B
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Binary as B

--------------------------------------------------------------------------------
-- | Constraints ---------------------------------------------------------------
--------------------------------------------------------------------------------

{-@ type Tag = { v : [Int] | len v == 1 } @-}

type Tag           = [Int]

data WfC a  =  WfC  { wenv  :: !IBindEnv
                    , wrft  :: (Symbol, Sort, KVar)
                    , winfo :: !a
                    }
              deriving (Eq, Generic, Functor)

type SubcId = Integer

data SubC a = SubC
  { _senv  :: !IBindEnv
  , slhs   :: !SortedReft
  , srhs   :: !SortedReft
  , _sid   :: !(Maybe SubcId)
  , _stag  :: !Tag
  , _sinfo :: !a
  }
  deriving (Eq, Generic, Functor)

data SimpC a = SimpC
  { _cenv  :: !IBindEnv
  , _crhs  :: !Expr
  , _cid   :: !(Maybe Integer)
  , cbind  :: !BindId               -- ^ Id of lhs/rhs binder
  , _ctag  :: !Tag
  , _cinfo :: !a
  }
  deriving (Generic, Functor)

instance Loc a => Loc (SimpC a) where
  srcSpan = srcSpan . _cinfo

strengthenHyp :: SInfo a -> [(Integer, Expr)] -> BindEnv a
strengthenHyp si ies = strengthenBinds si bindExprs
  where
    bindExprs        = safeFromList "strengthenHyp" [ (subcBind si i, e) | (i, e) <- ies ]

subcBind :: SInfo a -> SubcId -> BindId
subcBind si i
  | Just c <- M.lookup i (cm si)
  = cbind c
  | otherwise
  = errorstar $ "Unknown subcId in subcBind: " ++ show i


strengthenBinds :: SInfo a -> M.HashMap BindId Expr -> BindEnv a
strengthenBinds si m = mapBindEnv f (bs si)
  where
    f i (x, sr, l)   = case M.lookup i m of
                         Nothing -> (x, sr, l)
                         Just e  -> (x, strengthenSortedReft sr e, l)

strengthenSortedReft :: SortedReft -> Expr -> SortedReft
strengthenSortedReft (RR s (Reft (v, r))) e = RR s (Reft (v, pAnd [r, e]))


{-
  [(Int, Expr)]  ==> [(BindId, Expr)]

 -}

-- strengthenLhs :: Expr -> SubC a -> SubC a
-- strengthenLhs e subc = subc {slhs = go (slhs subc)}
--  where
--    go (RR s (Reft(v, r))) = RR s (Reft (v, pAnd [r, e]))

class TaggedC c a where
  senv  :: c a -> IBindEnv
  updateSEnv  :: c a -> (IBindEnv -> IBindEnv) -> c a
  sid   :: c a -> Maybe Integer
  stag  :: c a -> Tag
  sinfo :: c a -> a
  clhs  :: BindEnv a -> c a -> [(Symbol, SortedReft)]
  crhs  :: c a -> Expr

instance TaggedC SimpC a where
  senv      = _cenv
  updateSEnv c f = c { _cenv = f (_cenv c) }
  sid       = _cid
  stag      = _ctag
  sinfo     = _cinfo
  crhs      = _crhs
  clhs be c = envCs be (senv c)

instance TaggedC SubC a where
  senv      = _senv
  updateSEnv c f = c { _senv = f (_senv c) }
  sid       = _sid
  stag      = _stag
  sinfo     = _sinfo
  crhs      = reftPred . sr_reft . srhs
  clhs be c = sortedReftBind (slhs c) : envCs be (senv c)

sortedReftBind :: SortedReft -> (Symbol, SortedReft)
sortedReftBind sr = (x, sr)
  where
    Reft (x, _)   = sr_reft sr

subcId :: (TaggedC c a) => c a -> SubcId
subcId = mfromJust "subCId" . sid

---------------------------------------------------------------------------
-- | Solutions and Results
---------------------------------------------------------------------------

-- | Since some solutions are expensive to compute, we wrap them in a
-- "Delayed" type to compute them only if needed.
{- HLINT ignore Delayed "Use newtype instead of data" -}
data Delayed a = Delayed
  { forceDelayed  :: a
  }
  deriving (Generic, Show, Functor)

instance (NFData a) => NFData (Delayed a)


type FixSolution  = M.HashMap KVar Expr
type FixDelayedSolution  = M.HashMap KVar (Delayed Expr)

data Result a = Result
  { resStatus    :: !(FixResult a)
  , resSolution  :: !FixSolution
  , resNonCutsSolution :: !FixDelayedSolution
  , resSorts     :: !ResultSorts
  }
  deriving (Generic, Show, Functor)

type ResultSorts = M.HashMap KVar [(Symbol, Sort)]

data ScopedResult = MkScopedResult
  { scCuts    :: KVarMap ScopedExpr
  , scNonCuts :: KVarMap ScopedExpr
  }
  deriving (Generic, Show)

newtype KVarMap a = MkKVarMap { unKVarMap :: M.HashMap KVar a }
  deriving (Generic, Show)

newtype KVarBind a = MkKVarBind { unKVarBind :: (KVar, a) }
  deriving (Generic, Show)
data ScopedExpr = MkScopedExpr
  { seParams :: [(Symbol, Sort)]
  , seBody :: !Expr
  }
  deriving (Generic, Show)

instance ToHornSMT ScopedExpr where
  toHornSMT (MkScopedExpr xts p) = toHornWithBinders "lambda" xts p


scopedResult :: Result a -> ScopedResult
scopedResult res = MkScopedResult cuts  nonCuts
  where
    cuts = scoped $ resSolution res
    nonCuts = scoped $ M.map forceDelayed $ resNonCutsSolution res
    scoped sol = MkKVarMap $ M.fromList [ (k, MkScopedExpr (scope k) e) | (k, e) <- M.toList sol]
    scope k = M.lookupDefault [] k $ resSorts res

instance ToJSON a => ToJSON (Result a) where
  toJSON r@(Result {..}) = object
    [ "status"            .= resStatus
    , "solution"          .= scCuts scopedSolution
    , "nonCutsSolution"   .= scNonCuts scopedSolution
    ]
    where
      scopedSolution = scopedResult r

instance ToJSON a => ToJSON (KVarBind a) where
  toJSON (MkKVarBind (k, v)) = object
    [ "kvar" .= k
    , "val"  .= v
    ]

instance ToJSON a => ToJSON (KVarMap a) where
  toJSON = toJSON . map MkKVarBind . M.toList . unKVarMap

instance ToJSON ScopedExpr where
  toJSON = toJSON . render . toHornSMT

instance Semigroup (Result a) where
  r1 <> r2  = Result stat soln nonCutsSoln sorts
    where
      stat  = resStatus r1    <> resStatus r2
      soln  = resSolution r1  <> resSolution r2
      nonCutsSoln = resNonCutsSolution r1 <> resNonCutsSolution r2
      sorts = M.unionWith L.union (resSorts r1) (resSorts r2)

instance Monoid (Result a) where
  mempty        = Result mempty mempty mempty mempty
  mappend       = (<>)

unsafe, safe :: Result a
unsafe = mempty {resStatus = Unsafe mempty []}
safe   = mempty {resStatus = Safe mempty}

isSafe :: Result a -> Bool
isSafe r = case resStatus r of
  Safe{} -> True
  _ -> False

isUnsafe :: Result a -> Bool
isUnsafe r | Unsafe _ _ <- resStatus r
  = True
isUnsafe _ = False

instance (Ord a, Fixpoint a) => Fixpoint (FixResult (SubC a)) where
  toFix (Safe stats)     = text "Safe (" <+> text (show $ Solver.checked stats) <+> " constraints checked)"
  -- toFix (UnknownError d) = text $ "Unknown Error: " ++ d
  toFix (Crash xs msg)   = vcat $ [ text "Crash!" ] ++  pprSinfos "CRASH: " (fst <$> xs) ++ [parens (text msg)]
  toFix (Unsafe _ xs)    = vcat $ text "Unsafe:" : pprSinfos "WARNING: " xs

pprSinfos :: (Ord a, Fixpoint a) => String -> [SubC a] -> [Doc]
pprSinfos msg = map ((text msg <->) . toFix) . L.sort . fmap sinfo

instance Fixpoint a => Show (WfC a) where
  show = showFix

instance Fixpoint a => Show (SubC a) where
  show = showFix

instance Fixpoint a => Show (SimpC a) where
  show = showFix

instance Fixpoint a => PPrint (SubC a) where
  pprintTidy _ = toFix

instance Fixpoint a => PPrint (SimpC a) where
  pprintTidy _ = toFix

instance Fixpoint a => PPrint (WfC a) where
  pprintTidy _ = toFix

instance Fixpoint a => Fixpoint (SubC a) where
  toFix c     = hang (text "\n\nconstraint:") 2 bd
     where bd =   toFix (senv c)
              $+$ text "lhs" <+> toFix (slhs c)
              $+$ text "rhs" <+> toFix (srhs c)
              $+$ (pprId (sid c) <+> text "tag" <+> toFix (stag c))
              $+$ toFixMeta (text "constraint" <+> pprId (sid c)) (toFix (sinfo c))

instance Fixpoint a => Fixpoint (SimpC a) where
  toFix c     = hang (text "\n\nsimpleConstraint:") 2 bd
     where bd =   toFix (senv c)
              $+$ text "rhs" <+> toFix (crhs c)
              $+$ (pprId (sid c) <+> text "tag" <+> toFix (stag c))
              $+$ toFixMeta (text "simpleConstraint" <+> pprId (sid c)) (toFix (sinfo c))

instance Fixpoint a => Fixpoint (WfC a) where
  toFix w     = hang (text "\n\nwf:") 2 bd
    where bd  =   toFix (wenv w)
              -- NOTE: this next line is printed this way for compatability with the OCAML solver
              $+$ text "reft" <+> toFix (RR t (Reft (v, PKVar k M.empty mempty)))
              $+$ toFixMeta (text "wf") (toFix (winfo w))
          (v, t, k) = wrft w

toFixMeta :: Doc -> Doc -> Doc
toFixMeta k v = text "// META" <+> k <+> text ":" <+> v

pprId :: Show a => Maybe a -> Doc
pprId (Just i)  = "id" <+> tshow i
pprId _         = ""

----------------------------------------------------------------
instance S.Store QualPattern
instance S.Store QualParam
instance S.Store Qualifier
instance S.Store Kuts
instance S.Store HOInfo
instance (S.Store a) => S.Store (SubC a)
instance (S.Store a) => S.Store (WfC a)
instance (S.Store a) => S.Store (SimpC a)
instance (S.Store (c a), S.Store a) => S.Store (GInfo c a)

instance NFData QualPattern
instance NFData QualParam
instance NFData v => NFData (QualifierV v)
instance NFData Kuts
instance NFData HOInfo

instance (NFData a) => NFData (SubC a)
instance (NFData a) => NFData (WfC a)
instance (NFData a) => NFData (SimpC a)
instance (NFData (c a), NFData a) => NFData (GInfo c a)
instance (NFData a) => NFData (Result a)

instance Hashable v => Hashable (QualifierV v)
instance Hashable QualPattern
instance Hashable QualParam
instance Hashable v => Hashable (EquationV v)

instance B.Binary QualPattern
instance B.Binary QualParam
instance B.Binary v => B.Binary (QualifierV v)
instance B.Binary v => B.Binary (EquationV v)

---------------------------------------------------------------------------
-- | "Smart Constructors" for Constraints ---------------------------------
---------------------------------------------------------------------------

wfC :: (Fixpoint a) => IBindEnv -> SortedReft -> a -> [WfC a]
wfC be sr x = if all isEmptyKVarSubst sus -- ++ gsus)
                 -- NV TO RJ This tests fails with [LT:=GHC.Types.LT][EQ:=GHC.Types.EQ][GT:=GHC.Types.GT]]
                 -- NV TO RJ looks like a resolution issue
                then [WfC be (v, sr_sort sr, k) x      | k         <- ks ]
                else errorstar msg
  where
    msg             = "wfKvar: malformed wfC " ++ show sr ++ "\n" ++ show sus
    Reft (v, ras)   = sr_reft sr
    (ks, sus)       = unzip $ go ras

    go (PKVar k _ su) = [(k, su)]
    go (PAnd es)    = [(k, su) | PKVar k _ su <- es]
    go _            = []


mkSubC :: IBindEnv -> SortedReft -> SortedReft -> Maybe Integer -> Tag -> a -> SubC a
mkSubC = SubC

subC :: IBindEnv -> SortedReft -> SortedReft -> Maybe Integer -> Tag -> a -> [SubC a]
subC γ sr1 sr2 i y z = [SubC γ sr1' (sr2' r2') i y z | r2' <- reftConjuncts r2]
   where
     RR t1 r1          = sr1
     RR t2 r2          = sr2
     sr1'              = RR t1 $ shiftVV r1  vv'
     sr2' r2'          = RR t2 $ shiftVV r2' vv'
     vv'               = mkVV i

mkVV :: Maybe Integer -> Symbol
mkVV (Just i)  = vv $ Just i
mkVV Nothing   = vvCon

shiftVV :: Reft -> Symbol -> Reft
shiftVV r@(Reft (v, ras)) v'
   | v == v'   = r
   | otherwise = Reft (v', subst1 ras (v, EVar v'))

addIds :: [SubC a] -> [(Integer, SubC a)]
addIds = zipWith (\i c -> (i, shiftId i $ c {_sid = Just i})) [1..]
  where
    -- Adding shiftId to have distinct VV for SMT conversion
    shiftId i c = c { slhs = shiftSR i (slhs c) }
                    { srhs = shiftSR i (srhs c) }
    shiftSR i sr = sr { sr_reft = shiftR i $ sr_reft sr }
    shiftR i r@(Reft (v, _)) = shiftVV r (intSymbol v i)

--------------------------------------------------------------------------------
-- | Qualifiers ----------------------------------------------------------------
--------------------------------------------------------------------------------
type Qualifier = QualifierV Symbol
data QualifierV v = Q
  { qName   :: !Symbol     -- ^ Name
  , qParams :: [QualParam] -- ^ Parameters
  , qBody   :: !(ExprV v)  -- ^ Predicate
  , qPos    :: !SourcePos  -- ^ Source Location
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Functor, Foldable, Traversable)

data QualParam = QP
  { qpSym  :: !Symbol
  , qpPat  :: !QualPattern
  , qpSort :: !Sort
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToHornSMT QualParam where
  toHornSMT qp = toHornSMT (qpSym qp, qpSort qp)


data QualPattern
  = PatNone                 -- ^ match everything
  | PatPrefix !Symbol !Int  -- ^ str . $i  i.e. match prefix 'str' with suffix bound to $i
  | PatSuffix !Int !Symbol  -- ^ $i . str  i.e. match suffix 'str' with prefix bound to $i
  | PatExact  !Symbol       -- ^ str       i.e. exactly match 'str'
  | PatLit                  -- ^ match literals of the given sort
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance ToJSON   Qualifier   where
instance FromJSON Qualifier   where
instance ToJSON   QualParam   where
instance FromJSON QualParam   where
instance ToJSON   QualPattern where
instance FromJSON QualPattern where
instance ToJSON   Equation    where
instance FromJSON Equation    where
instance ToJSON   Rewrite     where
instance FromJSON Rewrite     where

instance ToHornSMT Qualifier where
  toHornSMT (Q n qps p _) =  toHornWithBinders name xts p
    where
      name = "qualif" <+> pprint n
      xts =  [(qpSym qp, qpSort qp) | qp <- qps]

trueQual :: Qualifier
trueQual = Q (symbol ("QTrue" :: String)) [] PTrue (dummyPos "trueQual")

instance Loc Qualifier where
  srcSpan q = SS l l
    where
      l     = qPos q

instance Subable Qualifier where
  syms   = qualFreeSymbols
  substr ns su q =
    let xs  = qpSym <$> qParams q
        (ns', xs') = freshInNSL xs ns
        su' = L.foldl' (\acc (x, x') -> extendSubst acc x (EVar x')) su (zip xs xs')
        ps' = zipWith (\qp x' -> qp { qpSym = x' }) (qParams q) xs'
    in  q { qParams = ps', qBody = substr ns' su' (qBody q) }

qualFreeSymbols :: Qualifier -> S.HashSet Symbol
qualFreeSymbols q = S.filter (not . isPrim) xs
  where
    xs            = syms (qBody q) `S.difference` S.fromList (qpSym <$> qParams q)

instance Fixpoint QualParam where
  toFix (QP x _ t) = toFix (x, t)

instance PPrint QualParam where
  pprintTidy k (QP x pat t) = pprintTidy k x <+> pprintTidy k pat <+> colon <+> pprintTidy k t

instance PPrint QualPattern where
  pprintTidy _ PatNone         = ""
  pprintTidy k (PatPrefix s i) = "as" <+> pprintTidy k s <+> ("$" <-> pprint i)
  pprintTidy k (PatSuffix s i) = "as" <+> ("$" <-> pprint i) <+> pprintTidy k s
  pprintTidy k (PatExact  s  ) = "~"  <+> pprintTidy k s
  pprintTidy _ PatLit          = "as lit"

instance Fixpoint Qualifier where
  toFix = pprQual

instance (Ord v, Fixpoint v, PPrint v) => PPrint (QualifierV v) where
  pprintTidy k q =
    "qualif" <+> pprintTidy k (qName q) <+>
     parens (hsep $ punctuate comma (pprintTidy k <$> qParams q)) <+>
     braces (pprintTidy k (qBody q)) <+> "//defined at" <+> pprintTidy k (qPos q)


pprQual :: Qualifier -> Doc
pprQual (Q n xts p l) = text "qualif" <+> text (symbolString n) <-> parens args <-> braces (toFix p) <+> text "//" <+> toFix l
  where
    args              = intersperse comma (toFix <$> xts)

qualifier :: SEnv Sort -> SourcePos -> SEnv Sort -> Symbol -> Sort -> Expr -> Qualifier
qualifier lEnv l γ v so p   = mkQ "Auto" ((v, so) : xts) p l
  where
    xs  = S.delete v $ syms p
    xts = catMaybes $ zipWith (envSort l lEnv γ) (S.toList xs) [0..]

mkQ :: Symbol -> [(Symbol, Sort)] -> Expr -> SourcePos -> Qualifier
mkQ n = Q n . qualParams

qualParams :: [(Symbol, Sort)] -> [QualParam]
qualParams xts = [ QP x PatNone t | (x, t) <- xts]

qualBinds   :: Qualifier -> [(Symbol, Sort)]
qualBinds q = [ (qpSym qp, qpSort qp) | qp <- qParams q ]

envSort :: SourcePos -> SEnv Sort -> SEnv Sort -> Symbol -> Integer -> Maybe (Symbol, Sort)
envSort l lEnv tEnv x i
  | Just t <- lookupSEnv x tEnv = Just (x, t)
  | Just _ <- lookupSEnv x lEnv = Nothing
  | otherwise                   = Just (x, ai)
  where
    ai  = {- trace msg $ -} fObj $ Loc l l $ tempSymbol "LHTV" i
    -- msg = "unknown symbol in qualifier: " ++ show x

remakeQual :: Qualifier -> Qualifier
remakeQual q = mkQual (qName q) (qParams q) (qBody q) (qPos q)

-- | constructing qualifiers
mkQual :: Symbol -> [QualParam] -> ExprV v -> SourcePos -> QualifierV v
mkQual n qps p = Q n qps' p
  where
    qps'       = zipWith (\qp t' -> qp { qpSort = t'}) qps ts'
    ts'        = gSorts (qpSort <$> qps)

gSorts :: [Sort] -> [Sort]
gSorts ts = substVars su <$> ts
  where
    su    = (`zip` [0..]) . sortNub . concatMap sortVars $ ts

substVars :: [(Symbol, Int)] -> Sort -> Sort
substVars su = mapSort' tx
  where
    tx (FObj x)
      | Just i <- lookup x su = FVar i
    tx t                      = t

sortVars :: Sort -> [Symbol]
sortVars = foldSort' go []
  where
    go b (FObj x) = x : b
    go b _        = b

-- COPIED from Visitor due to cyclic deps
mapSort' :: (Sort -> Sort) -> Sort -> Sort
mapSort' f = step
  where
    step             = go . f
    go (FFunc t1 t2) = FFunc (step t1) (step t2)
    go (FApp t1 t2)  = FApp (step t1) (step t2)
    go (FAbs i t)    = FAbs i (step t)
    go t             = t

-- COPIED from Visitor due to cyclic deps
foldSort' :: (a -> Sort -> a) -> a -> Sort -> a
foldSort' f = step
  where
    step b t           = go (f b t) t
    go b (FFunc t1 t2) = L.foldl' step b [t1, t2]
    go b (FApp t1 t2)  = L.foldl' step b [t1, t2]
    go b (FAbs _ t)    = go b t
    go b _             = b


--------------------------------------------------------------------------------
-- | Constraint Cut Sets -------------------------------------------------------
--------------------------------------------------------------------------------

newtype Kuts = KS { ksVars :: S.HashSet KVar }
               deriving (Eq, Show, Generic)

instance Fixpoint Kuts where
  toFix (KS s) = vcat $ ("cut " <->) . toFix <$> L.sort (S.toList s)

ksMember :: KVar -> Kuts -> Bool
ksMember k (KS s) = S.member k s

instance Semigroup Kuts where
  k1 <> k2 = KS $ S.union (ksVars k1) (ksVars k2)

instance Monoid Kuts where
  mempty  = KS S.empty
  mappend = (<>)

------------------------------------------------------------------------
-- | Constructing Queries
------------------------------------------------------------------------
fi :: [SubC a]
   -> [WfC a]
   -> BindEnv a
   -> SEnv Sort
   -> SEnv Sort
   -> Kuts
   -> [Qualifier]
   -> M.HashMap BindId a
   -> Bool
   -> Bool
   -> [Triggered Expr]
   -> AxiomEnv
   -> [DataDecl]
   -> GInfo SubC a
fi cs ws binds ls ds ks qs bi aHO aHOq es axe adts
  = FI { cm       = M.fromList $ addIds cs
       , ws       = M.fromListWith err [(k, w) | w <- ws, let (_, _, k) = wrft w]
       , bs       = binds
       , gLits    = ls
       , dLits    = ds
       , kuts     = ks
       , quals    = qs
       , bindInfo = bi
       , hoInfo   = HOI aHO aHOq
       , asserts  = es
       , ae       = axe
       , ddecls   = adts
       , lrws     = mempty
       , defns    = mempty
       }
  where
    --TODO handle duplicates gracefully instead (merge envs by intersect?)
    err = errorstar "multiple WfCs with same kvar"

------------------------------------------------------------------------
-- | Top-level Queries
------------------------------------------------------------------------

data FInfoWithOpts a = FIO
  { fioFI   :: FInfo a
  , fioOpts :: [String]
  }

type FInfo a   = GInfo SubC a
type SInfo a   = GInfo SimpC a

data HOInfo = HOI
  { hoBinds :: Bool          -- ^ Allow higher order binds in the environemnt
  , hoQuals :: Bool          -- ^ Allow higher order quals
  }
  deriving (Eq, Show, Generic)

cfgHoInfo :: C.Config -> HOInfo
cfgHoInfo cfg = HOI (C.allowHO cfg) (C.allowHOqs cfg)

allowHO, allowHOquals :: GInfo c a -> Bool
allowHO      = hoBinds . hoInfo
allowHOquals = hoQuals . hoInfo

data GInfo c a = FI
  { cm       :: !(M.HashMap SubcId (c a))  -- ^ cst id |-> Horn Constraint
  , ws       :: !(M.HashMap KVar (WfC a))  -- ^ Kvar  |-> WfC defining its scope/args
  , bs       :: !(BindEnv a)               -- ^ BindId  |-> (Symbol, SortedReft)
  , gLits    :: !(SEnv Sort)               -- ^ Global Constant symbols
  , dLits    :: !(SEnv Sort)               -- ^ Distinct Constant symbols
  , kuts     :: !Kuts                      -- ^ Set of KVars *not* to eliminate
  , quals    :: ![Qualifier]               -- ^ Abstract domain
  , bindInfo :: !(M.HashMap BindId a)      -- ^ Metadata about binders
  , ddecls   :: ![DataDecl]                -- ^ User-defined data declarations
  , hoInfo   :: !HOInfo                    -- ^ Higher Order info
  , asserts  :: ![Triggered Expr]          -- ^ TODO: what is this?
  , ae       :: AxiomEnv                   -- ^ Information about reflected function defs
  , lrws     :: LocalRewritesEnv           -- ^ Local rewrites
  , defns    :: DefinedFuns                -- ^ `define_fun` definitions to be passed to SMT
  }
  deriving (Eq, Show, Functor, Generic)

instance Semigroup HOInfo where
  i1 <> i2 = HOI { hoBinds = hoBinds i1 || hoBinds i2
                 , hoQuals = hoQuals i1 || hoQuals i2
                 }

instance Monoid HOInfo where
  mempty        = HOI False False

instance Semigroup (GInfo c a) where
  i1 <> i2 = FI { cm       = cm i1       <> cm i2
                , ws       = ws i1       <> ws i2
                , bs       = bs i1       <> bs i2
                , gLits    = gLits i1    <> gLits i2
                , dLits    = dLits i1    <> dLits i2
                , kuts     = kuts i1     <> kuts i2
                , quals    = quals i1    <> quals i2
                , bindInfo = bindInfo i1 <> bindInfo i2
                , ddecls   = ddecls i1   <> ddecls i2
                , hoInfo   = hoInfo i1   <> hoInfo i2
                , asserts  = asserts i1  <> asserts i2
                , ae       = ae i1       <> ae i2
                , lrws     = lrws i1     <> lrws i2
                , defns    = defns i1    <> defns i2
                }


instance Monoid (GInfo c a) where
  mempty        = FI { cm       = M.empty
                     , ws       = mempty
                     , bs       = mempty
                     , gLits    = mempty
                     , dLits    = mempty
                     , kuts     = mempty
                     , quals    = mempty
                     , bindInfo = mempty
                     , ddecls   = mempty
                     , hoInfo   = mempty
                     , asserts  = mempty
                     , ae       = mempty
                     , lrws     = mempty
                     , defns    = mempty
                     }

instance PTable (SInfo a) where
  ptable z = DocTable [ (text "# Sub Constraints", pprint $ length $ cm z)
                      , (text "# WF  Constraints", pprint $ length $ ws z)
                      ]

--------------------------------------------------------------------------
-- | Rendering Queries
--------------------------------------------------------------------------
toFixpoint :: (Fixpoint a, Fixpoint (c a)) => C.Config -> GInfo c a -> Doc
--------------------------------------------------------------------------
toFixpoint cfg x' =    cfgDoc   cfg
                  $++$ declsDoc x'
                  $++$ aeDoc    x'
                  $++$ lrwsDoc  x'
                  $++$ qualsDoc x'
                  $++$ kutsDoc  x'
                --   $++$ packsDoc x'
                  $++$ gConDoc   x'
                  $++$ dConDoc   x'
                  $++$ bindsDoc
                  $++$ csDoc    x'
                  $++$ wsDoc    x'
                  $++$ binfoDoc x'
                  $++$ text "\n"
  where
    cfgDoc cfg    = text ("// " ++ show cfg)
    gConDoc       = sEnvDoc "constant"             . gLits
    dConDoc       = sEnvDoc "distinct"             . dLits
    csDoc         = vcat     . map (toFix . snd) . hashMapToAscList . cm
    wsDoc         = vcat     . map toFix . L.sortOn (thd3 . wrft) . M.elems . ws
    kutsDoc       = toFix    . kuts
    -- packsDoc      = toFix    . packs
    declsDoc      = vcat     . map ((text "data" <+>) . toFix) . L.sort . ddecls
    bindsDoc      = toFix (bs x')
    qualsDoc      = vcat     . map toFix . L.sort . quals
    aeDoc         = toFix    . ae
    lrwsDoc       = toFix    . lrws
    metaDoc (i,d) = toFixMeta (text "bind" <+> toFix i) (toFix d)
    mdata         = C.metadata cfg
    binfoDoc
      | mdata     = vcat     . map metaDoc . M.toList . bindInfo
      | otherwise = \_ -> text "\n"

infixl 9 $++$
($++$) :: Doc -> Doc -> Doc
x $++$ y = x $+$ text "\n" $+$ y

sEnvDoc :: Doc -> SEnv Sort -> Doc
sEnvDoc d       = vcat . map kvD . L.sortOn fst . toListSEnv
  where
    kvD (c, so) = d <+> toFix c <+> ":" <+> parens (toFix so)

writeFInfo :: (Fixpoint a, Fixpoint (c a)) => C.Config -> GInfo c a -> FilePath -> IO ()
writeFInfo cfg fq f = writeFile f (render $ toFixpoint cfg fq)

--------------------------------------------------------------------------------
-- | Query Conversions: FInfo to SInfo
--------------------------------------------------------------------------------
convertFormat :: (Fixpoint a) => FInfo a -> SInfo a
--------------------------------------------------------------------------------
convertFormat fi = fi' { cm = subcToSimpc bindm <$> cm fi' }
  where
    (bindm, fi') = M.foldlWithKey' outVV (M.empty, fi) $ cm fi

subcToSimpc :: BindM -> SubC a -> SimpC a
subcToSimpc m s = SimpC
  { _cenv       = senv s
  , _crhs       = reftPred $ sr_reft $ srhs s
  , _cid        = sid s
  , cbind      = safeLookup "subcToSimpc" (subcId s) m
  , _ctag       = stag s
  , _cinfo      = sinfo s
  }

outVV :: (BindM, FInfo a) -> Integer -> SubC a -> (BindM, FInfo a)
outVV (m, fi) i c = (m', fi')
  where
    fi'           = fi { bs = be', cm = cm' }
    m'            = M.insert i bId m
    (bId, be')    = insertBindEnv x sr (sinfo c) (bs fi)
    cm'           = M.insert i c' $ cm fi
    c'            = c { _senv = insertsIBindEnv [bId] $ senv c }
    sr            = slhs c
    x             = reftBind $ sr_reft sr

type BindM = M.HashMap Integer BindId

sinfoToFInfo :: SInfo a -> FInfo a
sinfoToFInfo fi = fi
  { bs = envWithoutLhss
  , cm = simpcToSubc (bs fi) <$> cm fi
  }
  where
    envWithoutLhss =
      M.foldl' (\m c -> deleteBindEnv (cbind c) m) (bs fi) (cm fi)

-- Assumes the sort and the bind of the lhs is the same as the sort
-- and the bind of the rhs
simpcToSubc :: BindEnv a -> SimpC a -> SubC a
simpcToSubc env s = SubC
  { _senv  = deleteIBindEnv (cbind s) (senv s)
  , slhs   = sr
  , srhs   = RR (sr_sort sr) (Reft (b, _crhs s))
  , _sid   = sid s
  , _stag  = stag s
  , _sinfo = sinfo s
  }
  where
    (b, sr, _) = lookupBindEnv (cbind s) env

--------------------------------------------------------------------------------
saveQuery :: (Fixpoint a) => C.Config -> FInfo a -> IO ()
--------------------------------------------------------------------------------
saveQuery cfg fi = when (C.save cfg) $ do
  let fi'  = void fi
  saveBinaryQuery cfg fi'
  saveTextQuery cfg   fi

saveBinaryQuery :: C.Config -> FInfo () -> IO ()
saveBinaryQuery cfg fi = do
  let bfq  = C.queryFile Files.BinFq cfg
  putStrLn $ "Saving Binary Query: " ++ bfq ++ "\n"
  ensurePath bfq
  B.writeFile bfq (S.encode fi)

saveTextQuery :: Fixpoint a => C.Config -> FInfo a -> IO ()
saveTextQuery cfg fi = do
  let fq   = C.queryFile Files.Fq cfg
  putStrLn $ "Saving Text Query: "   ++ fq ++ "\n"
  ensurePath fq
  T.writeFile fq $ T.pack $ render (toFixpoint cfg fi)

-- | Used for debugging to inspect intermediate 'SInfo' files.
--
-- Takes a suffix to put in the name of the written file, whose name
-- is still derived from the input file name in `cfg`.
--
-- Usage example:
--
-- > when (save cfg) $
-- >   saveSInfo cfg ".sinfo" si
--
-- This will write a file like `.liquid/Test.hs.sinfo.fq` when the
-- `--save` flag is used.
--
saveSInfo :: Fixpoint a => C.Config -> String -> SInfo a -> IO ()
saveSInfo cfg sfx si = do
  let fq = Files.tempFileName (C.srcFile cfg ++ sfx ++ ".fq")
  putStrLn $ "Saving Text Query: "   ++ fq ++ "\n"
  ensurePath fq
  T.writeFile fq $ T.pack $ render (toFixpoint cfg si)

---------------------------------------------------------------------------
-- | Axiom Instantiation Information --------------------------------------
---------------------------------------------------------------------------
data AxiomEnv = AEnv
  { aenvEqs      :: ![Equation]
  , aenvSimpl    :: ![Rewrite]
  , aenvExpand   :: M.HashMap SubcId Bool
  , aenvAutoRW   :: M.HashMap SubcId [AutoRewrite]
  } deriving (Eq, Show, Generic)

newtype LocalRewrites = LocalRewrites (M.HashMap Symbol Expr)
  deriving (Eq, Show, Generic, Semigroup, Monoid, NFData, S.Store)

newtype LocalRewritesEnv = LocalRewritesMap (M.HashMap BindId LocalRewrites)
  deriving (Eq, Show, Generic, Semigroup, Monoid, NFData, S.Store)

lookupRewrite :: Symbol -> LocalRewrites -> Maybe Expr
lookupRewrite x (LocalRewrites m) = M.lookup x m

lookupLocalRewrites :: BindId -> LocalRewritesEnv -> Maybe LocalRewrites
lookupLocalRewrites i (LocalRewritesMap m) = M.lookup i m

insertRewrites :: BindId -> LocalRewrites -> LocalRewritesEnv -> LocalRewritesEnv
insertRewrites i rws (LocalRewritesMap m) = LocalRewritesMap $ M.insertWith (<>) i rws m


instance S.Store AutoRewrite
instance S.Store AxiomEnv
instance S.Store Rewrite
instance S.Store Equation
instance S.Store DefinedFuns
instance NFData AutoRewrite
instance NFData AxiomEnv
instance NFData Rewrite
instance NFData Equation
instance NFData DefinedFuns

dedupAutoRewrites :: M.HashMap SubcId [AutoRewrite] -> [AutoRewrite]
dedupAutoRewrites = Set.toList . Set.unions . map Set.fromList . M.elems

instance Semigroup AxiomEnv where
  a1 <> a2        = AEnv aenvEqs' aenvSimpl' aenvExpand' aenvAutoRW'
    where
      aenvEqs'    = aenvEqs a1    <> aenvEqs a2
      aenvSimpl'  = aenvSimpl a1  <> aenvSimpl a2
      aenvExpand' = aenvExpand a1 <> aenvExpand a2
      aenvAutoRW' = aenvAutoRW a1 <> aenvAutoRW a2

instance Monoid AxiomEnv where
  mempty          = AEnv [] [] (M.fromList []) (M.fromList [])
  mappend         = (<>)

instance PPrint AxiomEnv where
  pprintTidy _ = text . show


newtype DefinedFuns = MkDefinedFuns [Equation]
  deriving (Data, Eq, Ord, Show, Generic)

instance Semigroup DefinedFuns where
  MkDefinedFuns eq1 <> MkDefinedFuns eq2 = MkDefinedFuns (eq1 <> eq2)

instance Monoid DefinedFuns where
  mempty = MkDefinedFuns []

instance PPrint DefinedFuns where
  pprintTidy k (MkDefinedFuns eqs) = pprintTidy k eqs

type Equation = EquationV Symbol
data EquationV v = Equ
  { eqName :: !Symbol           -- ^ name of reflected function
  , eqArgs :: [(Symbol, Sort)]  -- ^ names of parameters
  , eqBody :: !(ExprV v)        -- ^ definition of body
  , eqSort :: !Sort             -- ^ sort of body
  , eqRec  :: !Bool             -- ^ is this a recursive definition
  }
  deriving (Data, Eq, Ord, Show, Generic, Functor)

eqnToHornSMT :: Doc -> Equation -> Doc
eqnToHornSMT keyword (Equ f xs e s _) = parens (keyword <+> pprint f <+> toHornSMT xs <+> toHornSMT s <+> toHornSMT e)


mkEquation :: Symbol -> [(Symbol, Sort)] -> Expr -> Sort -> Equation
mkEquation f xts e out = Equ f xts e out (f `elem` syms e)

instance Subable Equation where
  syms   a = syms (eqBody a) -- ++ F.syms (axiomEq a)
  substr ns su eq =
    let (xs, sorts) = unzip (eqArgs eq)
        (ns', xs')  = freshInNSL xs ns
        su'  = L.foldl' (\acc (x, x') -> extendSubst acc x (EVar x')) su (zip xs xs')
    in  eq { eqArgs = zip xs' sorts, eqBody = substr ns' su' (eqBody eq) }

instance PPrint Equation where
  pprintTidy _ = toFix

ppArgs :: (PPrint a) => [a] -> Doc
ppArgs = parens . intersperse ", " . fmap pprint


data AutoRewrite = AutoRewrite
  { arArgs :: [SortedReft]
  , arLHS  :: Expr
  , arRHS  :: Expr
} deriving (Eq, Ord, Show, Generic)

instance Hashable AutoRewrite

autoRWToFix :: M.HashMap SubcId [AutoRewrite] -> Doc
autoRWToFix autoRW =
  vcat $
  map fixRW rewrites ++
  rwsMapping
  where
    rewrites = dedupAutoRewrites autoRW

    fixRW rw@(AutoRewrite args lhs rhs) =
        text ("autorewrite " ++ show (hash rw))
        <+> hsep (map toFix args)
        <+> text "="
        <+> text "{"
        <+> toFix lhs
        <+> text "="
        <+> toFix rhs
        <+> text "}"

    rwsMapping = do
      (cid, rws) <- M.toList autoRW
      rw         <-  rws
      return $ "rewrite" <+> brackets (text $ show cid ++ " : " ++ show (hash rw))



-- eg  SMeasure (f D [x1..xn] e)
-- for f (D x1 .. xn) = e
data Rewrite  = SMeasure
  { smName  :: Symbol         -- eg. f
  , smDC    :: Symbol         -- eg. D
  , smArgs  :: [Symbol]       -- eg. xs
  , smBody  :: Expr           -- eg. e[xs]
  }
  deriving (Data, Eq, Ord, Show, Generic)

instance ToHornSMT Rewrite where
  toHornSMT (SMeasure f d xs e) =  parens ("match" <+> toHornSMT f <+> toHornSMT (d:xs) <+> toHornSMT e)



instance Fixpoint AxiomEnv where
  toFix axe = vcat ((toFix <$> L.sort (aenvEqs axe)) ++ (toFix <$> L.sort (aenvSimpl axe)))
              $+$ renderExpand (pairdoc <$> L.sort (M.toList $ aenvExpand axe))
              $+$ autoRWToFix (aenvAutoRW axe)
    where
      pairdoc (x,y) = text $ show x ++ " : " ++ show y
      renderExpand [] = empty
      renderExpand xs = text "expand" <+> toFix xs

instance Fixpoint Equation where
  toFix (Equ f xs e s _) = "define" <+> toFix f <+> ppArgs xs <+> ":" <+> toFix s <+> text "=" <+> braces (parens (toFix e))

instance Fixpoint LocalRewritesEnv where
  toFix (LocalRewritesMap rws) = vcat $ uncurry toFixLocal <$> M.toList rws
    where
      toFixLocal bid (LocalRewrites rws) = text "defineLocal" <+> toFix bid
        <+> brackets (vcat $ punctuate ";" $ uncurry toFixRewrite <$> M.toList rws)
      toFixRewrite sym eq = toFix sym <+> text ":=" <+> toFix eq

instance Fixpoint Rewrite where
  toFix (SMeasure f d xs e)
    = text "match"
   <+> toFix f
   <+> toFix d <+> hsep (toFix <$> xs)
   <+> braces (toFix e)

instance PPrint Rewrite where
  pprintTidy _ = toFix
