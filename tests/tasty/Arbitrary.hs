{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Arbitrary
  ( subexprs
  , Env(..)
  , NoAnfEnv(..)
  , AnfSymbol(..)
  , FlatAnfEnv(..)
  , ChainedAnfEnv(..)
  ) where

import Control.Monad                    (forM_, liftM)
import Data.Monoid (Sum(..), (<>))
import qualified Data.Text                 as Text
import qualified Data.HashMap.Strict       as M
import Test.Tasty.QuickCheck
import GHC.Generics

import Language.Fixpoint.Types.Refinements as R
import Language.Fixpoint.Parse             (isNotReserved, rr)
import Language.Fixpoint.Types             as T hiding (Result)
import Language.Fixpoint.Types.Spans       as Spans
import Language.Fixpoint.Types.Refinements (Expr(PKVar, EVar))
import Language.Fixpoint.Types.Names       (isFixKey)
import Data.Traversable                    (for)

{-

-- An example of how the Arbitrary Expr instance can be used.
-- Note that `expr == rr (showpp expr)` is *not* something we expect to hold.
-- See https://github.com/ucsd-progsys/liquid-fixpoint/issues/46

quickCheckTests :: TestTree
quickCheckTests
  = testGroup "Properties"
      [ testProperty "prop_pprint_parse_inv_expr" prop_pprint_parse_inv_expr
      ]

prop_pprint_parse_inv_expr :: Expr -> Bool
prop_pprint_parse_inv_expr expr = expr == rr (showpp expr)

-}

instance Arbitrary Expr where
  arbitrary = sized arbitraryExpr
  shrink x = filter valid $ genericShrink x
    where
      valid e@(T.PAnd es)     = length es >= 2 && all valid (subexprs e)
      valid e@(T.POr es)      = length es >= 2 && all valid (subexprs e)
      valid e@(T.PAll es _)   = length es >= 2 && all valid (subexprs e)
      valid e@(T.PExist es _) = length es >= 2 && all valid (subexprs e)
      valid e                 = all valid (subexprs e)

subexprs :: Expr -> [Expr]
subexprs (ESym _)        = []
subexprs (ECon _)        = []
subexprs (EVar _)        = []
subexprs (EApp e0 e1)    = [e0, e1]
subexprs (ENeg e)        = [e]
subexprs (EBin _ e0 e1)  = [e0, e1]
subexprs (EIte e0 e1 e2) = [e0, e1, e2]
subexprs (ECst e _)      = [e]
subexprs (ELam _ e)      = [e]
subexprs (ETApp e _)     = [e]
subexprs (ETAbs e _)     = [e]
subexprs (T.PAnd es)     = es
subexprs (T.POr es)      = es
subexprs (T.PNot e)      = [e]
subexprs (PImp e0 e1)    = [e0, e1]
subexprs (PIff e0 e1)    = [e0, e1]
subexprs (PAtom _ e0 e1) = [e0, e1]
subexprs (PKVar _ _)     = []
subexprs (PAll _ e)      = [e]
subexprs (PExist _ e)    = [e]
subexprs (PGrad _ _ _ e) = [e]
subexprs (ECoerc _ _ e)  = [e]

-- TODO: Adjust frequencies
-- | To ensure this reliably terminates we require that `zeroExprGen` generates
-- atomic expressions.
arbitraryFiniteExpr
  :: Gen Expr -- ^ called when the Int is zero.
  -> Int
  -> Gen Expr
arbitraryFiniteExpr zeroExprGen 0 = zeroExprGen
arbitraryFiniteExpr zeroExprGen n = frequency
  [ (1, EApp <$> arbitraryExpr' <*> arbitraryExpr')
  , (1, ENeg <$> arbitraryExpr')
  , (1, EBin <$> arbitrary <*> arbitraryExpr' <*> arbitraryExpr')
  , (1, EIte <$> arbitraryExpr' <*> arbitraryExpr' <*> arbitraryExpr')
  , (1, ECst <$> arbitraryExpr' <*> arbitrary)
  , (1, ELam <$> arbitrary <*> arbitraryExpr')
  , (1, ETApp <$> arbitraryExpr' <*> arbitrary)
  , (1, ETAbs <$> arbitraryExpr' <*> arbitrary)
  , (1, T.PAnd <$> arbitraryExprList)
  , (1, T.POr <$> arbitraryExprList)
  , (1, T.PNot <$> arbitraryExpr')
  , (1, PImp <$> arbitraryExpr' <*> arbitraryExpr')
  , (1, PIff <$> arbitraryExpr' <*> arbitraryExpr')
  , (1, PAtom <$> arbitrary <*> arbitraryExpr' <*> arbitraryExpr')
  , (1, PKVar <$> arbitrary <*> arbitrary)
  , (1, PAll <$> arbitraryList arbitrary <*> arbitraryExpr')
  , (1, PExist <$> arbitraryList arbitrary <*> arbitraryExpr')
  , (1, PGrad <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitraryExpr')
  , (1, ECoerc <$> arbitrary <*> arbitrary <*> arbitraryExpr')
  ]
  where
    arbitraryExpr' = arbitraryFiniteExpr zeroExprGen (n `div` 2)
    arbitraryList :: Gen a -> Gen [a]
    arbitraryList gen = choose (2, 3) >>= (`vectorOf` gen)
    arbitraryExprList = arbitraryList arbitraryExpr'

-- | Generates a finite expression, with the logarithm of the Int given
-- suggesting the depth of the expression tree.
arbitraryExpr :: Int -> Gen Expr
arbitraryExpr = arbitraryFiniteExpr arbitraryAtomicExpr

arbitraryAtomicExpr :: Gen Expr
arbitraryAtomicExpr = (oneof [ESym <$> arbitrary, ECon <$> arbitrary, EVar <$> arbitrary])

arbitraryEqualityConstraint :: Gen Brel
arbitraryEqualityConstraint = oneof [pure Eq, pure Ueq]

-- | Generates an expression that involves the given Symbol in an Eq or Ueq
-- PAtom with an arbitrary non-lq_anf$ EVar.
arbitraryExprInvolving :: Symbol -> Int -> Gen Expr
arbitraryExprInvolving sym = arbitraryFiniteExpr . pure $ EVar sym

instance Arbitrary KVar where
  arbitrary = KV <$> arbitrary

-- NOTE: This dummy Arbitrary instance returns a constant GradInfo.
instance Arbitrary GradInfo where
  arbitrary = pure $ GradInfo (SS pos pos) Nothing
    where pos = Spans.dummyPos "<unknown>"

instance Arbitrary Subst where
  arbitrary = do
    n <- choose (0, 3)
    l <- vectorOf n arbitrary
    return $ Su $ M.fromList l

instance Arbitrary Sort where
  arbitrary = sized arbitrarySort

arbitrarySort :: Int -> Gen Sort
arbitrarySort 0 = oneof (pure <$> [FInt, FReal, FNum, FFrac])
arbitrarySort n = frequency
  [ (1, FObj <$> arbitrary)
  , (1, FVar <$> arbitrary)
  -- , (1, FFunc <$> arbitrarySort' <*> arbitrarySort')
  , (1, FAbs <$> arbitrary <*> arbitrarySort')
  , (1, FTC <$> arbitrary)
  , (1, FApp <$> arbitrarySort' <*> arbitrarySort')
  ]
  where
    arbitrarySort' = arbitrarySort (n `div` 2)

instance Arbitrary Brel where
  arbitrary = oneof (map return [Eq, Ne, Gt, Ge, Lt, Le, Ueq, Une])

instance Arbitrary Bop where
  arbitrary = oneof (map return [Plus, Minus, Times, Div, Mod])

instance Arbitrary SymConst where
  arbitrary = SL . unShortLowercaseAlphabeticText <$> arbitrary

-- | Note that we rely below on the property that the Arbitrary instance for
-- Symbol cannot create lq_anf$ vars.
instance Arbitrary Symbol where
  arbitrary = (symbol :: Text.Text -> Symbol) . unShortLowercaseAlphabeticText <$> arbitrary

newtype ShortLowercaseAlphabeticText = ShortLowercaseAlphabeticText { unShortLowercaseAlphabeticText :: Text.Text }
  deriving (Eq, Show, Generic)

instance Arbitrary ShortLowercaseAlphabeticText where
  arbitrary = ShortLowercaseAlphabeticText <$> (choose (5, 12) >>= \n -> Text.pack <$> (vectorOf n char `suchThat` valid))
    where
      char = elements ['a'..'z']
      valid x = isNotReserved x && not (isFixKey (Text.pack x))

instance Arbitrary FTycon where
  arbitrary = do
    c <- elements ['A'..'Z']
    t <- unShortLowercaseAlphabeticText <$> arbitrary
    return $ symbolFTycon $ dummyLoc $ symbol $ c `Text.cons` t

instance Arbitrary Constant where
  arbitrary = oneof [ I <$> arbitrary `suchThat` (>= 0) -- Negative values use `ENeg`
                    , R <$> arbitrary `suchThat` (>= 0) -- Negative values use `ENeg`
                    , L . unShortLowercaseAlphabeticText <$> arbitrary <*> arbitrary
                    ]

-- | Used in UndoANFTests.
newtype AnfSymbol = AnfSymbol { unAnfSymbol :: Symbol }
  deriving (Eq, Show, Generic)
instance Arbitrary AnfSymbol where
  arbitrary = AnfSymbol . mappendSym anfPrefix <$> arbitrary

-- | This instance does **not** create Refts with anf symbols.
instance Arbitrary Reft where
  arbitrary = reft <$> arbitrary <*> arbitrary

-- | This instance does **not** create SortedRefts with anf symbols.
instance Arbitrary SortedReft where
  arbitrary = arbitrarySortedReft (const arbitrary) 1

arbitrarySortedReft :: (Int -> Gen Symbol) -> Int -> Gen SortedReft
arbitrarySortedReft symGen = \n -> do
  sort <- arbitrary
  eq <- arbitraryEqualityConstraint
  sym <- symGen n
  expr <- arbitrary
  pure $ RR sort $ reft sym (PAtom eq (EVar sym) expr)

-- | Base environment with no declared properties; do not add an Arbitrary
-- instance to this and instead use newtypes.
newtype Env = Env { unEnv :: M.HashMap Symbol SortedReft }
  deriving (Eq, Show, Generic)

-- | Env without anf vars.
newtype NoAnfEnv = NoAnfEnv { unNoAnfEnv :: Env }
  deriving (Eq, Show, Generic)
instance Arbitrary NoAnfEnv where
  arbitrary = NoAnfEnv <$> (arbitraryEnv gen 4)
    where
      -- | Note that this relies on the property that the Arbitrary instance for
      -- Symbol cannot create lq_anf$ vars.
      gen _ = (\a b -> [(a, b)]) <$> arbitrary <*> arbitrary

-- | Env with anf vars that do not reference further anf vars.
newtype FlatAnfEnv = FlatAnfEnv { unFlatAnfEnv :: Env }
  deriving (Eq, Show, Generic)
instance Arbitrary FlatAnfEnv where
  arbitrary = FlatAnfEnv <$> (arbitraryEnv gen 2)
    where
      arbs n = vectorOf n ((,) <$> arbitrary <*> arbitrary)
      anfsGen n = fmap (\(a, b) -> (unAnfSymbol a, b)) <$> arbs n
      gen = finalAnfGen anfsGen finalFlatGen
      finalFlatGen :: [(Symbol, SortedReft)] -> Gen (Symbol, SortedReft)
      -- This creates a final symbol which is either the conjunction or
      -- disjunction of all the anf symbols.
      finalFlatGen anfs = do
        conjOrDisj <- oneof $ pure <$> [T.PAnd, T.POr]
        let ultimateAnfExpr = conjOrDisj $ EVar . fst <$> anfs
        sym <- arbitrary
        ultimateAnfSym <- arbitrary
        sort <- arbitrary
        pure $ (sym, RR sort $ reft ultimateAnfSym (PAtom Eq (EVar ultimateAnfSym) ultimateAnfExpr))

-- | Given a generator for a bunch of (Symbol, SortedReft) pairs which bind
-- lq_anf$ vars, and another generator that takes those pairs and binds a
-- non-lq_anf$ var to some subset of them, this function generates those pairs
-- plus the "final" non-lq_anf$ expression, which represents the "original"
-- expression brought to ANF.
finalAnfGen :: (Int -> Gen [(Symbol, SortedReft)]) -> ([(Symbol, SortedReft)] -> Gen (Symbol, SortedReft)) -> Int -> Gen [(Symbol, SortedReft)]
finalAnfGen anfsGen finalGen = \n -> do
  anfs <- anfsGen n
  ultimateAnf <- finalGen anfs
  pure $ ultimateAnf : anfs

-- | Create an arbitrary env up to size k with the given generator for Symbols
-- and SortedRefts
arbitraryEnv :: (Int -> Gen [(Symbol, SortedReft)]) -> Int -> Gen Env
arbitraryEnv gen = \k -> Env . M.fromList <$> (choose (0, k) >>= gen)

-- | Env with anf vars that form a list of references.
newtype ChainedAnfEnv = ChainedAnfEnv { unChainedAnfEnv :: Env }
  deriving (Eq, Show, Generic)
instance Arbitrary ChainedAnfEnv where
  arbitrary = ChainedAnfEnv <$> (arbitraryEnv gen 4)
    where
      gen = finalAnfGen (chainedAnfGen anfSymNGen) finalChainedGen
      finalChainedGen :: [(Symbol, SortedReft)] -> Gen (Symbol, SortedReft)
      finalChainedGen anfs =
        case anfs of
          -- No ANFs, so just an arbitrary expression will do
          [] -> arbitrary
          ((penultimateSym, _):_) -> do
            sym <- arbitrary
            (,) <$> arbitrary {- symbol -} <*> (
              RR <$> arbitrary {- sort -} <*> (
                pure $ reft sym (PAtom Eq (EVar sym) (EVar penultimateSym))))

-- | Creates a "chain" of referencing `lq_anf$` var Symbols of length `n` such
-- that the first symbol references the second which references the third, and
-- so on.  The last symbol is bound to an arbitrary non-lq_anf$ var.
--
-- This list is in an acceptable form to be passed to `finalAnfGen` to close the
-- loop.
chainedAnfGen :: (Int -> Gen AnfSymbol) -> Int -> Gen [(Symbol, SortedReft)]
chainedAnfGen _ 0 = pure []
chainedAnfGen symGen n = do
  syms <- fmap unAnfSymbol <$> (for [1..n+1] symGen)
  finalSym <- arbitrary
  let symPairs :: [(Symbol, Symbol)]
      symPairs = pairs syms ++ [(last syms, finalSym)]
  for symPairs $ \(sym, prevSym) -> do
    otherSym <- arbitrary
    sort <- arbitrary
    prevSymExpr <- arbitraryExprInvolving prevSym n
    pure (sym, RR sort (reft otherSym (PAtom Eq (EVar otherSym) prevSymExpr)))
  where
    pairs xs = zip xs (tail xs)

-- This is not random, but is simplified so that you can make chains more
-- easily.
anfSymNGen :: Int -> Gen AnfSymbol
anfSymNGen i = pure . AnfSymbol . mappendSym anfPrefix . symbol . show $ i

