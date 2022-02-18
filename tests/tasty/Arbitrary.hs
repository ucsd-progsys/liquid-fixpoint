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
arbitraryExpr_ :: Gen Expr -> Int -> Gen Expr
arbitraryExpr_ zeroExprGen 0 = zeroExprGen
arbitraryExpr_ _ n = frequency
  [ (1, EApp <$> arbitraryExpr' <*> arbitraryExpr')
  , (1, ENeg <$> arbitraryExpr')
  , (1, EBin <$> arbitrary <*> arbitraryExpr' <*> arbitraryExpr')
  , (1, EIte <$> arbitraryExpr' <*> arbitraryExpr' <*> arbitraryExpr')
  , (1, ECst <$> arbitraryExpr' <*> arbitrary)
  , (1, ELam <$> arbitrary <*> arbitraryExpr')
  , (1, ETApp <$> arbitraryExpr' <*> arbitrary)
  , (1, ETAbs <$> arbitraryExpr' <*> arbitrary)
  , (1, T.PAnd <$> arbitraryList)
  , (1, T.POr <$> arbitraryList)
  , (1, T.PNot <$> arbitraryExpr')
  , (1, PImp <$> arbitraryExpr' <*> arbitraryExpr')
  , (1, PIff <$> arbitraryExpr' <*> arbitraryExpr')
  , (1, PAtom <$> arbitrary <*> arbitraryExpr' <*> arbitraryExpr')
  , (1, PKVar <$> arbitrary <*> arbitrary)
  , (1, PAll <$> arbitraryList <*> arbitraryExpr')
  , (1, PExist <$> arbitraryList <*> arbitraryExpr')
  , (1, PGrad <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitraryExpr')
  , (1, ECoerc <$> arbitrary <*> arbitrary <*> arbitraryExpr')
  ]
  where
    arbitraryExpr' = arbitraryExpr (n `div` 2)
    arbitraryList :: Arbitrary a => Gen [a]
    arbitraryList = choose (2, 3) >>= (`vectorOf` arbitrary)

arbitraryExpr :: Int -> Gen Expr
arbitraryExpr = arbitraryExpr_ (oneof [ESym <$> arbitrary, ECon <$> arbitrary, EVar <$> arbitrary])

arbitraryExprInvolving :: Symbol -> Int -> Gen Expr
arbitraryExprInvolving sym = arbitraryExpr_ . pure $ EVar sym

instance Arbitrary KVar where
  arbitrary = KV <$> arbitrary
  shrink (KV s) = KV <$> genericShrink s

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
  , (1, FFunc <$> arbitrarySort' <*> arbitrarySort')
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
  arbitrary = SL <$> arbitrary

-- | Note that we rely below on the property that the Arbitrary instance for
-- Symbol cannot create lq_anf$ vars.
instance Arbitrary Symbol where
  arbitrary = (symbol :: Text.Text -> Symbol) <$> arbitrary

instance Arbitrary Text.Text where
  arbitrary = choose (1, 12) >>= \n -> Text.pack <$> (vectorOf n char `suchThat` valid)
    where
      char = elements ['a'..'z']
      valid x = isNotReserved x && not (isFixKey (Text.pack x))

instance Arbitrary FTycon where
  arbitrary = do
    c <- elements ['A'..'Z']
    t <- arbitrary
    return $ symbolFTycon $ dummyLoc $ symbol $ c `Text.cons` t

instance Arbitrary Constant where
  arbitrary = oneof [ I <$> arbitrary `suchThat` (>= 0) -- Negative values use `ENeg`
                    , R <$> arbitrary `suchThat` (>= 0) -- Negative values use `ENeg`
                    , L <$> arbitrary <*> arbitrary
                    ]
  shrink = genericShrink

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
arbitrarySortedReft symGen = \i -> do
  sort <- arbitrary
  eq <- oneof $ pure <$> [Eq, Ueq]
  sym <- symGen i
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
  arbitrary = NoAnfEnv <$> (arbitraryEnv 4 gen)
    where
      -- | Note that this relies on the property that the Arbitrary instance for
      -- Symbol cannot create lq_anf$ vars.
      symGen = const arbitrary
      reftGen = const arbitrary
      gen = fmap pure . (pairGen symGen reftGen)

pairGen :: (Int -> Gen a) -> (Int -> Gen b) -> Int -> Gen (a, b)
pairGen aGen bGen = \i -> do
  a <- aGen i
  b <- bGen i
  pure (a, b)

-- | Env with anf vars that do not reference further anf vars.
newtype FlatAnfEnv = FlatAnfEnv { unFlatAnfEnv :: Env }
  deriving (Eq, Show, Generic)
instance Arbitrary FlatAnfEnv where
  arbitrary = FlatAnfEnv <$> (arbitraryEnv 4 gen)
    where
      symGen = const arbitrary
      anfGen' = anfGen symGen (const arbitrary)
      gen = finalAnfGen (fmap pure . anfGen')

anfGen :: (Int -> Gen AnfSymbol) -> (Int -> Gen SortedReft) -> Int -> Gen (Symbol, SortedReft)
anfGen symGen sreftGen = \i -> do
  sym <- unAnfSymbol <$> symGen i
  sreft <- sreftGen i
  pure $ (sym, sreft)

finalAnfGen :: (Int -> Gen [(Symbol, SortedReft)]) -> Int -> Gen [(Symbol, SortedReft)]
finalAnfGen anfsGen = \i -> do
  anfs <- anfsGen i
  case anfs of
    [] -> pure []
    ((penultimateSym, _):_) -> do
      ultimateAnf <- do
        sym <- arbitrary
        (,) <$> arbitrary {- symbol -} <*> (
          RR <$> arbitrary {- sort -} <*> (
            pure $ reft sym (PAtom Eq (EVar sym) (EVar penultimateSym))))
      pure $ ultimateAnf : anfs

-- | Create an arbitrary env up to size k with the given generator for Symbols
-- and SortedRefts
arbitraryEnv :: Int -> (Int -> Gen [(Symbol, SortedReft)]) -> Gen Env
arbitraryEnv k gen = Env . M.fromList <$> (choose (0, k) >>= gen)

-- | Env with anf vars that form a list of references.
newtype ChainedAnfEnv = ChainedAnfEnv { unChainedAnfEnv :: Env }
  deriving (Eq, Show, Generic)
instance Arbitrary ChainedAnfEnv where
  arbitrary = ChainedAnfEnv <$> (arbitraryEnv 8 gen)
    where
      gen = finalAnfGen $ chainedAnfGen anfSymNGen

chainedAnfGen :: (Int -> Gen AnfSymbol) -> Int -> Gen [(Symbol, SortedReft)]
chainedAnfGen symGen = \i -> do
  syms <- fmap unAnfSymbol <$> (for [0..i] symGen)
  let symPairs :: [(Symbol, Symbol)]
      symPairs = pairs syms
      anfPairs :: Gen [(Symbol, SortedReft)]
      anfPairs = traverse (\(sym, prevSym) -> do
                              otherSym <- arbitrary
                              (sym,) <$> (RR <$> arbitrary <*> (reft otherSym <$> (PAtom Eq (EVar otherSym) <$> (arbitraryExprInvolving prevSym i))))) symPairs
  anfPairs
  where
    pairs xs = zip xs (tail xs)

-- This is not random, but is simplified so that you can make chains more
-- easily.
anfSymNGen :: Int -> Gen AnfSymbol
anfSymNGen i = pure . AnfSymbol . mappendSym anfPrefix . symbol . show $ i

