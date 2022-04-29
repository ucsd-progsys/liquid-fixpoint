{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary
  ( subexprs
  , Env(..)
  , NoAnfEnv(..)
  , AnfSymbol(..)
  , FlatAnfEnv(..)
  , ChainedAnfEnv(..)
  ) where

import qualified Data.Text                 as Text
import qualified Data.HashMap.Strict       as M
import Test.Tasty.QuickCheck
import GHC.Generics

import Language.Fixpoint.Types.Refinements as R
import Language.Fixpoint.Parse             (isNotReserved)
import Language.Fixpoint.Types             as T hiding (Result)
import Language.Fixpoint.Types.Spans       as Spans
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

-- NOTE: `shrink _ = mempty` is identical to the default (implicit) shrink implementation.
-- We prefer to make it explicit.

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
  , (1, do
          e <- EBin <$> arbitrary <*> arbitraryExpr' <*> arbitraryExpr'
          return $ if divZero e then discard e else e)
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

    divZero :: Expr -> Bool
    divZero (EBin Mod (ECon (I _)) (ECon (I 0))) = True
    divZero (EBin Mod (ECon (R _)) (ECon (R 0.0))) = True
    divZero (EBin Div (ECon (I _)) (ECon (I 0))) = True
    divZero (EBin Div (ECon (R _)) (ECon (R 0.0))) = True
    divZero _ = False

-- | Generates a finite expression, with the logarithm of the Int given
-- suggesting the depth of the expression tree.
arbitraryExpr :: Int -> Gen Expr
arbitraryExpr = arbitraryFiniteExpr arbitraryAtomicExpr

arbitraryAtomicExpr :: Gen Expr
arbitraryAtomicExpr = oneof [ESym <$> arbitrary, ECon <$> arbitrary, EVar <$> arbitrary]

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
  shrink _ = mempty

instance Arbitrary Subst where
  arbitrary = do
    n <- choose (0, 3)
    l <- vectorOf n arbitrary
    return $ Su $ M.fromList l
  shrink _ = mempty

-- | This instance only creates `FVar` when they would be in scope from an
-- enclosing `FAbs`, and does not create `FObj`s
instance Arbitrary Sort where
  arbitrary = sized arbitrarySort
  shrink = genericShrink

-- | Create an arbitrary well-formed sort that does not contain `FObj`s.
--
-- The sort is \"well-formed\" in the sense that all `FVar`s have an enclosing
-- `FAbs` bringing them into scope.
arbitrarySort :: Int -> Gen Sort
arbitrarySort = arbitrarySortPossiblyInvolving []

-- | Create an arbitrary sort, possibly involving the variables represented by
-- the list of Ints. Can possibly create a `FAbs` that will also possibly
-- reference the new variable in an `FVar`, even when the first argument is
-- [].
arbitrarySortPossiblyInvolving :: [Int] -> Int -> Gen Sort
arbitrarySortPossiblyInvolving [] n = frequency
  [ (4, arbitrarySortNoAbs n)
  , (1, newAbs [] n) ]
arbitrarySortPossiblyInvolving vars n = do
  let fvar = oneof $ pure . FVar <$> vars
  frequency
    [ (1, arbitrarySortNoAbs n)
    , (1, FFunc <$> fvar <*> arbitrarySortPossiblyInvolving vars (n `div` 2))
    , (1, FFunc <$> arbitrarySortPossiblyInvolving vars (n `div` 2) <*> fvar)
    , (1, FApp <$> fvar <*> arbitrarySortPossiblyInvolving vars (n `div` 2))
    , (1, FApp <$> arbitrarySortPossiblyInvolving vars (n `div` 2) <*> fvar)
    , (1, fvar)
    , (1, newAbs vars n)
    ]

-- | Create a new FAbs sort whose body might involve the newly created variable.
-- First argument is the variables already in scope.
newAbs :: [Int] -> Int -> Gen Sort
newAbs vars n = do
  v <- arbitrary
  if v `elem` vars
    then discard v
    else FAbs <$> pure v <*> arbitrarySortPossiblyInvolving (v:vars) (n `div` 2)

-- | Does not create FObj, FAbs, or FVar
arbitrarySortNoAbs :: Int -> Gen Sort
arbitrarySortNoAbs 0 = oneof (pure <$> [FInt, FReal, FNum, FFrac])
arbitrarySortNoAbs n = frequency
  [ (1, FFunc <$> arbitrarySortNoAbs' <*> arbitrarySortNoAbs')
  , (1, FTC <$> arbitrary)
  , (1, FApp <$> arbitrarySortNoAbs' <*> arbitrarySortNoAbs')
  ]
  where
    arbitrarySortNoAbs' = arbitrarySortNoAbs (n `div` 2)

instance Arbitrary Brel where
  arbitrary = oneof (map return [Eq, Ne, Gt, Ge, Lt, Le, Ueq, Une])
  shrink _ = mempty

instance Arbitrary Bop where
  arbitrary = oneof (map return [Plus, Minus, Times, Div, Mod])
  shrink _ = mempty

instance Arbitrary SymConst where
  arbitrary = SL . unShortLowercaseAlphabeticText <$> arbitrary

-- | Note that we rely below on the property that the Arbitrary instance for
-- Symbol cannot create lq_anf$ vars.
instance Arbitrary Symbol where
  arbitrary = (symbol :: Text.Text -> Symbol) . unShortLowercaseAlphabeticText <$> arbitrary
  shrink _ = mempty

newtype ShortLowercaseAlphabeticText = ShortLowercaseAlphabeticText { unShortLowercaseAlphabeticText :: Text.Text }
  deriving (Eq, Show, Generic)

instance Arbitrary ShortLowercaseAlphabeticText where
  arbitrary = ShortLowercaseAlphabeticText <$> (choose (5, 12) >>= \n -> Text.pack <$> (vectorOf n char `suchThat` valid))
    where
      char = elements ['a'..'z']
      valid x = isNotReserved x && not (isFixKey (Text.pack x))
  shrink _ = mempty

instance Arbitrary FTycon where
  arbitrary = do
    c <- elements ['A'..'Z']
    t <- unShortLowercaseAlphabeticText <$> arbitrary
    return $ symbolFTycon $ dummyLoc $ symbol $ c `Text.cons` t
  shrink _ = mempty

instance Arbitrary Constant where
  arbitrary = oneof [ I <$> arbitrary `suchThat` (>= 0) -- Negative values use `ENeg`
                    , R <$> arbitrary `suchThat` (>= 0) -- Negative values use `ENeg`
                    , L . unShortLowercaseAlphabeticText <$> arbitrary <*> arbitrary
                    ]
  shrink (I x) = I <$> shrink x
  shrink (R x) = R <$> shrink x
  shrink (L x y) = L <$> pure x <*> shrink y

-- | Used in UndoANFTests.
newtype AnfSymbol = AnfSymbol { unAnfSymbol :: Symbol }
  deriving (Eq, Show, Generic)
instance Arbitrary AnfSymbol where
  arbitrary = AnfSymbol . mappendSym anfPrefix <$> arbitrary
  shrink = mempty

-- | This instance does **not** create Refts with anf symbols.
instance Arbitrary Reft where
  arbitrary = reft <$> arbitrary <*> arbitrary
  shrink = genericShrink

-- | This instance does **not** create SortedRefts with anf symbols.
instance Arbitrary SortedReft where
  arbitrary = sized $ arbitrarySortedReft (const arbitrary) (const arbitrary)
  shrink = genericShrink

arbitrarySortedReft :: (Int -> Gen Sort) -> (Int -> Gen Symbol) -> Int -> Gen SortedReft
arbitrarySortedReft sortGen symGen n = do
  sort <- sortGen n
  eq <- arbitraryEqualityConstraint
  sym <- symGen n
  expr <- arbitrary
  pure $ RR sort $ reft sym (PAtom eq (EVar sym) expr)

newtype IntSortedReft = IntSortedReft { unIntSortedReft :: SortedReft }
  deriving (Eq, Show)

instance Arbitrary IntSortedReft where
  arbitrary = sized $ fmap IntSortedReft . arbitrarySortedReft (const . pure $ FInt) (const arbitrary)

-- | Base environment with no declared properties; do not add an Arbitrary
-- instance to this and instead use newtypes.
newtype Env = Env { unEnv :: [(Symbol, SortedReft)] }
  deriving (Eq, Show, Generic)

shrinkEnv :: Env -> [Env]
shrinkEnv = fmap Env . traverse (traverse shrink) . unEnv

-- | Env without anf vars.
newtype NoAnfEnv = NoAnfEnv { unNoAnfEnv :: Env }
  deriving (Eq, Show, Generic)
instance Arbitrary NoAnfEnv where
  arbitrary = sized (fmap NoAnfEnv . arbitraryEnv gen)
    where
      -- | Note that this relies on the property that the Arbitrary instance for
      -- Symbol cannot create lq_anf$ vars.
      gen n = vectorOf n ((\a b -> (a, unIntSortedReft b)) <$> arbitrary <*> arbitrary)
  shrink = fmap NoAnfEnv . shrinkEnv . unNoAnfEnv

-- | Env with anf vars that do not reference further anf vars.
newtype FlatAnfEnv = FlatAnfEnv { unFlatAnfEnv :: Env }
  deriving (Eq, Show, Generic)
instance Arbitrary FlatAnfEnv where
  arbitrary = sized (fmap FlatAnfEnv . arbitraryEnv gen)
    where
      anfsGen n = vectorOf n ((\a b -> (unAnfSymbol a, unIntSortedReft b)) <$> arbitrary <*> arbitrary)
      gen = finalAnfGen anfsGen finalFlatGen
      finalFlatGen :: [(Symbol, SortedReft)] -> Gen (Symbol, SortedReft)
      -- This creates a final symbol which is either the conjunction or
      -- disjunction of all the anf symbols.
      finalFlatGen anfs = do
        conjOrDisj <- oneof $ pure <$> [T.PAnd, T.POr]
        let ultimateAnfExpr = conjOrDisj $ EVar . fst <$> anfs
        sym <- arbitrary
        ultimateAnfSym <- arbitrary
        pure (sym, RR FInt (reft ultimateAnfSym (PAtom Eq (EVar ultimateAnfSym) ultimateAnfExpr)))
  -- TODO
  shrink (FlatAnfEnv (Env (x:xs))) = pure . FlatAnfEnv . Env $ xs
  shrink _ = mempty

-- | Given a generator for a bunch of (`Symbol`, `SortedReft`) pairs which bind
-- lq_anf$ vars, and another generator that takes those pairs and binds a
-- non-lq_anf$ var to some subset of them, this function generates those pairs
-- plus the \"final\" non-lq_anf$ expression, which represents the \"original\"
-- expression brought to ANF.
finalAnfGen :: (Int -> Gen [(Symbol, SortedReft)]) -> ([(Symbol, SortedReft)] -> Gen (Symbol, SortedReft)) -> Int -> Gen [(Symbol, SortedReft)]
finalAnfGen anfsGen finalGen n = do
  anfs <- anfsGen n
  ultimateAnf <- finalGen anfs
  pure $ ultimateAnf : anfs

-- | Create an arbitrary env up to size k with the given generator for Symbols
-- and SortedRefts
arbitraryEnv :: (Int -> Gen [(Symbol, SortedReft)]) -> Int -> Gen Env

arbitraryEnv gen k = Env <$> (choose (0, k) >>= gen)

-- | Env with anf vars that form a list of references.
newtype ChainedAnfEnv = ChainedAnfEnv { unChainedAnfEnv :: Env }
  deriving (Eq, Show, Generic)
instance Arbitrary ChainedAnfEnv where
  arbitrary = sized (fmap ChainedAnfEnv . arbitraryEnv gen)
    where
      gen = finalAnfGen (chainedAnfGen anfSymNGen) finalChainedGen
      finalChainedGen :: [(Symbol, SortedReft)] -> Gen (Symbol, SortedReft)
      finalChainedGen anfs =
        case anfs of
          -- No ANFs, so just an arbitrary int sorted expression will do
          [] -> fmap unIntSortedReft <$> arbitrary
          ((penultimateSym, _):_) -> do
            sym <- arbitrary
            let sreft = RR FInt (reft sym (PAtom Eq (EVar sym) (EVar penultimateSym)))
            (, sreft) <$> arbitrary
  -- TODO
  shrink (ChainedAnfEnv (Env (x:xs))) = pure . ChainedAnfEnv . Env $ xs
  shrink _ = mempty

-- | Creates a "chain" of referencing `lq_anf$` var Symbols of length `n` such
-- that the first symbol references the second which references the third, and
-- so on.  The last symbol is bound to an arbitrary non-lq_anf$ var.
--
-- This list is in an acceptable form to be passed to `finalAnfGen` to close the
-- loop.
chainedAnfGen :: (Int -> Gen AnfSymbol) -> Int -> Gen [(Symbol, SortedReft)]
chainedAnfGen _ 0 = pure []
chainedAnfGen symGen n = do
  syms <- fmap unAnfSymbol <$> for [1..n+1] symGen
  finalSym <- arbitrary
  let symPairs :: [(Symbol, Symbol)]
      symPairs = pairs (syms ++ [finalSym])
  for symPairs $ \(sym, prevSym) -> do
    otherSym <- arbitrary
    prevSymExpr <- arbitraryExprInvolving prevSym n
    pure (sym, RR FInt (reft otherSym (PAtom Eq (EVar otherSym) prevSymExpr)))
  where
    pairs xs = zip xs (tail xs)

-- This is not random, but is simplified so that you can make chains more
-- easily.
anfSymNGen :: Int -> Gen AnfSymbol
anfSymNGen i = pure . AnfSymbol . mappendSym anfPrefix . symbol . show $ i
