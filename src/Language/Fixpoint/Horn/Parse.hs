
{-# LANGUAGE DeriveFunctor #-}

module Language.Fixpoint.Horn.Parse (
    hornP
  , hCstrP
  , hPredP
  , hQualifierP
  , hVarP
) where

import qualified Language.Fixpoint.Parse        as FP
import qualified Language.Fixpoint.Types        as F
import qualified Language.Fixpoint.Horn.Types   as H
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char           (char)
import qualified Data.HashMap.Strict            as M

-------------------------------------------------------------------------------
hornP :: FP.Parser H.TagQuery
-------------------------------------------------------------------------------
hornP = do
  hThings <- many hThingP
  pure (mkQuery hThings)

mkQuery :: [HThing a] -> H.Query a
mkQuery things = H.Query
  { H.qQuals =            [ q     | HQual q  <- things ]
  , H.qVars  =            [ k     | HVar  k  <- things ]
  , H.qCstr  = H.CAnd     [ c     | HCstr c  <- things ]
  , H.qCon   = M.fromList [ (x,t) | HCon x t <- things ]
  , H.qDis   = M.fromList [ (x,t) | HDis x t <- things ]
  , H.qEqns  =            [ e     | HDef e  <- things ]
  , H.qMats  =            [ m     | HMat m  <- things ]
  , H.qData  =            [ dd    | HDat dd <- things ]
  , H.qOpts  =            [ o     | HOpt o  <- things ]
  }

-- | A @HThing@ describes the kinds of things we may see, in no particular order
--   in a .smt2 query file.

data HThing a
  = HQual !F.Qualifier
  | HVar  !(H.Var a)
  | HCstr !(H.Cstr a)

  -- for uninterpred functions and ADT constructors
  | HCon  F.Symbol F.Sort
  | HDis  F.Symbol F.Sort
  | HDef  F.Equation
  | HMat  F.Rewrite
  | HDat !F.DataDecl
  | HOpt !String
  | HNum ()
  deriving (Functor)

hThingP :: FP.Parser (HThing H.Tag)
hThingP  = FP.parens body
  where
    body =  HQual <$> (FP.reserved "qualif"     *> hQualifierP)
        <|> HCstr <$> (FP.reserved "constraint" *> hCstrP)
        <|> HVar  <$> (FP.reserved "var"        *> hVarP)
        <|> HOpt  <$> (FP.reserved "fixpoint"   *> FP.stringLiteral)
        <|> HCon  <$> (FP.reserved "constant"   *> FP.symbolP) <*> sortP
        <|> HDis  <$> (FP.reserved "distinct"   *> FP.symbolP) <*> sortP
        <|> HDef  <$> (FP.reserved "define"     *> defineP)
        <|> HMat  <$> (FP.reserved "match"      *> matchP)
        <|> HDat  <$> (FP.reserved "data"       *> dataDeclP)
        <|> HNum  <$> (FP.reserved "numeric"    *> numericDeclP)



numericDeclP :: FP.Parser ()
numericDeclP = do
  sym <- FP.locUpperIdP
  FP.addNumTyCon (F.val sym)

-------------------------------------------------------------------------------
hCstrP :: FP.Parser (H.Cstr H.Tag)
-------------------------------------------------------------------------------
hCstrP = FP.parens body
  where
    body =  H.CAnd <$> (FP.reserved "and"    *> many hCstrP)
        <|> H.All  <$> (FP.reserved "forall" *> hBindP)      <*> hCstrP
        <|> H.Any  <$> (FP.reserved "exists" *> hBindP)      <*> hCstrP
        <|> H.Head <$> (FP.reserved "tag"    *> hPredP)      <*> (H.Tag <$> FP.stringLiteral)
        <|> H.Head <$> hPredP                             <*> pure H.NoTag

hBindP :: FP.Parser (H.Bind H.Tag)
hBindP   = FP.parens $ do
  (x, t) <- symSortP
  H.Bind x t <$> hPredP <*> pure H.NoTag

-------------------------------------------------------------------------------
hPredP :: FP.Parser H.Pred
-------------------------------------------------------------------------------
hPredP = FP.parens body
  where
    body =  H.Var  <$> kvSymP                           <*> some FP.symbolP
        <|> H.PAnd <$> (FP.reserved "and" *> some hPredP)
        <|> H.Reft <$> predP

kvSymP :: FP.Parser F.Symbol
kvSymP = char '$' *> FP.symbolP

-------------------------------------------------------------------------------
-- | Qualifiers
-------------------------------------------------------------------------------
hQualifierP :: FP.Parser F.Qualifier
hQualifierP = do
  pos    <- getSourcePos
  n      <- FP.upperIdP
  params <- FP.parens (some symSortP)
  body   <- FP.parens predP
  return  $ F.mkQual n (mkParam <$> params) body pos

mkParam :: (F.Symbol, F.Sort) -> F.QualParam
mkParam (x, t) = F.QP x F.PatNone t

-------------------------------------------------------------------------------
-- | Horn Variables
-------------------------------------------------------------------------------

hVarP :: FP.Parser (H.Var H.Tag)
hVarP = H.HVar <$> kvSymP <*> FP.parens (some (FP.parens sortP)) <*> pure H.NoTag

-------------------------------------------------------------------------------
-- | Helpers
-------------------------------------------------------------------------------

symSortP :: FP.Parser (F.Symbol, F.Sort)
symSortP = FP.parens ((,) <$> FP.symbolP <*> sortP)

-------------------------------------------------------------------------------
-- | TODO ---------------------------------------------------------------------
-------------------------------------------------------------------------------

predP :: FP.Parser F.Expr
predP = undefined

sortP :: FP.Parser F.Sort
sortP = undefined

defineP :: FP.Parser F.Equation
defineP = undefined

matchP :: FP.Parser F.Rewrite
matchP = undefined

dataDeclP :: FP.Parser F.DataDecl
dataDeclP = undefined