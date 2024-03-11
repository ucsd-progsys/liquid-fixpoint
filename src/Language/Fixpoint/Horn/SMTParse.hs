
{-# LANGUAGE DeriveFunctor #-}

module Language.Fixpoint.Horn.SMTParse (
    hornP
  , hCstrP
  , hPredP
  , hQualifierP
  , hVarP
  , exprP
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
        <|> HDat  <$> (FP.reserved "datatype"   *> dataDeclP)
        <|> HNum  <$> (FP.reserved "numeric"    *> numericDeclP)



numericDeclP :: FP.Parser ()
numericDeclP = do
  sym <- FP.locUpperIdP
  FP.addNumTyCon (F.val sym)

-------------------------------------------------------------------------------
hCstrP :: FP.Parser (H.Cstr H.Tag)
-------------------------------------------------------------------------------
hCstrP =  try (FP.parens body)
      <|> H.Head <$> hPredP                            <*> pure H.NoTag
  where
    body =  H.CAnd <$> (FP.reserved "and"    *> many hCstrP)
        <|> H.All  <$> (FP.reserved "forall" *> hBindP)  <*> hCstrP
        <|> H.Any  <$> (FP.reserved "exists" *> hBindP)  <*> hCstrP
        <|> H.Head <$> (FP.reserved "tag"    *> hPredP)  <*> (H.Tag <$> FP.stringLiteral)

hBindP :: FP.Parser (H.Bind H.Tag)
hBindP   = FP.parens $ do
  (x, t) <- symSortP
  H.Bind x t <$> hPredP <*> pure H.NoTag

-------------------------------------------------------------------------------
hPredP :: FP.Parser H.Pred
-------------------------------------------------------------------------------
hPredP = FP.parens body
  where
    body =  H.Var  <$> kvSymP <*> some FP.symbolP
        <|> H.PAnd <$> (FP.reserved "and" *> some hPredP)
        <|> H.Reft <$> exprP

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
  body   <- exprP
  return  $ F.mkQual n (mkParam <$> params) body pos

mkParam :: (F.Symbol, F.Sort) -> F.QualParam
mkParam (x, t) = F.QP x F.PatNone t

-------------------------------------------------------------------------------
-- | Horn Variables
-------------------------------------------------------------------------------

hVarP :: FP.Parser (H.Var H.Tag)
hVarP = H.HVar <$> kvSymP <*> FP.parens (some sortP) <*> pure H.NoTag

-------------------------------------------------------------------------------
-- | Helpers
-------------------------------------------------------------------------------
sPairP :: FP.Parser a -> FP.Parser b -> FP.Parser (a, b)
sPairP aP bP = FP.parens ((,) <$> aP <*> bP)

sMany :: FP.Parser a -> FP.Parser [a]
sMany p = FP.parens (many p)


symSortP :: FP.Parser (F.Symbol, F.Sort)
symSortP = sPairP  FP.symbolP sortP
-- symSortP = FP.parens ((,) <$> FP.symbolP <*> sortP)

dataDeclP :: FP.Parser F.DataDecl
dataDeclP = do
  (tc, n) <- sPairP FP.fTyConP FP.intP
  ctors   <- sMany dataCtorP
  pure     $ F.DDecl tc n ctors

dataCtorP :: FP.Parser F.DataCtor
dataCtorP = FP.parens (F.DCtor <$> FP.locSymbolP <*> sMany dataFieldP)

dataFieldP :: FP.Parser F.DataField
dataFieldP = uncurry F.DField <$> sPairP FP.locSymbolP sortP

bindsP :: FP.Parser [(F.Symbol, F.Sort)]
bindsP = sMany bindP

bindP :: FP.Parser (F.Symbol, F.Sort)
bindP = sPairP FP.symbolP sortP

defineP :: FP.Parser F.Equation
defineP = do
  name   <- FP.symbolP
  xts    <- bindsP
  s      <- sortP
  body   <- exprP
  return  $ F.mkEquation name xts body s

matchP :: FP.Parser F.Rewrite
matchP = do
  f    <- FP.symbolP
  d:xs <- FP.parens (some FP.symbolP)
  e    <- exprP
  return (F.SMeasure f d xs e)

sortP :: FP.Parser F.Sort
sortP =  FP.tvarP
     <|> (FP.reserved "Int"  >> return F.FInt)
     <|> (FP.reserved "Real" >> return F.FReal)
     <|> (FP.reserved "Frac" >> return F.FFrac)
     <|> (FP.reserved "num" >> return  F.FNum)
     <|> FP.parens (FP.reserved "func" >> (ffunc <$> FP.intP <*> sMany sortP <*> sortP))
     <|> FP.parens (F.fAppTC <$> FP.fTyConP <*> many sortP)

ffunc :: Int -> [F.Sort] -> F.Sort -> F.Sort
ffunc n ss s = F.mkFFunc n (ss ++ [s])

exprP :: FP.Parser F.Expr
exprP
  =   FP.trueP
  <|> FP.falseP
  <|> (F.ESym <$> FP.symconstP)
  <|> (F.ECon <$> FP.constantP)
  <|> (F.EVar <$> FP.symbolP)
  <|> FP.parens pExprP

pExprP :: FP.Parser F.Expr
pExprP
  =   try (FP.sym "-" >> (F.ENeg <$> exprP))
  <|> (FP.reserved   "if"     >> (F.EIte   <$> exprP <*> exprP <*> exprP))
  <|> (FP.reserved   "cast"   >> (F.ECst   <$> exprP <*> sortP))
  <|> (FP.reserved   "not"    >> (F.PNot   <$> exprP))
  <|> (FP.reservedOp "=>"     >> (F.PImp   <$> exprP <*> exprP))
  <|> (FP.reservedOp "<=>"    >> (F.PIff   <$> exprP <*> exprP))
  <|> (FP.reserved   "and"    >> (F.PAnd   <$> many exprP))
  <|> (FP.reserved   "or"     >> (F.POr    <$> many exprP))
  <|> (FP.reserved   "forall" >> (F.PAll   <$> bindsP <*> exprP))
  <|> (FP.reserved   "exists" >> (F.PExist <$> bindsP <*> exprP))
  <|> (FP.reserved   "lam"    >> (F.ELam   <$> bindP <*> exprP))
  <|> (FP.reserved   "coerce" >> (F.ECoerc <$> sortP <*> sortP <*> exprP))
  <|> (FP.reserved   "ETApp"  >> (F.ETApp  <$> exprP <*> sortP))
  <|> (FP.reserved   "ETAbs"  >> (F.ETAbs  <$> exprP <*> FP.symbolP))
  <|> try (F.EBin  <$> bopP <*> exprP <*> exprP)
  <|> try (F.PAtom <$> brelP <*> exprP <*> exprP)

bopP :: FP.Parser F.Bop
bopP
  =  (FP.sym "+" >> return F.Plus)
 <|> (FP.sym "-" >> return F.Minus)
 <|> (FP.sym "*" >> return F.Times)
 <|> (FP.sym "/" >> return F.Div)
 <|> (FP.sym "mod" >> return F.Div)
 <|> (FP.sym "*." >> return F.RTimes)
 <|> (FP.sym "/." >> return F.RDiv)

brelP :: FP.Parser F.Brel
brelP
  =  (FP.sym "="  >> return F.Eq)
 <|> (FP.sym "!=" >> return F.Ne)
 <|> (FP.sym "~~" >> return F.Ueq)
 <|> (FP.sym "!~" >> return F.Une)
 <|> (FP.sym ">"  >> return F.Gt)
 <|> (FP.sym ">=" >> return F.Ge)
 <|> (FP.sym "<"  >> return F.Lt)
 <|> (FP.sym "<=" >> return F.Le)