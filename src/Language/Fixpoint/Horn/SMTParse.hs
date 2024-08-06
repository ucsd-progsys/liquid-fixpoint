
{-# LANGUAGE DeriveFunctor #-}

module Language.Fixpoint.Horn.SMTParse (
    hornP
  , hCstrP
  , hPredP
  , hQualifierP
  , hVarP
  , exprP
  , sortP
) where

import qualified Language.Fixpoint.Parse        as FP
import qualified Language.Fixpoint.Types        as F
import qualified Language.Fixpoint.Horn.Types   as H
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char           (string, char)
import qualified Data.HashMap.Strict            as M
import qualified Data.Text as T

type FParser = FP.Parser

fReserved :: String -> FParser ()
fReserved = FP.reserved
fReservedOp :: String -> FParser ()
fReservedOp = FP.reservedOp
fParens :: FParser a -> FParser a
fParens = FP.parens
fStringLiteral :: FParser String
fStringLiteral = FP.stringLiteral
fSymbolP :: FParser F.Symbol
fSymbolP = FP.symbolP
fIntP :: FP.Parser Int
fIntP = FP.intP
fLocUpperIdP :: FP.Parser F.LocSymbol
fLocUpperIdP = FP.locUpperIdP
fLocSymbolP :: FP.Parser F.LocSymbol
fLocSymbolP = FP.locSymbolP
fAddNumTyCon :: F.Symbol -> FP.Parser ()
fAddNumTyCon = FP.addNumTyCon
fSym :: String -> FP.Parser String
fSym = FP.sym
fUpperIdP :: FP.Parser F.Symbol
fUpperIdP = FP.upperIdP
fLowerIdP :: FP.Parser F.Symbol
fLowerIdP = FP.lowerIdP
ffTyConP :: FP.Parser F.FTycon
ffTyConP = FP.fTyConP

fTrueP :: FP.Parser F.Expr
fTrueP = FP.trueP
fFalseP :: FP.Parser F.Expr
fFalseP = FP.falseP
fSymconstP :: FP.Parser F.SymConst
fSymconstP = FP.symconstP
fConstantP :: FP.Parser F.Constant
fConstantP = FP.constantP

-------------------------------------------------------------------------------
hornP :: FParser H.TagQuery
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
  , H.qEqns  =            [ e     | HDef e   <- things ]
  , H.qMats  =            [ m     | HMat m   <- things ]
  , H.qData  =            [ dd    | HDat dd  <- things ]
  , H.qOpts  =            [ o     | HOpt o   <- things ]
  , H.qNums  =            [ s     | HNum s   <- things ]
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
  | HNum  F.Symbol
  deriving (Functor)

hThingP :: FParser (HThing H.Tag)
hThingP  = fParens body
  where
    body =  HQual <$> (fReserved "qualif"     *> hQualifierP)
        <|> HCstr <$> (fReserved "constraint" *> hCstrP)
        <|> HVar  <$> (fReserved "var"        *> hVarP)
        <|> HOpt  <$> (fReserved "fixpoint"   *> fStringLiteral)
        <|> HCon  <$> (fReserved "constant"   *> fSymbolP) <*> sortP
        <|> HDis  <$> (fReserved "distinct"   *> fSymbolP) <*> sortP
        <|> HDef  <$> (fReserved "define"     *> defineP)
        <|> HMat  <$> (fReserved "match"      *> matchP)
        <|> HDat  <$> (fReserved "datatype"   *> dataDeclP)
        <|> HNum  <$> (fReserved "numeric"    *> numericDeclP)

numericDeclP :: FParser F.Symbol
numericDeclP = do
  x <- F.val <$> fLocUpperIdP
  fAddNumTyCon x
  pure x

-------------------------------------------------------------------------------
hCstrP :: FParser (H.Cstr H.Tag)
-------------------------------------------------------------------------------
hCstrP =  try (fParens body)
      <|> H.Head <$> hPredP                            <*> pure H.NoTag
  where
    body =  H.CAnd <$> (fReserved "and"    *> many hCstrP)
        <|> H.All  <$> (fReserved "forall" *> hBindP)  <*> hCstrP
        <|> H.Any  <$> (fReserved "exists" *> hBindP)  <*> hCstrP
        <|> H.Head <$> (fReserved "tag"    *> hPredP)  <*> (H.Tag <$> fStringLiteral)

hBindP :: FParser (H.Bind H.Tag)
hBindP   = fParens $ do
  (x, t) <- symSortP
  H.Bind x t <$> hPredP <*> pure H.NoTag

-------------------------------------------------------------------------------
hPredP :: FParser H.Pred
-------------------------------------------------------------------------------
hPredP = fParens body
  where
    body =  H.Var  <$> kvSymP <*> some fSymbolP
        <|> H.PAnd <$> (fReserved "and" *> some hPredP)
        <|> H.Reft <$> exprP

kvSymP :: FParser F.Symbol
kvSymP = char '$' *> fSymbolP

-------------------------------------------------------------------------------
-- | Qualifiers
-------------------------------------------------------------------------------
hQualifierP :: FParser F.Qualifier
hQualifierP = do
  pos    <- getSourcePos
  n      <- fUpperIdP
  params <- fParens (some symSortP)
  body   <- exprP
  return  $ F.mkQual n (mkParam <$> params) body pos

mkParam :: (F.Symbol, F.Sort) -> F.QualParam
mkParam (x, t) = F.QP x F.PatNone t

-------------------------------------------------------------------------------
-- | Horn Variables
-------------------------------------------------------------------------------

hVarP :: FParser (H.Var H.Tag)
hVarP = H.HVar <$> kvSymP <*> fParens (some sortP) <*> pure H.NoTag

-------------------------------------------------------------------------------
-- | Helpers
-------------------------------------------------------------------------------
sPairP :: FParser a -> FParser b -> FParser (a, b)
sPairP aP bP = fParens ((,) <$> aP <*> bP)

sMany :: FParser a -> FParser [a]
sMany p = fParens (many p)


symSortP :: FParser (F.Symbol, F.Sort)
symSortP = sPairP  fSymbolP sortP
-- symSortP = fParens ((,) <$> fSymbolP <*> sortP)

dataDeclP :: FParser F.DataDecl
dataDeclP = do
  (tc, n) <- sPairP ffTyConP fIntP
  ctors   <- sMany dataCtorP
  pure     $ F.DDecl tc n ctors

dataCtorP :: FParser F.DataCtor
dataCtorP = fParens (F.DCtor <$> fLocSymbolP <*> sMany dataFieldP)

dataFieldP :: FParser F.DataField
dataFieldP = uncurry F.DField <$> sPairP fLocSymbolP sortP

bindsP :: FParser [(F.Symbol, F.Sort)]
bindsP = sMany bindP

bindP :: FParser (F.Symbol, F.Sort)
bindP = sPairP fSymbolP sortP

defineP :: FParser F.Equation
defineP = do
  name   <- fSymbolP
  xts    <- bindsP
  s      <- sortP
  body   <- exprP
  return  $ F.mkEquation name xts body s

matchP :: FParser F.Rewrite
matchP = do
  f    <- fSymbolP
  d:xs <- fParens (some fSymbolP)
  F.SMeasure f d xs <$> exprP

sortP :: FParser F.Sort
sortP =  (string "@" >> (F.FVar <$> fParens fIntP))
     <|> (fReserved "Int"  >> return F.FInt)
     <|> (fReserved "Real" >> return F.FReal)
     <|> (fReserved "Frac" >> return F.FFrac)
     <|> (fReserved "num" >> return  F.FNum)
     <|> (F.fAppTC <$> ffTyConP <*> pure [])
     <|> (F.FObj . F.symbol <$> fLowerIdP)
     <|> try (fParens (fReserved "func" >> (mkFunc <$> fIntP <*> sMany sortP <*> sortP)))
     <|> try (fParens (fReserved "list" >> (mkList <$> sortP)))
     <|> fParens (F.fAppTC <$> ffTyConP <*> many sortP)

mkFunc :: Int -> [F.Sort] -> F.Sort -> F.Sort
mkFunc n ss s = F.mkFFunc n (ss ++ [s])

mkList :: F.Sort -> F.Sort
mkList s = F.fAppTC F.listFTyCon [s]

exprP :: FParser F.Expr
exprP
  =   fTrueP
  <|> fFalseP
  <|> (F.ESym <$> fSymconstP)
  <|> (F.ECon <$> fConstantP)
  <|> (F.EVar <$> fSymbolP)
  <|> fParens pExprP

pExprP :: FParser F.Expr
pExprP
  =   (fReserved   "if"     >> (F.EIte   <$> exprP <*> exprP <*> exprP))
  <|> (fReserved   "lit"    >> (mkLit    <$> fStringLiteral <*> sortP))
  <|> (fReserved   "cast"   >> (F.ECst   <$> exprP <*> sortP))
  <|> (fReserved   "not"    >> (F.PNot   <$> exprP))
  <|> (fReservedOp "=>"     >> (F.PImp   <$> exprP <*> exprP))
  <|> (fReservedOp "<=>"    >> (F.PIff   <$> exprP <*> exprP))
  <|> (fReserved   "and"    >> (F.PAnd   <$> many exprP))
  <|> (fReserved   "or"     >> (F.POr    <$> many exprP))
  <|> (fReserved   "forall" >> (F.PAll   <$> bindsP <*> exprP))
  <|> (fReserved   "exists" >> (F.PExist <$> bindsP <*> exprP))
  <|> (fReserved   "lam"    >> (F.ELam   <$> bindP <*> exprP))
  <|> (fReserved   "coerce" >> (F.ECoerc <$> sortP <*> sortP <*> exprP))
  <|> (fReserved   "ETApp"  >> (F.ETApp  <$> exprP <*> sortP))
  <|> (fReserved   "ETAbs"  >> (F.ETAbs  <$> exprP <*> fSymbolP))
  <|> try (F.EBin  <$> bopP <*> exprP <*> exprP)
  <|> try (F.PAtom <$> brelP <*> exprP <*> exprP)
  <|> try (fSym "-" >> (F.ENeg <$> exprP))
  <|> (mkApp <$> some exprP)

mkLit :: String -> F.Sort -> F.Expr
mkLit l t = F.ECon (F.L (T.pack l) t)

mkApp :: [F.Expr] -> F.Expr
mkApp (e:es) = F.eApps e es
mkApp _      = error "impossible"

bopP :: FParser F.Bop
bopP
  =  (fSym "+"   >> return F.Plus)
 <|> (fSym "-"   >> return F.Minus)
 <|> (fSym "*"   >> return F.Times)
 <|> (fSym "/"   >> return F.Div)
 <|> (fSym "mod" >> return F.Mod)
 <|> (fSym "*."  >> return F.RTimes)
 <|> (fSym "/."  >> return F.RDiv)

brelP :: FParser F.Brel
brelP
  =  (fSym "="  >> return F.Eq)
 <|> (fSym "!=" >> return F.Ne)
 <|> (fSym "~~" >> return F.Ueq)
 <|> (fSym "!~" >> return F.Une)
 <|> (fSym ">=" >> return F.Ge)
 <|> (fSym ">"  >> return F.Gt)
 <|> (fSym "<=" >> return F.Le)
 <|> (fSym "<"  >> return F.Lt)