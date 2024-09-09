
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

import qualified Language.Fixpoint.Parse        as FP (Parser, addNumTyCon, lexeme', locLexeme', reserved', reservedOp', symbolR, upperIdR, lowerIdR, stringR, naturalR, mkFTycon)
import qualified Language.Fixpoint.Types        as F
import qualified Language.Fixpoint.Horn.Types   as H
import           Text.Megaparsec                hiding (State)
import           Text.Megaparsec.Char           (space1, string, char)
import qualified Data.HashMap.Strict            as M
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer  as L

type FParser = FP.Parser

fAddNumTyCon :: F.Symbol -> FP.Parser ()
fAddNumTyCon = FP.addNumTyCon

lexeme :: FParser a -> FParser a
lexeme = FP.lexeme' spaces

locLexeme :: FP.Parser a -> FP.Parser (F.Located a)
locLexeme = FP.locLexeme' spaces

-- | Consumes all whitespace, including LH comments.
--
-- Should not be used directly, but primarily via 'lexeme'.
--
-- The only "valid" use case for spaces is in top-level parsing
-- function, to consume initial spaces.
--
spaces :: FParser ()
spaces =
  L.space
    space1
    lineComment
    blockComment

lineComment :: FParser ()
lineComment = L.skipLineComment ";"

blockComment :: FParser ()
blockComment = L.skipBlockComment "/* " "*/"

reserved :: String -> FParser ()
reserved = FP.reserved' spaces

reservedOp :: String -> FParser ()
reservedOp = FP.reservedOp' spaces

sym :: String -> FParser String
sym x = lexeme (string x)

parens :: FParser a -> FParser a
parens = between (sym "(") (sym ")")

stringLiteral :: FParser String
stringLiteral = lexeme FP.stringR <?> "string literal"

symbolP :: FParser F.Symbol
symbolP = lexeme FP.symbolR <?> "identifier"

fIntP :: FParser Int
fIntP = fromInteger <$> natural

natural :: FParser Integer
natural = lexeme FP.naturalR <?> "nat literal"

double :: FParser Double
double = lexeme L.float <?> "float literal"


locUpperIdP, locSymbolP :: FParser F.LocSymbol
locUpperIdP = locLexeme FP.upperIdR
locSymbolP  = locLexeme FP.symbolR

upperIdP :: FP.Parser F.Symbol
upperIdP = lexeme FP.upperIdR <?> "upperIdP"

lowerIdP :: FP.Parser F.Symbol
lowerIdP = lexeme FP.lowerIdR <?> "upperIdP"

fTyConP :: FParser F.FTycon
fTyConP
  =   (reserved "int"     >> return F.intFTyCon)
  <|> (reserved "Integer" >> return F.intFTyCon)
  <|> (reserved "Int"     >> return F.intFTyCon)
  <|> (reserved "real"    >> return F.realFTyCon)
  <|> (reserved "bool"    >> return F.boolFTyCon)
  <|> (reserved "num"     >> return F.numFTyCon)
  <|> (reserved "Str"     >> return F.strFTyCon)
  <|> (FP.mkFTycon        =<<  locUpperIdP)


fTrueP :: FP.Parser F.Expr
fTrueP = reserved "true"  >> return F.PTrue

fFalseP :: FP.Parser F.Expr
fFalseP = reserved "false" >> return F.PFalse

fSymconstP :: FP.Parser F.SymConst
fSymconstP =  F.SL . T.pack <$> stringLiteral

-- | Parser for literal numeric constants: floats or integers without sign.
constantP :: FParser F.Constant
constantP =
     try (F.R <$> double)   -- float literal
 <|> F.I <$> natural        -- nat literal

-------------------------------------------------------------------------------
hornP :: FParser H.TagQuery
-------------------------------------------------------------------------------
hornP = do
  spaces
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
hThingP  = spaces >> parens body
  where
    body =  HQual <$> (reserved "qualif"     *> hQualifierP)
        <|> HCstr <$> (reserved "constraint" *> hCstrP)
        <|> HVar  <$> (reserved "var"        *> hVarP)
        <|> HOpt  <$> (reserved "fixpoint"   *> stringLiteral)
        <|> HCon  <$> (reserved "constant"   *> symbolP) <*> sortP
        <|> HDis  <$> (reserved "distinct"   *> symbolP) <*> sortP
        <|> HDef  <$> (reserved "define"     *> defineP)
        <|> HMat  <$> (reserved "match"      *> matchP)
        <|> HDat  <$> (reserved "datatype"   *> dataDeclP)
        <|> HNum  <$> (reserved "numeric"    *> numericDeclP)

numericDeclP :: FParser F.Symbol
numericDeclP = do
  x <- F.val <$> locUpperIdP
  fAddNumTyCon x
  pure x

-------------------------------------------------------------------------------
hCstrP :: FParser (H.Cstr H.Tag)
-------------------------------------------------------------------------------
hCstrP =  try (parens body)
      <|> H.Head <$> hPredP                            <*> pure H.NoTag
  where
    body =  H.CAnd <$> (reserved "and"    *> many hCstrP)
        <|> H.All  <$> (reserved "forall" *> hBindP)  <*> hCstrP
        <|> H.Any  <$> (reserved "exists" *> hBindP)  <*> hCstrP
        <|> H.Head <$> (reserved "tag"    *> hPredP)  <*> (H.Tag <$> stringLiteral)

hBindP :: FParser (H.Bind H.Tag)
hBindP   = parens $ do
  (x, t) <- symSortP
  H.Bind x t <$> hPredP <*> pure H.NoTag

-------------------------------------------------------------------------------
hPredP :: FParser H.Pred
-------------------------------------------------------------------------------
hPredP = parens body
  where
    body =  H.Var  <$> kvSymP <*> some symbolP
        <|> H.PAnd <$> (reserved "and" *> some hPredP)
        <|> H.Reft <$> exprP

kvSymP :: FParser F.Symbol
kvSymP = char '$' *> symbolP

-------------------------------------------------------------------------------
-- | Qualifiers
-------------------------------------------------------------------------------
hQualifierP :: FParser F.Qualifier
hQualifierP = do
  pos    <- getSourcePos
  n      <- upperIdP
  params <- parens (some symSortP)
  body   <- exprP
  return  $ F.mkQual n (mkParam <$> params) body pos

mkParam :: (F.Symbol, F.Sort) -> F.QualParam
mkParam (x, t) = F.QP x F.PatNone t

-------------------------------------------------------------------------------
-- | Horn Variables
-------------------------------------------------------------------------------

hVarP :: FParser (H.Var H.Tag)
hVarP = H.HVar <$> kvSymP <*> parens (some sortP) <*> pure H.NoTag

-------------------------------------------------------------------------------
-- | Helpers
-------------------------------------------------------------------------------
sPairP :: FParser a -> FParser b -> FParser (a, b)
sPairP aP bP = parens ((,) <$> aP <*> bP)

sMany :: FParser a -> FParser [a]
sMany p = parens (many p)


symSortP :: FParser (F.Symbol, F.Sort)
symSortP = sPairP  symbolP sortP
-- symSortP = fParens ((,) <$> fSymbolP <*> sortP)

dataDeclP :: FParser F.DataDecl
dataDeclP = do
  (tc, n) <- sPairP fTyConP fIntP
  ctors   <- sMany dataCtorP
  pure     $ F.DDecl tc n ctors

dataCtorP :: FParser F.DataCtor
dataCtorP = parens (F.DCtor <$> locSymbolP <*> sMany dataFieldP)

dataFieldP :: FParser F.DataField
dataFieldP = uncurry F.DField <$> sPairP locSymbolP sortP

bindsP :: FParser [(F.Symbol, F.Sort)]
bindsP = sMany bindP

bindP :: FParser (F.Symbol, F.Sort)
bindP = sPairP symbolP sortP

defineP :: FParser F.Equation
defineP = do
  name   <- symbolP
  xts    <- bindsP
  s      <- sortP
  body   <- exprP
  return  $ F.mkEquation name xts body s

matchP :: FParser F.Rewrite
matchP = do
  f    <- symbolP
  d:xs <- parens (some symbolP)
  F.SMeasure f d xs <$> exprP

sortP :: FParser F.Sort
sortP =  (string "@" >> (F.FVar <$> parens fIntP))
     <|> (reserved "Int"  >> return F.FInt)
     <|> (reserved "Real" >> return F.FReal)
     <|> (reserved "Frac" >> return F.FFrac)
     <|> (reserved "num" >> return  F.FNum)
     <|> (F.fAppTC <$> fTyConP <*> pure [])
     <|> (F.FObj . F.symbol <$> lowerIdP)
     <|> try (parens (reserved "func" >> (mkFunc <$> fIntP <*> sMany sortP <*> sortP)))
     <|> try (parens (reserved "list" >> (mkList <$> sortP)))
     <|> parens (F.fAppTC <$> fTyConP <*> many sortP)

mkFunc :: Int -> [F.Sort] -> F.Sort -> F.Sort
mkFunc n ss s = F.mkFFunc n (ss ++ [s])

mkList :: F.Sort -> F.Sort
mkList s = F.fAppTC F.listFTyCon [s]

exprP :: FParser F.Expr
exprP
  =   fTrueP
  <|> fFalseP
  <|> (F.ESym <$> fSymconstP)
  <|> (F.ECon <$> constantP)
  <|> (F.EVar <$> symbolP)
  <|> parens pExprP

pExprP :: FParser F.Expr
pExprP
  =   (reserved   "if"     >> (F.EIte   <$> exprP <*> exprP <*> exprP))
  <|> (reserved   "lit"    >> (mkLit    <$> stringLiteral <*> sortP))
  <|> (reserved   "cast"   >> (F.ECst   <$> exprP <*> sortP))
  <|> (reserved   "not"    >> (F.PNot   <$> exprP))
  <|> (reservedOp "=>"     >> (F.PImp   <$> exprP <*> exprP))
  <|> (reservedOp "<=>"    >> (F.PIff   <$> exprP <*> exprP))
  <|> (reserved   "and"    >> (F.PAnd   <$> many exprP))
  <|> (reserved   "or"     >> (F.POr    <$> many exprP))
  <|> (reserved   "forall" >> (F.PAll   <$> bindsP <*> exprP))
  <|> (reserved   "exists" >> (F.PExist <$> bindsP <*> exprP))
  <|> (reserved   "lam"    >> (F.ELam   <$> bindP <*> exprP))
  <|> (reserved   "coerce" >> (F.ECoerc <$> sortP <*> sortP <*> exprP))
  <|> (reserved   "ETApp"  >> (F.ETApp  <$> exprP <*> sortP))
  <|> (reserved   "ETAbs"  >> (F.ETAbs  <$> exprP <*> symbolP))
  <|> try (F.EBin  <$> bopP <*> exprP <*> exprP)
  <|> try (F.PAtom <$> brelP <*> exprP <*> exprP)
  <|> try (sym "-" >> (F.ENeg <$> exprP))
  <|> (mkApp <$> some exprP)

mkLit :: String -> F.Sort -> F.Expr
mkLit l t = F.ECon (F.L (T.pack l) t)

mkApp :: [F.Expr] -> F.Expr
mkApp (e:es) = F.eApps e es
mkApp _      = error "impossible"

bopP :: FParser F.Bop
bopP
  =  (sym "+"   >> return F.Plus)
 <|> (sym "-"   >> return F.Minus)
 <|> (sym "*"   >> return F.Times)
 <|> (sym "/"   >> return F.Div)
 <|> (sym "mod" >> return F.Mod)
 <|> (sym "*."  >> return F.RTimes)
 <|> (sym "/."  >> return F.RDiv)

brelP :: FParser F.Brel
brelP
  =  (sym "="  >> return F.Eq)
 <|> (sym "!=" >> return F.Ne)
 <|> (sym "~~" >> return F.Ueq)
 <|> (sym "!~" >> return F.Une)
 <|> (sym ">=" >> return F.Ge)
 <|> (sym ">"  >> return F.Gt)
 <|> (sym "<=" >> return F.Le)
 <|> (sym "<"  >> return F.Lt)