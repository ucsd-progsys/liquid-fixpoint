{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Fixpoint.Types.SMTPrint where


import qualified Language.Fixpoint.Misc  as Misc
import qualified Text.PrettyPrint.HughesPJ.Compat as P
import qualified Language.Fixpoint.Types.PrettyPrint as F
import qualified Language.Fixpoint.Types.Names as F
import qualified Language.Fixpoint.Types.Sorts as F
import qualified Language.Fixpoint.Types.Spans as F
import qualified Language.Fixpoint.Types.Refinements as F
-- import qualified Language.Fixpoint.Types.Constraints as F

-----------------------------------------------------------------------------------------------------------------
-- Human readable but robustly parseable SMT-LIB format pretty printer
-----------------------------------------------------------------------------------------------------------------
class ToHornSMT a where
  toHornSMT :: a -> P.Doc


instance ToHornSMT F.Symbol where
  toHornSMT s = F.pprint s



toHornWithBinders :: (ToHornSMT a, ToHornSMT t) => P.Doc -> [(F.Symbol, t)] -> a -> P.Doc
toHornWithBinders name xts p =  P.parens (name P.<+> toHornSMT xts P.<+> toHornSMT p)

instance ToHornSMT a => ToHornSMT (F.Symbol, a) where
  toHornSMT (x, t) = P.parens $ F.pprint x P.<+> toHornSMT t

instance ToHornSMT a => ToHornSMT [a] where
  toHornSMT = toHornMany . fmap toHornSMT

toHornMany :: [P.Doc] -> P.Doc
toHornMany = P.parens . P.sep -- Misc.intersperse " "

toHornAnd :: (a -> P.Doc) -> [a] -> P.Doc
toHornAnd f xs = P.parens (P.vcat ("and" : (P.nest 1 . f <$> xs)))


instance ToHornSMT F.DataDecl where
  toHornSMT (F.DDecl tc n ctors) =
    P.parens $ P.vcat [
      P.text "datatype" P.<+> P.parens (toHornSMT tc P.<+> P.int n)
    , P.parens (P.vcat (toHornSMT <$> ctors))
    ]

instance ToHornSMT F.FTycon where
  toHornSMT c
    | c == F.listFTyCon = "list"
    | otherwise         = toHornSMT (F.symbol c)

instance ToHornSMT a => ToHornSMT (F.Located a) where
  toHornSMT = toHornSMT . F.val

instance ToHornSMT F.DataCtor where
  toHornSMT (F.DCtor x flds) = P.parens (toHornSMT x P.<+> toHornSMT flds)

instance ToHornSMT F.DataField where
  toHornSMT (F.DField x t) = toHornSMT (F.val x, t)

instance ToHornSMT F.Sort where
  toHornSMT = toHornSort

toHornSort :: F.Sort -> P.Doc
toHornSort (F.FVar i)     = "@" P.<-> P.parens (P.int i)
toHornSort F.FInt         = "Int"
toHornSort F.FReal        = "Real"
toHornSort F.FFrac        = "Frac"
toHornSort (F.FObj x)     = toHornSMT x -- P.parens ("obj" P.<+> toHornSMT x)
toHornSort F.FNum         = "num"
toHornSort t@(F.FAbs _ _) = toHornAbsApp t
toHornSort t@(F.FFunc _ _)= toHornAbsApp t
toHornSort (F.FTC c)      = toHornSMT c
toHornSort t@(F.FApp _ _) = toHornFApp (F.unFApp t)

toHornAbsApp :: F.Sort -> P.Doc
toHornAbsApp (F.functionSort -> Just (vs, ss, s)) = P.parens ("func" P.<+> P.int (length vs) P.<+> toHornSMT ss P.<+> toHornSMT s )
toHornAbsApp _                                    = error "Unexpected nothing function sort"

toHornFApp     :: [F.Sort] -> P.Doc
toHornFApp [t] = toHornSMT t
toHornFApp ts  = toHornSMT ts

instance ToHornSMT F.Subst where
  toHornSMT (F.Su m) = toHornSMT (Misc.hashMapToAscList m)



instance ToHornSMT F.KVar where
  toHornSMT (F.KV k) = "$" P.<-> toHornSMT k

instance ToHornSMT F.Expr where
  toHornSMT = toHornExpr

toHornExpr :: F.Expr -> P.Doc
toHornExpr (F.ESym c)        = F.pprint c
toHornExpr (F.ECon c)        = F.pprint c
toHornExpr (F.EVar s)        = toHornSMT s
toHornExpr (F.ENeg e)        = P.parens ("-" P.<+> toHornExpr e)
toHornExpr (F.EApp e1 e2)    = toHornSMT [e1, e2]
toHornExpr (F.EBin o e1 e2)  = toHornOp   (F.toFix o) [e1, e2]
toHornExpr (F.ELet x e1 e2)  = toHornMany ["let", toHornSMT [(x, e1)], toHornSMT e2]
toHornExpr (F.EIte e1 e2 e3) = toHornOp "if"  [e1, e2, e3]
toHornExpr (F.ECst e t)      = toHornMany ["cast", toHornSMT e, toHornSMT t]
toHornExpr (F.PNot p)        = toHornOp "not"  [p]
toHornExpr (F.PImp e1 e2)    = toHornOp "=>"   [e1, e2]
toHornExpr (F.PIff e1 e2)    = toHornOp "<=>"  [e1, e2]
toHornExpr e@F.PTrue         = F.pprint e
toHornExpr e@F.PFalse        = F.pprint e
toHornExpr (F.PAnd es)       = toHornOp "and" es
toHornExpr (F.POr  es)       = toHornOp "or"  es
toHornExpr (F.PAtom r e1 e2) = toHornOp (F.toFix r) [e1, e2]
toHornExpr (F.PAll xts p)    = toHornMany ["forall", toHornSMT xts, toHornSMT p]
toHornExpr (F.PExist xts p)  = toHornMany ["exists", toHornSMT xts, toHornSMT p]
toHornExpr (F.ELam b e)      = toHornMany ["lam", toHornSMT b, toHornSMT e]
toHornExpr (F.ECoerc a t e)  = toHornMany ["coerce", toHornSMT a, toHornSMT t, toHornSMT e]
toHornExpr (F.PKVar k su)    = toHornMany [toHornSMT k, toHornSMT su]
toHornExpr (F.ETApp e s)     = toHornMany ["ETApp" , toHornSMT e, toHornSMT s]
toHornExpr (F.ETAbs e s)     = toHornMany ["ETAbs" , toHornSMT e, toHornSMT s]
toHornExpr (F.PGrad k _ _ e) = toHornMany ["&&", toHornSMT e, toHornSMT k]

toHornOp :: ToHornSMT a => P.Doc -> [a] -> P.Doc
toHornOp op es = toHornMany (op : (toHornSMT <$> es))
