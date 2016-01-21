module Language.Fixpoint.Types.Bare where

import Language.Fixpoint.Types.Refinements
import Language.Fixpoint.Types.Constraints
import Language.Fixpoint.Types.Sorts
import Language.Fixpoint.Types.Names
import Language.Fixpoint.Misc 

import qualified Data.HashMap.Strict       as M

import Data.Hashable
import Data.Maybe (fromJust)

type Env = [(LocSymbol, Sort)]


ofBexpr :: Env -> BExpr -> Expr 
ofBexpr env = emap (symbolVar env)

symbolVar :: Env -> LocSymbol -> Var 
symbolVar env l 
  = case lookup l env of 
     Nothing -> errorstar $ ("Cannot find symbol" ++ show l)
     Just s  -> locSymbolVar l s 

ofBSubC :: Env -> (Sort, Sort, BSubC a) -> (Integer, SubC a)
ofBSubC env (sl, sr, subc) = (cid, subc {slhs = slhs', srhs = srhs'})
  where
    cid   = fromJust $ ssubCSid subc
    slhs' = ofBreft env (sl, slhs subc)
    srhs' = ofBreft env (sr, srhs subc)

ofBQual :: Env -> BQualifier -> Qualifier 
ofBQual env q = q {q_body = emap (symbolVar env) $ q_body q}

ofBreft :: Env -> (Sort, BReft) -> Reft 
ofBreft env (s, Reft (x, e)) 
  = Reft (locSymbolVar x s, emap (symbolVar env) e)

class ExprMap e where
  emap :: (Eq a, Eq b, Hashable a, Hashable b) => (a -> b) -> e a -> e b 

instance ExprMap (SQualifier s) where
   emap f q = q {q_body = emap f $ q_body q} 

instance ExprMap (SReft s) where
   emap f (Reft (x, e)) = Reft (f x, emap f e)

instance ExprMap (SExpr s) where
   emap f (EVar x)        = EVar $ f x 
   emap f (EApp e1 e2)    = EApp (emap f e1) (emap f e2)
   emap _ (ECon c)        = ECon c 
   emap f (ECst e s)      = ECst (emap f e) s 
   emap f (ETick t e)     = ETick t (emap f e)
   emap f (PKVar k s)     = PKVar k (emap f s)
   emap f (ENeg e)        = ENeg (emap f e)
   emap f (EBin o e1 e2)  = EBin o (emap f e1) (emap f e2)
   emap f (EIte e e1 e2)  = EIte (emap f e) (emap f e1) (emap f e2)
   emap f (PAnd es)       = PAnd (emap f <$> es)
   emap f (POr es)        = POr (emap f <$> es)
   emap f (PNot e)        = PNot (emap f e)
   emap f (PImp e1 e2)    = PImp (emap f e1) (emap f e2)
   emap f (PIff e1 e2)    = PIff (emap f e1) (emap f e2)
   emap f (PAtom b e1 e2) = PAtom b (emap f e1) (emap f e2)
   emap f (PAll xts e)    = PAll   xts (emap f e)
   emap f (PExist xts e)  = PExist xts (emap f e)

instance ExprMap (SSubst t) where
   emap f (Su m) = Su $ M.fromList $ (go <$> M.toList m) 
    where
     go (s,e) = (f s, emap f e)

