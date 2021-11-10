module Language.Fixpoint.Solver.Normalize (normalize) where 

import  Language.Fixpoint.Types 

-------------------------------------------------------------------------------
-- | Normalization of Equation: make their arguments unique -------------------
-------------------------------------------------------------------------------

class Normalizable a where 
  normalize :: a -> a 

instance Normalizable (GInfo c a) where 
  normalize si = si {ae = normalize $ ae si}

instance Normalizable AxiomEnv where 
  normalize aenv = aenv { aenvEqs   = (normalize <$> aenvEqs   aenv)
                        , aenvSimpl = (normalize <$> aenvSimpl aenv) }

instance Normalizable Rewrite where 
  normalize rw = rw { smArgs = xs', smBody = normalizeBody (smName rw) $ subst su $ smBody rw }
    where 
      su  = mkSubst $ zipWith (\x y -> (x,EVar y)) xs xs'
      xs  = smArgs rw 
      xs' = zipWith mkSymbol xs [0..]
      mkSymbol x i = x `suffixSymbol` intSymbol (smName rw) i 

instance Normalizable Equation where 
  normalize eq = eq {eqArgs = zip xs' ss, 
                     eqBody = normalizeBody (eqName eq) $ subst su $ eqBody eq }
    where 
      su           = mkSubst $ zipWith (\x y -> (x,EVar y)) xs xs'
      (xs,ss)      = unzip (eqArgs eq) 
      xs'          = zipWith mkSymbol xs [0..]
      mkSymbol x i = x `suffixSymbol` intSymbol (eqName eq) i 

normalizeBody :: Symbol -> Expr -> Expr
normalizeBody f = go   
  where 
    go e 
      | any (== f) (syms e) 
      = go' e 
    go e 
      = e 
    
    go' (PAnd [PImp c e1,PImp (PNot c') e2])
      | c == c' = EIte c e1 (go' e2)
    go' e = e 
