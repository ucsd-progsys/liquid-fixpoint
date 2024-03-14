{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE FlexibleContexts     #-}

{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Fixpoint.Solver.Extensionality (expand) where

import           Control.Monad.State
import qualified Data.HashMap.Strict       as M
import           Data.Maybe  (fromMaybe)
import           Data.List (foldl')

import           Language.Fixpoint.Types.Config
import           Language.Fixpoint.SortCheck
import           Language.Fixpoint.Solver.Sanitize (symbolEnv)
import           Language.Fixpoint.Types hiding (mapSort, Pos)
import           Language.Fixpoint.Types.Visitor (mapSort)

mytracepp :: (PPrint a) => String -> a -> a
mytracepp = notracepp

expand :: Config -> SInfo a -> SInfo a
expand cfg si = evalState (ext si) $ initST (symbolEnv cfg si) (ddecls si)
  where
    ext ::SInfo a -> Ex a (SInfo a)
    ext = extend


class Extend ann a where
  extend :: a -> Ex ann a


instance Extend a (SInfo a) where
  extend si = do
    setBEnv (bs si)
    cm'      <- extend (cm si)
    bs'      <- gets exbenv
    return $ si{ cm = cm' , bs = bs' }

instance (Extend ann a) => Extend ann (M.HashMap SubcId a) where
  extend h = M.fromList <$> mapM extend (M.toList h)

instance (Extend ann a, Extend ann b) => Extend ann (a,b) where
  extend (a,b) = (,) <$> extend a <*> extend b

instance Extend ann SubcId where
  extend i = return i

instance Extend a (SimpC a) where
  extend c = do
    setExBinds (_cenv c)
    rhs <- extendExpr (sinfo c) Pos (_crhs c)
    is  <- gets exbinds
    return $ c{_crhs = rhs, _cenv = is }


extendExpr :: a -> Pos -> Expr -> Ex a Expr
extendExpr ann p expr'
  | p == Pos
  = mapMPosExpr Pos goP e' >>= mapMPosExpr Pos goN
  | otherwise
  = mapMPosExpr Neg goP e' >>= mapMPosExpr Neg goN
    where
      e' = normalize expr'
      goP Pos (PAtom b e1 e2)
       | b == Eq || b == Ne
       , Just s <- getArg (exprSort "extensionality" e1)
       = mytracepp ("extending POS = " ++ showpp expr') <$> (extendRHS ann b e1 e2 s >>= goP Pos)
      goP _ e = return e
      goN Neg (PAtom b e1 e2)
       | b == Eq || b == Ne
       , Just s <- getArg (exprSort "extensionality" e1)
       = mytracepp ("extending NEG = " ++ showpp expr') <$> (extendLHS ann b e1 e2 s >>= goN Neg)
      goN _ e = return e

getArg :: Sort -> Maybe Sort
getArg s = case bkFFunc s of
             Just (_, a:_:_) -> Just a
             _                -> Nothing

extendRHS, extendLHS :: a -> Brel -> Expr -> Expr -> Sort -> Ex a Expr
extendRHS ann b e1 e2 s =
  do es <- generateArguments ann s
     mytracepp "extendRHS = " . pAnd <$> mapM (makeEq b e1 e2) es

extendLHS ann b e1 e2 s =
  do es  <- generateArguments ann s
     dds <- gets exddecl
     is  <- instantiate ann dds s
     mytracepp "extendLHS = " . pAnd . (PAtom b e1 e2:) <$> mapM (makeEq b e1 e2) (es ++ is)

generateArguments :: a -> Sort -> Ex a [Expr]
generateArguments ann srt = do
  ddatadecls <- gets exddecl
  case breakSort ddatadecls srt of
    Left dds -> mapM (freshArgDD ann) dds
    Right s  -> (\x -> [EVar x]) <$> freshArgOne ann s

makeEq :: Brel-> Expr -> Expr -> Expr -> Ex ann Expr
makeEq b e1 e2 e = do
  env <- gets exenv
  let elab = elaborate (dummyLoc "extensionality") env
  return $ PAtom b (elab $ EApp (unElab e1) e)  (elab $ EApp (unElab e2) e)

instantiate :: a -> [DataDecl]  -> Sort -> Ex a [Expr]
instantiate ann ds s = instantiateOne ann (breakSort ds s)

instantiateOne :: a -> Either [(LocSymbol, [Sort])] Sort  -> Ex a [Expr]
instantiateOne ann (Right s@(FVar _)) =
  (\x -> [EVar x]) <$> freshArgOne ann s
instantiateOne _ (Right s) = do
  xss <- gets excbs
  return [EVar x | (x,xs) <- xss, xs == s ]
instantiateOne ann (Left [(dc, ts)]) =
  map (mkEApp dc) . combine <$>  mapM (instantiateOne ann) (Right <$> ts)
instantiateOne _ _ = undefined

combine :: [[a]] -> [[a]]
combine []          = [[]]
combine ([]:_)      = []
combine ((x:xs):ys) = map (x:) (combine ys) ++ combine (xs:ys)


data Pos = Pos | Neg deriving Eq
negatePos :: Pos -> Pos
negatePos Pos = Neg
negatePos Neg = Pos

mapMPosExpr :: (Monad m) => Pos -> (Pos -> Expr -> m Expr) -> Expr -> m Expr
mapMPosExpr pos f = go pos
  where
    go p e@(ESym _)      = f p e
    go p e@(ECon _)      = f p e
    go p e@(EVar _)      = f p e
    go p e@(PKVar _ _)   = f p e
    go p (ENeg e)        = f p . ENeg =<< go p e
    go p (ECst e t)      = f p . (`ECst` t) =<< go p e
    go p (ECoerc a t e)  = f p . ECoerc a t =<< go p e
    go p (EApp g e)      = f p =<< (EApp        <$>  go p g  <*> go p e             )
    go p (EBin o e1 e2)  = f p =<< (EBin o      <$>  go p e1 <*> go p e2            )
    go p (PAtom r e1 e2) = f p =<< (PAtom r     <$>  go p e1 <*> go p e2            )

    go p (PImp p1 p2)    = f p =<< (PImp        <$>  go (negatePos p) p1 <*> go p p2)
    go p (PAnd ps)       = f p . PAnd =<< (go p `traverse` ps)

    -- The below cannot appear due to normalization
    go p (PNot e)        = f p . PNot =<< go p e
    go p (PIff p1 p2)    = f p =<< (PIff        <$>  go p p1 <*> go p p2            )
    go p (EIte e e1 e2)  = f p =<< (EIte        <$>  go p e  <*> go p e1 <*> go p e2)
    go p (POr  ps)       = f p . POr =<< (go p `traverse` ps)

    -- The following canot appear in general
    go p (PAll xts e)    = f p . PAll   xts =<< go p e
    go p (ELam (x,t) e)  = f p . ELam (x,t) =<< go p e
    go p (PExist xts e)  = f p . PExist xts =<< go p e
    go p (ETApp e s)     = f p . (`ETApp` s) =<< go p e
    go p (ETAbs e s)     = f p . (`ETAbs` s) =<< go p e
    go p (PGrad k s i e) = f p . PGrad k s i =<< go p e

normalize :: Expr -> Expr
normalize expr' = mytracepp ("normalize: " ++ showpp expr') $ go expr'
  where
    go e@(ESym _)        = e
    go e@(ECon _)        = e
    go e@(EVar _)        = e
    go e@(PKVar _ _)     = e
    go e@(ENeg _)        = e
    go (PNot e)          = PImp e PFalse
    go e@(ECst _ _)      = e
    go e@ECoerc{}        = e
    go e@(EApp _ _)      = e
    go e@EBin{}          = e
    go (PImp p1 p2)      = PImp (go p1) (go p2)
    go (PIff p1 p2)      = PAnd [PImp p1' p2', PImp p2' p1'] where (p1', p2') = (go p1, go p2)
    go e@PAtom{}         = e
    go (EIte e e1 e2)    = go $ PAnd [PImp e e1, PImp (PNot e) e2]
    go (PAnd ps)         = pAnd (go <$> ps)
    go (POr  ps)         = foldl' (\x y -> PImp (PImp (go x) PFalse) y) PFalse ps
    go e@(PAll _ _)      = e -- Cannot appear
    go e@(ELam _ _)      = e -- Cannot appear
    go e@(PExist _ _)    = e -- Cannot appear
    go e@(ETApp _ _)     = e -- Cannot appear
    go e@(ETAbs _ _)     = e -- Cannot appear
    go e@PGrad{}         = e -- Cannot appear


type Ex a = State (ExSt a)
data ExSt a = ExSt
  { unique  :: Int
  , exddecl :: [DataDecl]
  , exenv   :: SymEnv        -- used for elaboration
  , exbenv  :: BindEnv a
  , exbinds :: IBindEnv
  , excbs   :: [(Symbol, Sort)]
  }

initST :: SymEnv -> [DataDecl]  -> ExSt ann
initST env dd = ExSt 0 (d:dd) env mempty mempty mempty
  where
    -- NV: hardcore Haskell pairs because they do not appear in DataDecl (why?)
    d = mytracepp "Tuple DataDecl" $ DDecl (symbolFTycon (dummyLoc tupConName)) 2 [ct]
#if MIN_TOOL_VERSION_ghc(9,6,0)
    ct = DCtor (dummyLoc (symbol "GHC.Tuple.Prim.(,)")) [
            DField (dummyLoc (symbol "lqdc$select$GHC.Tuple.Prim.(,)$1")) (FVar 0)
          , DField (dummyLoc (symbol "lqdc$select$GHC.Tuple.Prim.(,)$2")) (FVar 1)
          ]
#else
    ct = DCtor (dummyLoc (symbol "GHC.Tuple.(,)")) [
            DField (dummyLoc (symbol "lqdc$select$GHC.Tuple.(,)$1")) (FVar 0)
          , DField (dummyLoc (symbol "lqdc$select$GHC.Tuple.(,)$2")) (FVar 1)
          ]
#endif

setBEnv :: BindEnv a -> Ex a ()
setBEnv benv = modify (\st -> st{exbenv = benv})

setExBinds :: IBindEnv -> Ex a ()
setExBinds bids = modify (\st -> st{ exbinds = bids
                                   , excbs   = [ (x, sr_sort r) | (i, (x, r, _)) <- bindEnvToList (exbenv st)
                                                                , memberIBindEnv i bids]})


freshArgDD :: a -> (LocSymbol, [Sort]) -> Ex a Expr
freshArgDD ann (dc, sorts) = do
  xs <- mapM (freshArgOne ann) sorts
  return $ mkEApp dc (EVar <$> xs)


freshArgOne :: ann -> Sort -> Ex ann Symbol
freshArgOne ann s = do
  exst <- get
  let x = symbol ("ext$" ++ show (unique exst))
  let (bindId, benv') = insertBindEnv x (trueSortedReft s) ann (exbenv exst)
  modify (\st -> st{ exenv   = insertSymEnv x s (exenv st)
                   , exbenv  = benv'
                   , exbinds = insertsIBindEnv [bindId] (exbinds st)
                   , unique   = 1 + unique st
                   , excbs = (x,s) : excbs st
                   })
  return x


breakSort :: [DataDecl] -> Sort -> Either [(LocSymbol, [Sort])] Sort
breakSort ddatadecls s
    | Just (tc, ts) <- splitTC s
    , [(dds,i)] <- [ (ddCtors dd,ddVars dd) | dd <- ddatadecls, ddTyCon dd == tc ]
    = Left ((\dd -> (dcName dd, instSort  (Sub $ zip [0..(i-1)] ts) . dfSort <$> dcFields dd)) <$> dds)
    | otherwise
    = Right s

instSort :: Sub -> Sort -> Sort
instSort (Sub su) x = mapSort go x
  where
    go :: Sort -> Sort
    go (FVar i) = fromMaybe (FVar i) $ lookup i su
    go s        = s

splitTC :: Sort -> Maybe (FTycon, [Sort])
splitTC s
     | (FTC f, ts) <- splitFApp s
     = Just (f, ts)
     | otherwise
     = Nothing

splitFApp :: Sort -> (Sort, [Sort])
splitFApp = go []
    where go acc (FApp s1 s2) = go (s2:acc) s1
          go acc s            = (s, acc)
