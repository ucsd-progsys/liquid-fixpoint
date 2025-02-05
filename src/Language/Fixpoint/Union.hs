{-# LANGUAGE BangPatterns #-}
module Language.Fixpoint.Union where
import Data.HashMap.Strict (lookup, insert, HashMap, empty)
import Prelude hiding (lookup)
import Language.Fixpoint.Types (Sort(..))

unionSub :: UF -> Sort -> Sort -> UF
unionSub uf s1 s2 = 
    -- get the parents and call unionVals 
    let s1_root = getRep uf s1
        s2_root = getRep uf s2 
    in 
        unionVals uf s1_root s2_root

--------------------------------------------------------------------------------
-- | union for sorts in union find
--------------------------------------------------------------------------------
unionVals :: UF -> Sort -> Sort -> UF
--------------------------------------------------------------------------------
unionVals uf s1 s2
  | isNumericSort s1 && isNumericSort s2 = uf
  where
    isNumericSort FReal = True
    isNumericSort FNum  = True
    isNumericSort FFrac = True
    isNumericSort FInt = True
    isNumericSort _     = False

unionVals uf (FObj x) (FObj y)
    | x == y = uf
-- unionVals u@(MkUF uf) (FVar i) (FVar j) = if i == j then u else MkUF (insert i (FVar j) uf)
unionVals (MkUF uf) (FVar i) s = MkUF (insert i s uf)
unionVals (MkUF uf) s (FVar i) = MkUF (insert i s uf)
unionVals uf (FFunc s1 s2) (FFunc s1' s2') = let uf' = unionSub uf s1 s1' in unionSub uf' s2 s2'
unionVals uf (FApp s1 s2) (FApp s1' s2') = let uf' = unionSub uf s1 s1' in unionSub uf' s2 s2'
unionVals uf (FAbs _ s) (FAbs _ s') = unionSub uf s s'
unionVals uf (FTC s1) (FTC s2) | s1 == s2 = uf
unionVals _ s1 s2 = error ("Cannot unify " ++ show s1 ++ " and " ++ show s2)


newtype UF = MkUF (HashMap Int Sort) deriving (Show)
new :: UF
new = MkUF empty

union :: UF -> Int -> Sort -> UF
union !u !tyv !s =
    let tyv_root =  find u tyv 
        sort_root = getRep u s 
    in
    if tyv_root == sort_root then u else unionVals u tyv_root sort_root

getRep :: UF -> Sort -> Sort
getRep u s =
    case s of
        FVar i -> find u i
        _ -> s

find  :: UF -> Int -> Sort
find (MkUF ufM) = f
    where
    f k = do
        case lookup k ufM of 
            Nothing -> FVar k
            Just (FVar i) -> f i 
            Just s -> s