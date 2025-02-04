{-# LANGUAGE BangPatterns #-}
module Language.Fixpoint.Union where 
import Data.HashMap.Strict (lookup, insert, HashMap, empty)
import Prelude hiding (lookup)
import Language.Fixpoint.Types.Sorts (Sort(..))
import GHC.IO (unsafePerformIO)

unionSub :: UF -> Int -> Sort -> Sort -> UF
unionSub uf i s1 s2 = case (s1, s2) of 
    (FVar i1, FVar i2) -> if i1 == i2 then uf else union uf i1 s2
    (FVar i1, _) -> union uf i1 s2
    (_, FVar i2) -> union uf i2 s1
    (_, _) -> unionVals uf i s1 s2

--------------------------------------------------------------------------------
-- | union for sorts in union find
--------------------------------------------------------------------------------
unionVals :: UF -> Int -> Sort -> Sort -> UF
--------------------------------------------------------------------------------
unionVals uf _ s1 s2 
  | isNumericSort s1 && isNumericSort s2 = uf
  where
    isNumericSort FReal = True
    isNumericSort FNum  = True
    isNumericSort FFrac = True
    isNumericSort FInt = True
    isNumericSort _     = False

unionVals uf _ (FObj x) (FObj y)
    | x == y = uf
unionVals (MkUF uf) _ (FVar i) (FVar j) = if i == j then MkUF uf else MkUF (insert i (FVar j) uf)
unionVals (MkUF uf) _ (FVar i) s = MkUF (insert i s uf)
unionVals (MkUF uf) _ s (FVar i) = MkUF (insert i s uf)
unionVals uf i (FFunc s1 s2) (FFunc s1' s2') = 
    let uf' = unionSub uf i s1 s1' in 
        unionSub uf' i s2 s2'
unionVals uf i (FApp s1 s2) (FApp s1' s2') = 
    let uf' = unionSub uf i s1 s1' in 
        unionSub uf' i s2 s2'
unionVals uf i (FAbs _ s) (FAbs _ s') = unionSub uf i s s'
unionVals uf _ (FTC s1) (FTC s2)
    | s1 == s2 = uf
unionVals _ _ s1 s2 = error ("Cannot unify " ++ show s1 ++ " and " ++ show s2)
    

newtype UF = MkUF (HashMap Int Sort) deriving (Show)
new :: UF
new = MkUF empty


unionSafe :: UF -> Int -> Sort -> UF
unionSafe u@(MkUF ufM) tyv s = 
    -- find the root for tyv 
    let tyv_root = findWithIndex (MkUF ufM) tyv in
    case tyv_root of
            -- if tyv not in union find, insert
            Nothing -> MkUF (insert tyv s ufM)
            -- otherwise, unify the current sort with 
            -- the new one and insert that
            Just (i, s') ->
                let !_ = unsafePerformIO $ print ("Here with " ++ show s' ++ " and " ++ show s) in
                    unionVals u i s s'


union :: UF -> Int -> Sort -> UF
union u tyv s =
    case s of 
        FVar i -> case find u i of 
                Just (FVar j) -> if tyv == j then u else unionSafe u tyv s
                _ -> unionSafe u tyv s
        _ -> unionSafe u tyv s

findWithIndex :: UF -> Int -> Maybe (Int, Sort)
findWithIndex u@(MkUF ufM) k = do 
    s <- lookup k ufM 
    case s of 
        FVar i -> findWithIndex u i
        s' -> Just (k, s')

find  :: UF -> Int -> Maybe Sort
find (MkUF ufM) = f 
    where 
    f k = do 
        s <- lookup k ufM 
        case s of 
            FVar i -> case f i of
                Nothing -> Just (FVar i)
                Just s' -> Just s'
            s' -> Just s'