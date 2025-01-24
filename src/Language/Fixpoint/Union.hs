module Language.Fixpoint.Union where 
import Data.HashMap.Strict (lookup, insert, HashMap, empty)
import Prelude hiding (lookup)
import Language.Fixpoint.Types.Sorts (Sort(..))
next :: Sort -> Maybe Int 
next (FVar i) = Just i
next _ = Nothing 
unionVals :: UF -> Int -> Sort -> Sort -> UF
unionVals _ _ _ _ = error "todo"
-- unionVals ufM i s1 s2 = error "todo"
-- unionVals ufM _ FInt FInt = ufM
-- unionVals ufM _ FReal FReal = ufM
-- unionVals ufM _ FInt FReal = ufM
-- unionVals ufM _ FReal FInt = ufM
newtype UF = MkUF (HashMap Int Sort) deriving (Show)
new :: UF
new = MkUF empty
union :: UF -> Int -> Sort -> UF
union u@(MkUF ufM) tyv s =
    -- find the root for tyv 
    let tyv_root = find (MkUF ufM) tyv in
    case tyv_root of
            -- if tyv not in union find, insert
            Nothing -> MkUF (insert tyv s ufM)
            -- otherwise, unify the current sort with 
            -- the new one and insert that
            Just (i, s') -> unionVals u i s s'
find  :: UF -> Int -> Maybe (Int, Sort)
find (MkUF ufM) k = do
    s <- lookup k ufM
    case next s of 
        Nothing -> Just (k, s)
        Just i -> find (MkUF ufM) i