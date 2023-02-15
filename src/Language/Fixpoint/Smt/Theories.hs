{-# LANGUAGE CPP                       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE PatternGuards             #-}
{-# LANGUAGE ViewPatterns              #-}

{-# OPTIONS_GHC -Wno-orphans           #-}
{-# OPTIONS_GHC -Wno-name-shadowing    #-}

module Language.Fixpoint.Smt.Theories
     (
       -- * Convert theory applications TODO: merge with smt2symbol
       smt2App
       -- * Convert theory sorts
     , sortSmtSort

       -- * Convert theory symbols
     , smt2Symbol

       -- * Preamble to initialize SMT
     , preamble

       -- * Bit Vector Operations
     , sizeBv
       -- , toInt

       -- * Theory Symbols
     , theorySymbols
     , dataDeclSymbols


       -- * Theories
     , setEmpty, setEmp, setCap, setSub, setAdd, setMem
     , setCom, setCup, setDif, setSng, mapSel, mapCup, mapSto, mapDef

      -- * Query Theories
     , isSmt2App
     , axiomLiterals
     , maxLamArg
     ) where

import           Prelude hiding (map)
import           Language.Fixpoint.Types.Sorts
import           Language.Fixpoint.Types.Config
import           Language.Fixpoint.Types
import           Language.Fixpoint.Smt.Types
-- import qualified Data.HashMap.Strict      as M
import           Data.Maybe (catMaybes)
import qualified Data.Text.Lazy           as T
-- import           Data.Text.Format
import qualified Data.Text
import           Data.String                 (IsString(..))
import Language.Fixpoint.Utils.Builder

{- | [NOTE:Adding-Theories] To add new (SMTLIB supported) theories to
     liquid-fixpoint and upstream, grep for "Map_default" and then add
     your corresponding symbol in all those places.
     This is currently far more complicated than it needs to be.
 -}

--------------------------------------------------------------------------------
-- | Theory Symbols ------------------------------------------------------------
--------------------------------------------------------------------------------

-- "set" is currently \"LSet\" instead of just \"Set\" because Z3 has its own
-- \"Set\" since 4.8.5
elt, set, map :: Raw
elt  = "Elt"
set  = "LSet"
map  = "Map"

emp, sng, add, cup, cap, mem, dif, sub, com, sel, sto, mcup, mdef, mprj :: Raw
mToSet, mshift, mmax, mmin :: Raw
emp   = "smt_set_emp"
sng   = "smt_set_sng"
add   = "smt_set_add"
cup   = "smt_set_cup"
cap   = "smt_set_cap"
mem   = "smt_set_mem"
dif   = "smt_set_dif"
sub   = "smt_set_sub"
com   = "smt_set_com"
sel   = "smt_map_sel"
sto   = "smt_map_sto"
mcup  = "smt_map_cup"
mmax  = "smt_map_max"
mmin  = "smt_map_min"
mdef  = "smt_map_def"
mprj  = "smt_map_prj"
mshift = "smt_map_shift"
mToSet = "smt_map_to_set"

---- Size changes
bvConcatName, bvExtractName, bvRepeatName, bvZeroExtName, bvSignExtName :: Symbol
bvConcatName   = "concat"
bvExtractName  = "extract"
bvRepeatName   = "repeat"
bvZeroExtName  = "zero_extend"
bvSignExtName  = "sign_extend"

-- Unary Logic
bvNotName, bvNegName :: Symbol
bvNotName = "bvnot"
bvNegName = "bvneg"

-- Binary Logic
bvAndName, bvNandName, bvOrName, bvNorName, bvXorName, bvXnorName :: Symbol
bvAndName  = "bvand"
bvNandName = "bvnand"
bvOrName   = "bvor"
bvNorName  = "bvnor"
bvXorName  = "bvxor"
bvXnorName = "bvxnor"

-- Shifts
bvShlName, bvLShrName, bvAShrName, bvLRotName, bvRRotName :: Symbol
bvShlName  = "bvshl"
bvLShrName = "bvlshr"
bvAShrName = "bvashr"
bvLRotName = "rotate_left"
bvRRotName = "rotate_right"

-- Arithmetic
bvAddName, bvSubName, bvMulName, bvUDivName :: Symbol
bvURemName, bvSDivName, bvSRemName, bvSModName :: Symbol
bvAddName  = "bvadd"
bvSubName  = "bvsub"
bvMulName  = "bvmul"
bvUDivName = "bvudiv"
bvURemName = "bvurem"
bvSDivName = "bvsdiv"
bvSRemName = "bvsrem"
bvSModName = "bvsmod"

-- Comparisons
bvCompName, bvULtName, bvULeName, bvUGtName, bvUGeName :: Symbol
bvSLtName, bvSLeName, bvSGtName, bvSGeName :: Symbol
bvCompName = "bvcomp"
bvULtName  = "bvult"
bvULeName  = "bvule"
bvUGtName  = "bvugt"
bvUGeName  = "bvuge"
bvSLtName  = "bvslt"
bvSLeName  = "bvsle"
bvSGtName  = "bvsgt"
bvSGeName  = "bvsge"

setEmpty, setEmp, setCap, setSub, setAdd, setMem, setCom, setCup :: Symbol
setDif, setSng :: Symbol
setEmpty = "Set_empty"
setEmp   = "Set_emp"
setCap   = "Set_cap"
setSub   = "Set_sub"
setAdd   = "Set_add"
setMem   = "Set_mem"
setCom   = "Set_com"
setCup   = "Set_cup"
setDif   = "Set_dif"
setSng   = "Set_sng"

mapSel, mapSto, mapCup, mapDef, mapPrj, mapToSet :: Symbol
mapMax, mapMin, mapShift :: Symbol
mapSel   = "Map_select"
mapSto   = "Map_store"
mapCup   = "Map_union"
mapMax   = "Map_union_max"
mapMin   = "Map_union_min"
mapDef   = "Map_default"
mapPrj   = "Map_project"
mapShift = "Map_shift" -- See [Map key shift]
mapToSet = "Map_to_set"

-- [Interaction between Map and Set]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Function mapToSet: Convert a map to a set. The map's key may be of
-- any type and is preserved as the set's element type. More precisely:
--   mapToSet : Map k Int -> Set k
-- The element type must be Int. All non-positive elements are mapped
-- to False, and all positive elements are mapped to True. In practice,
-- negative elements should not exist because Map is intended to be used
-- as a bag, so the element is a non-negative number representing
-- the occurrences of its corresponding key.
--
-- Function mapPrj: Project a subset of a map. Type signature:
--   mapPrj : Set k -> Map k Int -> Map k Int
-- If the key is present in both the argument set and the argument map,
-- then the key (along with its associated value in the map) are preserved
-- in the output. Keys not present in the set are mapped to zero. Keys not
-- present in the set are mapped to zero.
--
-- [Map key shift]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Function mapShift: Add an integer to all keys in a map. Type signature:
--   mapShift : Int -> Map Int v -> Map Int v
-- Let's call the first argument (the shift amount) N, the second argument K1,
-- and the result K2. For all indices i, we have K2[i] = K1[i - N].
-- This is implemented with Z3's lambda, which lets us construct an array
-- from a function.
--
-- [Map max and min]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Functions mapMax and mapMin: Union two maps, combining the elements by
-- taking either the greatest (mapMax) or the least (mapMin) of them.
--   mapMax, mapMin : Map v Int -> Map v Int -> Map v Int

strLen, strSubstr, strConcat :: (IsString a) => a -- Symbol
strLen    = "strLen"
strSubstr = "subString"
strConcat = "concatString"

z3strlen, z3strsubstr, z3strconcat :: Raw
z3strlen    = "str.len"
z3strsubstr = "str.substr"
z3strconcat = "str.++"

strLenSort, substrSort, concatstrSort :: Sort
strLenSort    = FFunc strSort intSort
substrSort    = mkFFunc 0 [strSort, intSort, intSort, strSort]
concatstrSort = mkFFunc 0 [strSort, strSort, strSort]

string :: Raw
string = strConName

bFun :: Raw -> [(Builder, Builder)] -> Builder -> Builder -> T.Text
bFun name xts out body = blt $ key "define-fun" (seqs [bb name, args, out, body])
  where
    args = parenSeqs [parens (x <+> t) | (x, t) <- xts]

bFun' :: Raw -> [Builder] -> Builder -> T.Text
bFun' name ts out = blt $ key "declare-fun" (seqs [bb name, args, out])
  where
    args = parenSeqs ts

bSort :: Raw -> Builder -> T.Text
bSort name def = blt $ key "define-sort" (bb name <+> "()" <+> def)

z3Preamble :: Config -> [T.Text]
z3Preamble u
  = stringPreamble u ++
    [ bSort elt
        "Int"
    , bSort set
        (key2 "Array" (bb elt) "Bool")
    , bFun emp
        []
        (bb set)
        (parens (key "as const" (bb set) <+> "false"))
    , bFun sng
        [("x", bb elt)]
        (bb set)
        (key3 "store" (parens (key "as const" (bb set) <+> "false")) "x" "true")
    , bFun mem
        [("x", bb elt), ("s", bb set)]
        "Bool"
        "(select s x)"
    , bFun add
        [("s", bb set), ("x", bb elt)]
        (bb set)
        "(store s x true)"
    , bFun cup
        [("s1", bb set), ("s2", bb set)]
        (bb set)
        "((_ map or) s1 s2)"
    , bFun cap
        [("s1", bb set), ("s2", bb set)]
        (bb set)
        "((_ map and) s1 s2)"
    , bFun com
        [("s", bb set)]
        (bb set)
        "((_ map not) s)"
    , bFun dif
        [("s1", bb set), ("s2", bb set)]
        (bb set)
        (key2 (bb cap) "s1" (key (bb com) "s2"))
    , bFun sub
        [("s1", bb set), ("s2", bb set)]
        "Bool"
        (key2 "=" (bb emp) (key2 (bb dif) "s1" "s2"))

    -- Maps
    , bSort map
        (key2 "Array" (bb elt) (bb elt))
    , bFun sel
        [("m", bb map), ("k", bb elt)]
        (bb elt)
        "(select m k)"
    , bFun sto
        [("m", bb map), ("k", bb elt), ("v", bb elt)]
        (bb map)
        "(store m k v)"
    , bFun mcup
        [("m1", bb map), ("m2", bb map)]
        (bb map)
        (key2 (key "_ map" (key2 "+" (parens (bb elt <+> bb elt)) (bb elt))) "m1" "m2")
    , bFun mprj -- See [Interaction Between Map and Set]
        [("s", bb set), ("m", bb map)]
        (bb map)
        (key3
          (key "_ map"
            (key2 "ite"
              (parens ("Bool" <+> bb elt <+> bb elt))
              (bb elt)
            )
          )
          "s"
          "m"
          (parens (key "as const" (key2 "Array" (bb elt) (bb elt)) <+> "0"))
        )
    , bFun mToSet -- See [Interaction Between Map and Set]
        [("m", bb map)]
        (bb set)
        (key2
          (key "_ map"
            (key2 ">"
              (parens (bb elt <+> bb elt))
              "Bool"
            )
          )
          "m"
          (parens (key "as const" (key2 "Array" (bb elt) (bb elt)) <+> "0"))
        )
    , bFun mmax -- See [Map max and min]
        [("m1", bb map),("m2", bb map)]
        (bb map)
        "(lambda ((i Int)) (ite (> (select m1 i) (select m2 i)) (select m1 i) (select m2 i)))"
    , bFun mmin -- See [Map max and min]
        [("m1", bb map),("m2", bb map)]
        (bb map)
        "(lambda ((i Int)) (ite (< (select m1 i) (select m2 i)) (select m1 i) (select m2 i)))"
    , bFun mshift -- See [Map key shift]
        [("n", "Int"),("m", bb map)]
        (bb map)
        "(lambda ((i Int)) (select m (- i n)))"
    , bFun mdef
        [("v", bb elt)]
        (bb map)
        (key (key "as const" (parens (bb map))) "v")
    , bFun boolToIntName
        [("b", "Bool")]
        "Int"
        "(ite b 1 0)"

    , uifDef u (symbolLText mulFuncName) "*"
    , uifDef u (symbolLText divFuncName) "div"
    ]

symbolLText :: Symbol -> T.Text
symbolLText = T.fromStrict . symbolText

-- RJ: Am changing this to `Int` not `Real` as (1) we usually want `Int` and
-- (2) have very different semantics. TODO: proper overloading, post genEApp
uifDef :: Config -> T.Text -> T.Text -> T.Text
uifDef cfg f op
  | linear cfg || Z3 /= solver cfg
  = bFun' f ["Int", "Int"] "Int"
  | otherwise
  = bFun f [("x", "Int"), ("y", "Int")] "Int" (key2 (bb op) "x" "y")

cvc4Preamble :: Config -> [T.Text]
cvc4Preamble z
  = [        "(set-logic ALL_SUPPORTED)"]
  ++ commonPreamble z
  ++ cvc4MapPreamble z

commonPreamble :: Config -> [T.Text]
commonPreamble _ --TODO use uif flag u (see z3Preamble)
  = [ bSort elt    "Int"
    , bSort set    "Int"
    , bSort string "Int"
    , bFun' emp []               (bb set)
    , bFun' sng [bb elt]         (bb set)
    , bFun' add [bb set, bb elt] (bb set)
    , bFun' cup [bb set, bb set] (bb set)
    , bFun' cap [bb set, bb set] (bb set)
    , bFun' dif [bb set, bb set] (bb set)
    , bFun' sub [bb set, bb set] "Bool"
    , bFun' mem [bb elt, bb set] "Bool"
    , bFun boolToIntName [("b", "Bool")] "Int" "(ite b 1 0)"
    ]

cvc4MapPreamble :: Config -> [T.Text]
cvc4MapPreamble _ =
    [ bSort map    (key2 "Array" (bb elt) (bb elt))
    , bFun sel [("m", bb map), ("k", bb elt)]                (bb elt) "(select m k)"
    , bFun sto [("m", bb map), ("k", bb elt), ("v", bb elt)] (bb map) "(store m k v)"
    ]

smtlibPreamble :: Config -> [T.Text]
smtlibPreamble z --TODO use uif flag u (see z3Preamble)
  = commonPreamble z
 ++ [ bSort map "Int"
    , bFun' sel [bb map, bb elt] (bb elt)
    , bFun' sto [bb map, bb elt, bb elt] (bb map)
    ]

stringPreamble :: Config -> [T.Text]
stringPreamble cfg | stringTheory cfg
  = [ bSort string "String"
    , bFun strLen [("s", bb string)] "Int" (key (bb z3strlen) "s")
    , bFun strSubstr [("s", bb string), ("i", "Int"), ("j", "Int")] (bb string) (key (bb z3strsubstr) "s i j")
    , bFun strConcat [("x", bb string), ("y", bb string)] (bb string) (key (bb z3strconcat) "x y")
    ]

stringPreamble _
  = [ bSort string "Int"
    , bFun' strLen [bb string] "Int"
    , bFun' strSubstr [bb string, "Int", "Int"] (bb string)
    , bFun' strConcat [bb string, bb string] (bb string)
    ]

--------------------------------------------------------------------------------
-- | Exported API --------------------------------------------------------------
--------------------------------------------------------------------------------
smt2Symbol :: SymEnv -> Symbol -> Maybe Builder
smt2Symbol env x = fromLazyText . tsRaw <$> symEnvTheory x env

instance SMTLIB2 SmtSort where
  smt2 _ = smt2SmtSort

smt2SmtSort :: SmtSort -> Builder
smt2SmtSort SInt         = "Int"
smt2SmtSort SReal        = "Real"
smt2SmtSort SBool        = "Bool"
smt2SmtSort SString      = bb string
smt2SmtSort SSet         = bb set
smt2SmtSort SMap         = bb map
smt2SmtSort (SBitVec n)  = key "_ BitVec" (bShow n)
smt2SmtSort (SVar n)     = "T" <> bShow n
smt2SmtSort (SData c []) = symbolBuilder c
smt2SmtSort (SData c ts) = parenSeqs [symbolBuilder c, smt2SmtSorts ts]

-- smt2SmtSort (SApp ts)    = build "({} {})" (symbolBuilder tyAppName, smt2SmtSorts ts)

smt2SmtSorts :: [SmtSort] -> Builder
smt2SmtSorts = seqs . fmap smt2SmtSort

type VarAs = SymEnv -> Symbol -> Sort -> Builder
--------------------------------------------------------------------------------
smt2App :: VarAs -> SymEnv -> Expr -> [Builder] -> Maybe Builder
--------------------------------------------------------------------------------
smt2App _ _ (dropECst -> EVar f) [d]
  | f == setEmpty = Just (bb emp)
  | f == setEmp   = Just (key2 "=" (bb emp) d)
  | f == setSng   = Just (key (bb sng) d) -- Just (key2 (bb add) (bb emp) d)

smt2App k env f (d:ds)
  | Just fb <- smt2AppArg k env f
  = Just $ key fb (d <> mconcat [ " " <> d | d <- ds])

smt2App _ _ _ _    = Nothing

smt2AppArg :: VarAs -> SymEnv -> Expr -> Maybe Builder
smt2AppArg k env (ECst (dropECst -> EVar f) t)
  | Just fThy <- symEnvTheory f env
  = Just $ if isPolyCtor fThy t
            then k env f (ffuncOut t)
            else bb (tsRaw fThy)

smt2AppArg _ _ _
  = Nothing

isPolyCtor :: TheorySymbol -> Sort -> Bool
isPolyCtor fThy t = isPolyInst (tsSort fThy) t && tsInterp fThy == Ctor

ffuncOut :: Sort -> Sort
ffuncOut t = maybe t (last . snd) (bkFFunc t)

--------------------------------------------------------------------------------
isSmt2App :: SEnv TheorySymbol -> Expr -> Maybe Int
--------------------------------------------------------------------------------
isSmt2App g  (dropECst -> EVar f)
  | f == setEmpty = Just 1
  | f == setEmp   = Just 1
  | f == setSng   = Just 1
  | otherwise     = lookupSEnv f g >>= thyAppInfo
isSmt2App _ _     = Nothing

thyAppInfo :: TheorySymbol -> Maybe Int
thyAppInfo ti = case tsInterp ti of
  Field    -> Just 1
  _        -> sortAppInfo (tsSort ti)

sortAppInfo :: Sort -> Maybe Int
sortAppInfo t = case bkFFunc t of
  Just (_, ts) -> Just (length ts - 1)
  Nothing      -> Nothing

preamble :: Config -> SMTSolver -> [T.Text]
preamble u Z3   = z3Preamble u
preamble u Cvc4 = cvc4Preamble u
preamble u _    = smtlibPreamble u

--------------------------------------------------------------------------------
-- | Theory Symbols : `uninterpSEnv` should be disjoint from see `interpSEnv`
--   to avoid duplicate SMT definitions.  `uninterpSEnv` is for uninterpreted
--   symbols, and `interpSEnv` is for interpreted symbols.
--------------------------------------------------------------------------------

-- | `theorySymbols` contains the list of ALL SMT symbols with interpretations,
--   i.e. which are given via `define-fun` (as opposed to `declare-fun`)
theorySymbols :: [DataDecl] -> SEnv TheorySymbol -- M.HashMap Symbol TheorySymbol
theorySymbols ds = fromListSEnv $  -- SHIFTLAM uninterpSymbols
                                  interpSymbols
                               ++ concatMap dataDeclSymbols ds


--------------------------------------------------------------------------------
interpSymbols :: [(Symbol, TheorySymbol)]
--------------------------------------------------------------------------------
interpSymbols =
  [ interpSym setEmp   emp  (FAbs 0 $ FFunc (setSort $ FVar 0) boolSort)
  , interpSym setEmpty emp  (FAbs 0 $ FFunc intSort (setSort $ FVar 0))
  , interpSym setSng   sng  (FAbs 0 $ FFunc (FVar 0) (setSort $ FVar 0))
  , interpSym setAdd   add   setAddSort
  , interpSym setCup   cup   setBopSort
  , interpSym setCap   cap   setBopSort
  , interpSym setMem   mem   setMemSort
  , interpSym setDif   dif   setBopSort
  , interpSym setSub   sub   setCmpSort
  , interpSym setCom   com   setCmpSort

  , interpSym mapSel   sel   mapSelSort
  , interpSym mapSto   sto   mapStoSort
  , interpSym mapCup   mcup  mapCupSort
  , interpSym mapMax   mmax  mapMaxSort
  , interpSym mapMin   mmin  mapMinSort
  , interpSym mapDef   mdef  mapDefSort
  , interpSym mapPrj   mprj  mapPrjSort
  , interpSym mapShift mshift mapShiftSort
  , interpSym mapToSet mToSet mapToSetSort

  , interpSym strLen    strLen    strLenSort
  , interpSym strSubstr strSubstr substrSort
  , interpSym strConcat strConcat concatstrSort
  , interpSym boolInt   boolInt   (FFunc boolSort intSort)

  -- Function mappings for indexed identifier functions
  , interpSym' "_" iiSort
  , interpSym "app" "" appSort

  , interpSym' bvConcatName bvConcatSort
  , interpSym' bvExtractName $ FFunc FInt $ bvExtendSort
  , interpBvExt bvRepeatName
  , interpBvExt bvZeroExtName
  , interpBvExt bvSignExtName

  , interpBvUop bvNotName
  , interpBvUop bvNegName

  , interpBvBop bvAndName 
  , interpBvBop bvNandName
  , interpBvBop bvOrName
  , interpBvBop bvNorName
  , interpBvBop bvXorName
  , interpBvBop bvXnorName

  , interpBvBop bvShlName
  , interpBvBop bvLShrName
  , interpBvBop bvAShrName
  , interpBvRot bvLRotName
  , interpBvRot bvRRotName

  , interpBvBop bvAddName
  , interpBvBop bvSubName
  , interpBvBop bvMulName
  , interpBvBop bvUDivName
  , interpBvBop bvURemName
  , interpBvBop bvSDivName
  , interpBvBop bvSRemName
  , interpBvBop bvSModName

  , interpSym' bvCompName bvEqSort
  , interpBvCmp bvULtName
  , interpBvCmp bvULeName
  , interpBvCmp bvUGtName
  , interpBvCmp bvUGeName
  , interpBvCmp bvSLtName
  , interpBvCmp bvSLeName
  , interpBvCmp bvSGtName
  , interpBvCmp bvSGeName
  ]
  where
    boolInt    = boolToIntName
    setAddSort = FAbs 0 $ FFunc (setSort $ FVar 0) $ FFunc (FVar 0)           (setSort $ FVar 0)
    setBopSort = FAbs 0 $ FFunc (setSort $ FVar 0) $ FFunc (setSort $ FVar 0) (setSort $ FVar 0)
    setMemSort = FAbs 0 $ FFunc (FVar 0) $ FFunc (setSort $ FVar 0) boolSort
    setCmpSort = FAbs 0 $ FFunc (setSort $ FVar 0) $ FFunc (setSort $ FVar 0) boolSort
    -- select :: forall i a. Map i a -> i -> a
    mapSelSort = FAbs 0 $ FAbs 1 $ FFunc (mapSort (FVar 0) (FVar 1))
                                 $ FFunc (FVar 0) (FVar 1)
    -- cup :: forall i. Map i Int -> Map i Int -> Map i Int
    mapCupSort = FAbs 0          $ FFunc (mapSort (FVar 0) intSort)
                                 $ FFunc (mapSort (FVar 0) intSort)
                                         (mapSort (FVar 0) intSort)
    mapMaxSort = mapCupSort
    mapMinSort = mapCupSort
    mapPrjSort = FAbs 0          $ FFunc (setSort (FVar 0))
                                 $ FFunc (mapSort (FVar 0) intSort)
                                         (mapSort (FVar 0) intSort)
    mapShiftSort = FAbs 0        $ FFunc intSort
                                 $ FFunc (mapSort intSort (FVar 0))
                                         (mapSort intSort (FVar 0))
    mapToSetSort = FAbs 0        $ FFunc (mapSort (FVar 0) intSort) (setSort (FVar 0))
    -- store :: forall i a. Map i a -> i -> a -> Map i a
    mapStoSort = FAbs 0 $ FAbs 1 $ FFunc (mapSort (FVar 0) (FVar 1))
                                 $ FFunc (FVar 0)
                                 $ FFunc (FVar 1)
                                         (mapSort (FVar 0) (FVar 1))
    mapDefSort = FAbs 0 $ FAbs 1 $ FFunc (FVar 1)
                                         (mapSort (FVar 0) (FVar 1))

    interpBvUop name = interpSym' name bvUopSort
    interpBvBop name = interpSym' name bvBopSort
    interpBvCmp name = interpSym' name bvCmpSort
    interpBvExt name = interpSym' name bvExtendSort
    interpBvRot name = interpSym' name bvRotSort

    interpSym' name = interpSym name (T.pack $ symbolString name)

    -- Indexed Identifier sort.
    -- Together with 'app', this allows one to write indexed identifier
    -- functions (smtlib2 specific functions). (e.g. ((_ sign_extend 1) bv))
    --
    -- The idea here is that 'app' is elaborated to the empty string,
    -- and '_' does the typelit application as it does in smtlib2.
    --
    -- Then if we write, (app (_ sign_extend 1) bv), LF will elaborate
    -- it as ( (_ sign_extend 1) bv). Fitting the smtlib2 format exactly!
    --
    -- One thing to note, is that any indexed identifier function (like
    -- sign_extend) has to have no FAbs in it. Otherwise, they will be
    -- elaborated like e.g. ( (_ (as sign_extend Int) 1) bv), which is wrong!
    --
    -- _ :: forall a b c. (a -> b -> c) -> a -> (b -> c)
    iiSort = FAbs 0 $ FAbs 1 $ FAbs 2 $ FFunc
               (FFunc (FVar 0) $ FFunc (FVar 1) (FVar 2))
               (FFunc (FVar 0) $ FFunc (FVar 1) (FVar 2))

    -- Simple application, used for indexed identifier function, check '_'.
    --
    -- app :: forall a b. (a -> b) -> a -> b
    appSort = FAbs 0 $ FAbs 1 $ FFunc
                (FFunc (FVar 0) (FVar 1))
                (FFunc (FVar 0) (FVar 1))

    -- Indexed identifier operation, purposely didn't place FAbs!
    --
    -- extend :: Int -> BitVec a -> BitVec b
    bvExtendSort  = FFunc FInt $ FFunc (bitVecSort 1) (bitVecSort 2)

    -- Indexed identifier operation, purposely didn't place FAbs!
    --
    -- rot :: Int -> BitVec a -> BitVec a
    bvRotSort  = FFunc FInt $ FFunc (bitVecSort 0) (bitVecSort 0)

    -- uOp :: forall a. BitVec a -> BitVec a
    bvUopSort = FAbs 0 $ FFunc (bitVecSort 0) (bitVecSort 0)

    -- bOp :: forall a. BitVec a -> BitVec a -> BitVec a
    bvBopSort = FAbs 0 $ FFunc (bitVecSort 0) $ FFunc (bitVecSort 0) (bitVecSort 0)

    -- cmp :: forall a. BitVec a -> BitVec a -> Bool
    bvCmpSort = FAbs 0 $ FFunc (bitVecSort 0) $ FFunc (bitVecSort 0) boolSort

    -- eq :: forall a. BitVec a -> BitVec a -> BitVec 1
    bvEqSort = FAbs 0 $ FFunc (bitVecSort 0) $ FFunc (bitVecSort 0) (sizedBitVecSort "Size1")

    -- concat :: forall a b c. BitVec a -> BitVec b -> BitVec c
    bvConcatSort = FAbs 0 $ FAbs 1 $ FAbs 2 $
                     FFunc (bitVecSort 0) $ FFunc (bitVecSort 1) (bitVecSort 2)

interpSym :: Symbol -> Raw -> Sort -> (Symbol, TheorySymbol)
interpSym x n t = (x, Thy x n t Theory)

maxLamArg :: Int
maxLamArg = 7

axiomLiterals :: [(Symbol, Sort)] -> [Expr]
axiomLiterals lts = catMaybes [ lenAxiom l <$> litLen l | (l, t) <- lts, isString t ]
  where
    lenAxiom l n  = EEq (EApp (expr (strLen :: Symbol)) (expr l)) (expr n `ECst` intSort)
    litLen        = fmap (Data.Text.length .  symbolText) . unLitSymbol

--------------------------------------------------------------------------------
-- | Constructors, Selectors and Tests from 'DataDecl'arations.
--------------------------------------------------------------------------------
dataDeclSymbols :: DataDecl -> [(Symbol, TheorySymbol)]
dataDeclSymbols d = ctorSymbols d ++ testSymbols d ++ selectSymbols d

-- | 'selfSort d' returns the _self-sort_ of 'd' :: 'DataDecl'.
--   See [NOTE:DataDecl] for details.

selfSort :: DataDecl -> Sort
selfSort (DDecl c n _) = fAppTC c (FVar <$> [0..(n-1)])

-- | 'fldSort d t' returns the _real-sort_ of 'd' if 't' is the _self-sort_
--   and otherwise returns 't'. See [NOTE:DataDecl] for details.

fldSort :: DataDecl -> Sort -> Sort
fldSort d (FTC c)
  | c == ddTyCon d = selfSort d
fldSort _ s        = s

--------------------------------------------------------------------------------
ctorSymbols :: DataDecl -> [(Symbol, TheorySymbol)]
--------------------------------------------------------------------------------
ctorSymbols d = ctorSort d <$> ddCtors d

ctorSort :: DataDecl -> DataCtor -> (Symbol, TheorySymbol)
ctorSort d ctor = (x, Thy x (symbolRaw x) t Ctor)
  where
    x           = symbol ctor
    t           = mkFFunc n (ts ++ [selfSort d])
    n           = ddVars d
    ts          = fldSort d . dfSort <$> dcFields ctor

--------------------------------------------------------------------------------
testSymbols :: DataDecl -> [(Symbol, TheorySymbol)]
--------------------------------------------------------------------------------
testSymbols d = testTheory t . symbol <$> ddCtors d
  where
    t         = mkFFunc (ddVars d) [selfSort d, boolSort]

testTheory :: Sort -> Symbol -> (Symbol, TheorySymbol)
testTheory t x = (sx, Thy sx raw t Test)
  where
    sx         = testSymbol x
    raw        = "is-" <> symbolRaw x

symbolRaw :: Symbol -> T.Text
symbolRaw = T.fromStrict . symbolSafeText

--------------------------------------------------------------------------------
selectSymbols :: DataDecl -> [(Symbol, TheorySymbol)]
--------------------------------------------------------------------------------
selectSymbols d = theorify <$> concatMap (ctorSelectors d) (ddCtors d)

-- | 'theorify' converts the 'Sort' into a full 'TheorySymbol'
theorify :: (Symbol, Sort) -> (Symbol, TheorySymbol)
theorify (x, t) = (x, Thy x (symbolRaw x) t Field)

ctorSelectors :: DataDecl -> DataCtor -> [(Symbol, Sort)]
ctorSelectors d ctor = fieldSelector d <$> dcFields ctor

fieldSelector :: DataDecl -> DataField -> (Symbol, Sort)
fieldSelector d f = (symbol f, mkFFunc n [selfSort d, ft])
  where
    ft            = fldSort d $ dfSort f
    n             = ddVars  d

{- | [NOTE:DataDecl]  This note explains the set of symbols generated
     for the below data-declaration:

  data Vec 1 = [
    | nil  { }
    | cons { vHead : @(0), vTail : Vec}
  ]

We call 'Vec' the _self-sort_ of the data-type, and we want to ensure that
in all constructors, tests and selectors, the _self-sort_ is replaced with
the actual sort, namely, 'Vec @(0)'.

Constructors  // ctor : (fld-sorts) => me

        nil   : func(1, [Vec @(0)])
        cons  : func(1, [@(0); Vec @(0); Vec @(0)])

Tests         // is#ctor : (me) => bool

      is#nil  : func(1, [Vec @(0); bool])
      is#cons : func(1, [Vec @(0); bool])

Selectors     // fld : (me) => fld-sort

      vHead   : func(1, [Vec @(0); @(0)])
      vTail   : func(1, [Vec @(0); Vec @(0)])

-}
