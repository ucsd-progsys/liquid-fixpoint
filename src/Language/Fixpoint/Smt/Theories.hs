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
import           Data.ByteString.Builder (Builder)
import           Language.Fixpoint.Types.Sorts
import           Language.Fixpoint.Types.Config
import           Language.Fixpoint.Types
import           Language.Fixpoint.Smt.Types
-- import qualified Data.HashMap.Strict      as M
import           Data.Maybe (catMaybes)
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

bFun :: Raw -> [(Builder, Builder)] -> Builder -> Builder -> Builder
bFun name xts out body = key "define-fun" (seqs [fromText name, args, out, body])
  where
    args = parenSeqs [parens (x <+> t) | (x, t) <- xts]

bFun' :: Raw -> [Builder] -> Builder -> Builder
bFun' name ts out = key "declare-fun" (seqs [fromText name, args, out])
  where
    args = parenSeqs ts

bSort :: Raw -> Builder -> Builder
bSort name def = key "define-sort" (fromText name <+> "()" <+> def)

z3Preamble :: Config -> [Builder]
z3Preamble u
  = stringPreamble u ++
    [ bSort elt
        "Int"
    , bSort set
        (key2 "Array" (fromText elt) "Bool")
    , bFun emp
        []
        (fromText set)
        (parens (key "as const" (fromText set) <+> "false"))
    , bFun sng
        [("x", fromText elt)]
        (fromText set)
        (key3 "store" (parens (key "as const" (fromText set) <+> "false")) "x" "true")
    , bFun mem
        [("x", fromText elt), ("s", fromText set)]
        "Bool"
        "(select s x)"
    , bFun add
        [("s", fromText set), ("x", fromText elt)]
        (fromText set)
        "(store s x true)"
    , bFun cup
        [("s1", fromText set), ("s2", fromText set)]
        (fromText set)
        "((_ map or) s1 s2)"
    , bFun cap
        [("s1", fromText set), ("s2", fromText set)]
        (fromText set)
        "((_ map and) s1 s2)"
    , bFun com
        [("s", fromText set)]
        (fromText set)
        "((_ map not) s)"
    , bFun dif
        [("s1", fromText set), ("s2", fromText set)]
        (fromText set)
        (key2 (fromText cap) "s1" (key (fromText com) "s2"))
    , bFun sub
        [("s1", fromText set), ("s2", fromText set)]
        "Bool"
        (key2 "=" (fromText emp) (key2 (fromText dif) "s1" "s2"))

    -- Maps
    , bSort map
        (key2 "Array" (fromText elt) (fromText elt))
    , bFun sel
        [("m", fromText map), ("k", fromText elt)]
        (fromText elt)
        "(select m k)"
    , bFun sto
        [("m", fromText map), ("k", fromText elt), ("v", fromText elt)]
        (fromText map)
        "(store m k v)"
    , bFun mcup
        [("m1", fromText map), ("m2", fromText map)]
        (fromText map)
        (key2 (key "_ map" (key2 "+" (parens (fromText elt <+> fromText elt)) (fromText elt))) "m1" "m2")
    , bFun mprj -- See [Interaction Between Map and Set]
        [("s", fromText set), ("m", fromText map)]
        (fromText map)
        (key3
          (key "_ map"
            (key2 "ite"
              (parens ("Bool" <+> fromText elt <+> fromText elt))
              (fromText elt)
            )
          )
          "s"
          "m"
          (parens (key "as const" (key2 "Array" (fromText elt) (fromText elt)) <+> "0"))
        )
    , bFun mToSet -- See [Interaction Between Map and Set]
        [("m", fromText map)]
        (fromText set)
        (key2
          (key "_ map"
            (key2 ">"
              (parens (fromText elt <+> fromText elt))
              "Bool"
            )
          )
          "m"
          (parens (key "as const" (key2 "Array" (fromText elt) (fromText elt)) <+> "0"))
        )
    , bFun mmax -- See [Map max and min]
        [("m1", fromText map),("m2", fromText map)]
        (fromText map)
        "(lambda ((i Int)) (ite (> (select m1 i) (select m2 i)) (select m1 i) (select m2 i)))"
    , bFun mmin -- See [Map max and min]
        [("m1", fromText map),("m2", fromText map)]
        (fromText map)
        "(lambda ((i Int)) (ite (< (select m1 i) (select m2 i)) (select m1 i) (select m2 i)))"
    , bFun mshift -- See [Map key shift]
        [("n", "Int"),("m", fromText map)]
        (fromText map)
        "(lambda ((i Int)) (select m (- i n)))"
    , bFun mdef
        [("v", fromText elt)]
        (fromText map)
        (key (key "as const" (parens (fromText map))) "v")
    , bFun boolToIntName
        [("b", "Bool")]
        "Int"
        "(ite b 1 0)"

    , uifDef u (symbolText mulFuncName) "*"
    , uifDef u (symbolText divFuncName) "div"
    ]

-- RJ: Am changing this to `Int` not `Real` as (1) we usually want `Int` and
-- (2) have very different semantics. TODO: proper overloading, post genEApp
uifDef :: Config -> Data.Text.Text -> Data.Text.Text -> Builder
uifDef cfg f op
  | linear cfg || Z3 /= solver cfg
  = bFun' f ["Int", "Int"] "Int"
  | otherwise
  = bFun f [("x", "Int"), ("y", "Int")] "Int" (key2 (fromText op) "x" "y")

cvc4Preamble :: Config -> [Builder]
cvc4Preamble z
  = [        "(set-logic ALL_SUPPORTED)"]
  ++ commonPreamble z
  ++ cvc4MapPreamble z

commonPreamble :: Config -> [Builder]
commonPreamble _ --TODO use uif flag u (see z3Preamble)
  = [ bSort elt    "Int"
    , bSort set    "Int"
    , bSort string "Int"
    , bFun' emp []               (fromText set)
    , bFun' sng [fromText elt]         (fromText set)
    , bFun' add [fromText set, fromText elt] (fromText set)
    , bFun' cup [fromText set, fromText set] (fromText set)
    , bFun' cap [fromText set, fromText set] (fromText set)
    , bFun' dif [fromText set, fromText set] (fromText set)
    , bFun' sub [fromText set, fromText set] "Bool"
    , bFun' mem [fromText elt, fromText set] "Bool"
    , bFun boolToIntName [("b", "Bool")] "Int" "(ite b 1 0)"
    ]

cvc4MapPreamble :: Config -> [Builder]
cvc4MapPreamble _ =
    [ bSort map    (key2 "Array" (fromText elt) (fromText elt))
    , bFun sel [("m", fromText map), ("k", fromText elt)]                (fromText elt) "(select m k)"
    , bFun sto [("m", fromText map), ("k", fromText elt), ("v", fromText elt)] (fromText map) "(store m k v)"
    ]

smtlibPreamble :: Config -> [Builder]
smtlibPreamble z --TODO use uif flag u (see z3Preamble)
  = commonPreamble z
 ++ [ bSort map "Int"
    , bFun' sel [fromText map, fromText elt] (fromText elt)
    , bFun' sto [fromText map, fromText elt, fromText elt] (fromText map)
    ]

stringPreamble :: Config -> [Builder]
stringPreamble cfg | stringTheory cfg
  = [ bSort string "String"
    , bFun strLen [("s", fromText string)] "Int" (key (fromText z3strlen) "s")
    , bFun strSubstr [("s", fromText string), ("i", "Int"), ("j", "Int")] (fromText string) (key (fromText z3strsubstr) "s i j")
    , bFun strConcat [("x", fromText string), ("y", fromText string)] (fromText string) (key (fromText z3strconcat) "x y")
    ]

stringPreamble _
  = [ bSort string "Int"
    , bFun' strLen [fromText string] "Int"
    , bFun' strSubstr [fromText string, "Int", "Int"] (fromText string)
    , bFun' strConcat [fromText string, fromText string] (fromText string)
    ]

--------------------------------------------------------------------------------
-- | Exported API --------------------------------------------------------------
--------------------------------------------------------------------------------
smt2Symbol :: SymEnv -> Symbol -> Maybe Builder
smt2Symbol env x = fromText . tsRaw <$> symEnvTheory x env

instance SMTLIB2 SmtSort where
  smt2 _ = smt2SmtSort

smt2SmtSort :: SmtSort -> Builder
smt2SmtSort SInt         = "Int"
smt2SmtSort SReal        = "Real"
smt2SmtSort SBool        = "Bool"
smt2SmtSort SString      = fromText string
smt2SmtSort SSet         = fromText set
smt2SmtSort SMap         = fromText map
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
  | f == setEmpty = Just (fromText emp)
  | f == setEmp   = Just (key2 "=" (fromText emp) d)
  | f == setSng   = Just (key (fromText sng) d) -- Just (key2 (bb add) (bb emp) d)

smt2App k env f (d:ds)
  | Just fb <- smt2AppArg k env f
  = Just $ key fb (d <> mconcat [ " " <> d | d <- ds])

smt2App _ _ _ _    = Nothing

smt2AppArg :: VarAs -> SymEnv -> Expr -> Maybe Builder
smt2AppArg k env (ECst (dropECst -> EVar f) t)
  | Just fThy <- symEnvTheory f env
  = Just $ if isPolyCtor fThy t
            then k env f (ffuncOut t)
            else fromText (tsRaw fThy)

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

preamble :: Config -> SMTSolver -> [Builder]
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
  , interpSym bvOrName  "bvor"  bvBopSort
  , interpSym bvAndName "bvand" bvBopSort
  , interpSym bvAddName "bvadd" bvBopSort
  , interpSym bvSubName "bvsub" bvBopSort

  , interpSym intbv32Name "(_ int2bv 32)" (FFunc intSort bitVec32Sort)
  , interpSym intbv64Name "(_ int2bv 64)" (FFunc intSort bitVec64Sort)
  , interpSym bv32intName  "(_ bv2int 32)" (FFunc bitVec32Sort intSort)
  , interpSym bv64intName   "(_ bv2int 64)" (FFunc bitVec64Sort intSort)

  , interpSym strLen    strLen    strLenSort
  , interpSym strSubstr strSubstr substrSort
  , interpSym strConcat strConcat concatstrSort
  , interpSym boolInt   boolInt   (FFunc boolSort intSort)
  ]
  where
    boolInt    = boolToIntName
    setAddSort = FAbs 0 $ FFunc (setSort $ FVar 0) $ FFunc (FVar 0)           (setSort $ FVar 0)
    setBopSort = FAbs 0 $ FFunc (setSort $ FVar 0) $ FFunc (setSort $ FVar 0) (setSort $ FVar 0)
    setMemSort = FAbs 0 $ FFunc (FVar 0) $ FFunc (setSort $ FVar 0) boolSort
    setCmpSort = FAbs 0 $ FFunc (setSort $ FVar 0) $ FFunc (setSort $ FVar 0) boolSort
    mapSelSort = FAbs 0 $ FAbs 1 $ FFunc (mapSort (FVar 0) (FVar 1))
                                 $ FFunc (FVar 0) (FVar 1)
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
    mapStoSort = FAbs 0 $ FAbs 1 $ FFunc (mapSort (FVar 0) (FVar 1))
                                 $ FFunc (FVar 0)
                                 $ FFunc (FVar 1)
                                         (mapSort (FVar 0) (FVar 1))
    mapDefSort = FAbs 0 $ FAbs 1 $ FFunc (FVar 1)
                                         (mapSort (FVar 0) (FVar 1))

    bvBopSort  = FAbs 0 $ FFunc (bitVecSort (FVar 0)) (FFunc (bitVecSort (FVar 0)) (bitVecSort (FVar 0)))

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

symbolRaw :: Symbol -> Data.Text.Text
symbolRaw = symbolSafeText

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
