{-# LANGUAGE DeriveGeneric #-}

-- | This module implements functions that print out
--   statistics about the constraints.

module Language.Fixpoint.Partition (
    CPart (..)

  -- * Split constraints
  , partition, partition', partitionN

  -- * Information about cores
  , MCInfo (..)
  , mcInfo

  ) where

import           GHC.Conc                  (getNumProcessors)
import           Debug.Trace (trace)
import           Control.Monad (forM_)
import           GHC.Generics                   (Generic)
import           Language.Fixpoint.Utils.Misc         hiding (group)-- (fst3, safeLookup, mlookup, groupList)
import           Language.Fixpoint.Utils.Files
import           Language.Fixpoint.Types.Config
import           Language.Fixpoint.Types.PrettyPrint
import qualified Language.Fixpoint.Types.Visitor      as V
import qualified Language.Fixpoint.Types        as F
import qualified Data.HashMap.Strict            as M
import qualified Data.Graph                     as G
import qualified Data.Tree                      as T
import           Data.Maybe                     (isJust, mapMaybe, listToMaybe, fromMaybe)
import           Data.Hashable
import           Text.PrettyPrint.HughesPJ
import           Data.List (sortBy)

-------------------------------------------------------------------------
-- | Constraint Partition Container -------------------------------------
-------------------------------------------------------------------------

data CPart a = CPart { pws :: M.HashMap F.KVar (F.WfC a)
                     , pcm :: M.HashMap Integer (F.SubC a)
                     , cFileName :: FilePath
                     }

instance Monoid (CPart a) where
   mempty = CPart mempty mempty mempty
   mappend l r = CPart { pws = pws l `mappend` pws r
                       , pcm = pcm l `mappend` pcm r
                       , cFileName = cFileName l
                       }

-------------------------------------------------------------------------
-- | Multicore info -----------------------------------------------------
-------------------------------------------------------------------------

data MCInfo = MCInfo { mcCores :: Int
                     , mcMinPartSize :: Int
                     , mcMaxPartSize :: Int
                     } deriving (Show)

mcInfo :: Config -> IO MCInfo
mcInfo c = do
   np <- getNumProcessors
   let nc = fromMaybe np (cores c)
   return MCInfo { mcCores = nc
                 , mcMinPartSize = minPartSize c
                 , mcMaxPartSize = maxPartSize c
                 }



partition :: (F.Fixpoint a) => Config -> F.FInfo a -> IO (F.Result a)
partition cfg fi
  = do dumpPartitions cfg fis
       dumpEdges      cfg es
       return mempty
    where
       (es, fis) = partition' Nothing fi

------------------------------------------------------------------------------
-- | Partition an FInfo into multiple disjoint FInfos
------------------------------------------------------------------------------
partition' :: Maybe MCInfo -- ^ Nothing to produce the maximum possible
                             -- number of partitions. Or a MultiCore Info
                             -- to control the partitioning
           -> F.FInfo a -> (KVGraph, [F.FInfo a])
------------------------------------------------------------------------------
partition' mn fi  = case mn of
   Nothing -> (g, fis mkPartition id)
   (Just mi) -> (g, partitionN mi fi $ fis mkPartition' finfoToCpart)
  where
    es             = kvEdges   fi
    g              = kvGraph   es
    css            = decompose g
    fis partF ctor = applyNonNull [ctor fi] (pbc partF) css
    pbc partF      = partitionByConstraints partF fi


-- | Partition an FInfo into a specific number of partitions of roughly equal
-- amounts of work
partitionN :: MCInfo    -- ^ describes thresholds and partiton amounts
           -> F.FInfo a   -- ^ The originial FInfo
           -> [CPart a] -- ^ A list of the smallest possible CParts
           -> [F.FInfo a] -- ^ At most N partitions of at least thresh work
partitionN mi fi cp
   | cpartSize (finfoToCpart fi) <= minThresh = [fi]
   | otherwise = map (cpartToFinfo fi) $ toNParts sortedParts
   where
      toNParts p
         | isDone p = p
         | otherwise = toNParts $ insertSorted firstTwo rest
            where (firstTwo, rest) = unionFirstTwo p
      isDone [] = True
      isDone [_] = True
      isDone fi'@(a:b:_) = length fi' <= prts
                            && (cpartSize a >= minThresh
                                || cpartSize a + cpartSize b >= maxThresh)
      sortedParts = sortBy sortPredicate cp
      unionFirstTwo (a:b:xs) = (a `mappend` b, xs)
      sortPredicate lhs rhs
         | cpartSize lhs < cpartSize rhs = GT
         | cpartSize lhs > cpartSize rhs = LT
         | otherwise = EQ
      insertSorted a []     = [a]
      insertSorted a (x:xs) = if sortPredicate a x == LT
                              then x : insertSorted a xs
                              else a : x : xs
      prts      = mcCores mi
      minThresh = mcMinPartSize mi
      maxThresh = mcMaxPartSize mi


-- | Return the "size" of a CPart. Used to determine if it's
-- substantial enough to be worth parallelizing.
cpartSize :: CPart a -> Int
cpartSize c = (M.size . pcm) c + (length . pws) c

-- | Convert a CPart to an FInfo
cpartToFinfo :: F.FInfo a -> CPart a -> F.FInfo a
cpartToFinfo fi p = fi { F.cm = pcm p
                       , F.ws = pws p
                       , F.fileName = cFileName p
                       }

-- | Convert an FInfo to a CPart
finfoToCpart :: F.FInfo a -> CPart a
finfoToCpart fi = CPart { pcm = F.cm fi
                        , pws = F.ws fi
                        , cFileName = F.fileName fi
                        }

-------------------------------------------------------------------------------------
dumpPartitions :: (F.Fixpoint a) => Config -> [F.FInfo a] -> IO ()
-------------------------------------------------------------------------------------
dumpPartitions cfg fis =
  forM_ fis $ \fi ->
    writeFile (F.fileName fi) (render $ F.toFixpoint cfg fi)

partFile :: F.FInfo a -> Int -> FilePath
partFile fi j = {- trace ("partFile: " ++ fjq) -} fjq
  where
    fjq = extFileName (Part j) (F.fileName fi)

-------------------------------------------------------------------------------------
dumpEdges :: Config -> KVGraph -> IO ()
-------------------------------------------------------------------------------------
dumpEdges cfg = writeFile f . render . ppGraph
  where
    f         = extFileName Dot (inFile cfg)

ppGraph :: KVGraph -> Doc
ppGraph g = ppEdges [ (v, v') | (v,_,vs) <- g, v' <- vs]

ppEdges :: [CEdge] -> Doc
ppEdges es = vcat [pprint v <+> text "-->" <+> pprint v' | (v, v') <- es]

-- | Type alias for a function to construct a partition. mkPartition and
-- mkPartition' are the two primary functions that conform to this interface
type PartitionCtor a b = F.FInfo a
                         -> M.HashMap Int [(Integer, F.SubC a)]
                         -> M.HashMap Int [(F.KVar, F.WfC a)]
                         -> Int
                         -> b -- ^ typically a F.FInfo a or F.CPart a

partitionByConstraints :: PartitionCtor a b -- ^ mkPartition or mkPartition'
                          -> F.FInfo a
                          -> KVComps
                          -> ListNE b -- ^ [F.FInfo a] or [F.CPart a]
partitionByConstraints f fi kvss = f fi icM iwM <$> js
  where
    js   = fst <$> jkvs                                -- groups
    gc   = groupFun cM                                 -- (i, ci) |-> j
    gk   = groupFun kM                                 -- k       |-> j

    iwM  = groupMap (gk . fst) (M.toList (F.ws fi))             -- j |-> [w]
    icM  = groupMap (gc . fst) (M.toList (F.cm fi))  -- j |-> [(i, ci)]

    jkvs = zip [1..] kvss
    kvI  = [ (x, j) | (j, kvs) <- jkvs, x <- kvs ]
    kM   = M.fromList [ (k, i) | (KVar k, i) <- kvI ]
    cM   = M.fromList [ (c, i) | (Cstr c, i) <- kvI ]

mkPartition fi icM iwM j
  = fi { F.cm       = M.fromList $ M.lookupDefault [] j icM
       , F.ws       = M.fromList $ M.lookupDefault [] j iwM
       , F.fileName = partFile fi j
       }

mkPartition' fi icM iwM j
  = CPart { pcm       = M.fromList $ M.lookupDefault [] j icM
          , pws       = M.fromList $ M.lookupDefault [] j iwM
          , cFileName = partFile fi j
          }

groupFun :: (Show k, Eq k, Hashable k) => M.HashMap k Int -> k -> Int
groupFun m k = safeLookup ("groupFun: " ++ show k) k m

-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------

data CVertex = KVar F.KVar
             | Cstr Integer
               deriving (Eq, Ord, Show, Generic)

instance PPrint CVertex where
  pprint (KVar k) = pprint k
  pprint (Cstr i) = text "id:" <+> pprint i

instance Hashable CVertex

type CEdge    = (CVertex, CVertex)
type KVGraph  = [(CVertex, CVertex, [CVertex])]

type Comps a  = [[a]]
type KVComps  = Comps CVertex

-------------------------------------------------------------------------------
decompose :: KVGraph -> KVComps
-------------------------------------------------------------------------------
decompose kg = map (fst3 . f) <$> vss
  where
    (g,f,_)  = G.graphFromEdges kg
    vss      = T.flatten <$> G.components g

kvGraph :: [CEdge] -> KVGraph
kvGraph es = [(v,v,vs) | (v, vs) <- groupList es ]

kvEdges :: F.FInfo a -> [CEdge]
kvEdges fi = selfes ++ concatMap (subcEdges bs) cs
  where
    bs     = F.bs fi
    cs     = M.elems (F.cm fi)
    selfes = [(Cstr i, Cstr i) | c <- cs, let i = F.subcId c] ++
             [(KVar k, KVar k) | k <- fiKVars fi]

fiKVars :: F.FInfo a -> [F.KVar]
fiKVars = M.keys . F.ws

subcEdges :: F.BindEnv -> F.SubC a -> [CEdge]
subcEdges bs c =  [(KVar k, Cstr i ) | k  <- V.envKVars bs c]
               ++ [(Cstr i, KVar k') | k' <- V.rhsKVars c ]
  where
    i          = F.subcId c
