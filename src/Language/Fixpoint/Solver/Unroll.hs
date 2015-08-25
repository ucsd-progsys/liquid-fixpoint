{-# LANGUAGE PartialTypeSignatures #-}

module Language.Fixpoint.Solver.Unroll (unroll) where

import           Data.Maybe
import           Data.Hashable
import           Data.Bifunctor
import           Control.Monad
import           Control.Comonad
import qualified Control.Arrow as A
import           Language.Fixpoint.Names (renameSymbol)
import           Language.Fixpoint.Visitor (lhsKVars, rhsKVars)
import           GHC.Exts (groupWith)

import           Language.Fixpoint.Config
import           Language.Fixpoint.Types
import qualified Data.HashMap.Strict              as M

data Node b a = Node { me :: a, kids :: [Node a b] }

instance Bifunctor Node where
  bimap f g (Node a bs) = Node (g a) [Node (f b) (bimap f g <$> as) | Node b as <- bs]

instance Functor (Node b) where
  fmap = bimap id

gmap :: (b -> c) -> Node b a -> Node c a
gmap = flip bimap id

instance Comonad (Node b) where
  extract (Node a _) = a 
  duplicate t@(Node _ bs) = Node t [Node b (duplicate <$> as) | Node b as <- bs]

unroll :: FInfo a -> Integer -> FInfo a
unroll fi start = fi -- {cm = M.fromList $ extras ++ map reid cons'}
  where m = cm fi
        mlookup v = M.lookupDefault (error $"cons # "++show v++" not found") v m
        kidsm = M.fromList $ (fst.head A.&&& (snd <$>)) <$> groupWith fst pairs
          where pairs = [(k,i)|(i,ks) <- A.second rhs <$> M.toList m, k<-ks]
        klookup k = M.lookupDefault (error $"kids for "++show k++" not found") k kidsm

        rhs, lhs :: SubC a -> [KVar]
        rhs = rhsKVars
        lhs = lhsKVars (bs fi)

        cons' = hylo (prime . (kvarSubs <<=) . prune . index M.empty) =<< lhs (mlookup start)
        extras = M.toList $ M.filter ((==[]).lhs) m
        reid :: (Integer, SubC a) -> (Integer, SubC a)
        reid (b,a) = (b, a { sid = Just b })

        hylo f = cata.f.ana
        ana k = Node k [Node v $ ana <$> rhs (mlookup v) | v <- klookup k]
        cata (Node _ bs) = join $ join [[b]:(cata<$>ns) | Node b ns <- bs]

        -- Removes all nodes numbered higher than `depth`
        prune (Node (a,i) l) = Node (a,i) $
          if i>depth
             then []
             else [Node v (fmap prune ns) | Node v ns <- l]

        -- Lists all the subsitutions that are to made
        kvarSubs :: Node b (KVar, Int) -> [(KVar,KVar)]
        kvarSubs t@(Node (k,i) _) = cata $ Node (error "Unroll.cata: :/")
                                                [(\(k,i) -> (k,renameKv k i)) <$> t]

        -- Builds our new constraint graph, now knowing the substitutions.
        prime :: Node (Integer, Int) [(KVar, KVar)] -> Node (Integer, SubC _) [(KVar, KVar)]
        prime (Node subs vs) = Node subs [Node (num v i, substKV subs $ mlookup v) (prime <$> ns) | Node (v,i) ns <- vs]

        -- renumber constraint #a
        num a i = cantor a i $ M.size m

renameKv :: Integral i => KVar -> i -> KVar
-- "k" -> n -> "k_n"
renameKv a i = KV $ renameSymbol (kv a) $ fromIntegral i

substKV :: [(KVar, KVar)] -> SubC a -> SubC a
substKV = undefined -- obviously, @TODO

cantor :: Integer -> Int -> Int -> Integer
-- ^The Cantor pairing function when `i/=0`, offset by `s`. Otherwise, just `v`
cantor v i' s' = if i==0
                  then v
                  else s + i + quot ((v+i)*(v+i+1)) 2
  where s = fromIntegral s'
        i = fromIntegral i'

index :: (Eq a, Hashable a) => M.HashMap a Int -> Node b a -> Node (b,Int) (a,Int)
-- |Number each node by the number of ancestors it has that hae the same label
index m (Node a bs) = Node (a,i) [Node (b,i) (index m' <$> ns) | Node b ns <- bs]
  where i = M.lookupDefault 0 a m
        m' = M.insertWith (+) a 1 m

depth :: Int
-- |Equals 4 @TODO justify me
depth = 4
