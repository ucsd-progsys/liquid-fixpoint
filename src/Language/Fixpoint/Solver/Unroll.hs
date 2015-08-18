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

data Node b a = Node a [Node a b]

instance Bifunctor Node where
  bimap f g (Node a bs) = Node (g a) [Node (f b) (bimap f g <$> as) | Node b as <- bs]

instance Functor (Node b) where
  fmap f = bimap id f

instance Comonad (Node b) where
  extract (Node a _) = a 
  duplicate t@(Node _ bs) = Node t [Node b (duplicate <$> as) | Node b as <- bs]

unroll :: FInfo a -> Integer -> FInfo a
unroll fi start = fi -- {cm = M.fromList $ extras ++ cons'}
  where m = cm fi
        mlookup v = M.lookupDefault (error $"cons # "++show v++" not found") v m
        kidsm = M.fromList $ (fst.head A.&&& (snd <$>)) <$> groupWith fst pairs
          where pairs = [(k,i)|(i,ks) <- A.second rhs <$> M.toList m, k<-ks]
        klookup k = M.lookupDefault (error $"kids for "++show k++" not found") k kidsm

        rhs, lhs :: SubC a -> [KVar]
        rhs = rhsKVars
        lhs = lhsKVars (bs fi)

        cons' = hylo (prime . prune . index M.empty) =<< lhs (mlookup start)
        extras = M.toList $ M.filter ((==[]).lhs) m

        hylo f = cata.f.ana
        ana k = Node k [Node v $ ana <$> rhs (mlookup v) | v <- klookup k]
        cata (Node _ bs) = join $ join [[b]:(cata<$>ns) | Node b ns <- bs]

        prune :: Node Integer (KVar, Int) -> Node Integer (KVar, Int)
        prune (Node (a,i) l) = Node (a,i) $
          if i>depth
             then []
             else [Node v (fmap prune ns) | Node v ns <- l]

        prime :: Node Integer (KVar, Int) -> Node (Integer, SubC _) KVar
        prime (Node (a,i) bs) = Node (renameKv a i) [Node (rename a i b) (prime <$> as) | Node b as <- bs]

        rename :: KVar -> Int -> Integer -> (Integer, SubC _)
        -- adds `i` primes to the kvar `a`
        -- then subsitutes the new kvar for the old in the SubC #`v`
        -- also gives us a new number for `v`, since it's now a different SubC
        rename a i v = (num v i, substKVar a (renameKv a i) (mlookup v))
        substKVar :: KVar -> KVar -> SubC a -> SubC a
        substKVar k k' c = undefined
        num a i = cantor a i $ M.size m

renameKv :: Integral i => KVar -> i -> KVar
renameKv a i = KV $ renameSymbol (kv a) $ fromIntegral i

substKV :: KVar -> KVar -> SubC a
substKV = undefined

cantor :: Integer -> Int -> Int -> Integer
-- The Cantor pairing function, offset by s when i/=0
cantor v i' s' = if i==0
                  then v
                  else s + i + quot ((v+i)*(v+i+1)) 2
  where s = fromIntegral s'
        i = fromIntegral i'

index :: (Eq a, Hashable a) => M.HashMap a Int -> Node b a -> Node b (a,Int)
index m (Node a bs) = Node (a,i) [Node b (index m' <$> ns) | Node b ns <- bs]
  where i = M.lookupDefault 0 a m
        m' = M.insertWith (+) a 1 m

depth :: Int
-- @TODO justify me
depth = 4

