{-# LANGUAGE OverloadedStrings    #-}

-- | Wrapper around `Data.Text.Builder` that exports some useful combinators

module Language.Fixpoint.Utils.Builder
  ( Builder
  , fromLazyByteString
  , fromText
  , fromString
  , toLazyByteString
  , toBuilder
  , parens
  , (<+>)
  , parenSeqs
  , seqs
  , key
  , key2
  , key3
  , bShow
  , bFloat
  , bb
  ) where

import           Data.Foldable (fold)
import           Data.String
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Builder as B
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.List              as L
import qualified Numeric

-- | Offers efficient concatenation, no matter the associativity
data Builder
  = Node Builder Builder
  | Leaf B.Builder

instance IsString Builder where
  fromString = Leaf . fromString

instance Semigroup Builder where
  (<>) = Node

instance Monoid Builder where
  mempty = Leaf mempty

toLazyByteString :: Builder -> ByteString
toLazyByteString = B.toLazyByteString . toBuilder

toBuilder :: Builder -> B.Builder
toBuilder = go mempty
  where
    go tl (Leaf b) = b <> tl
    go tl (Node t0 t1) = go (go tl t1) t0

fromLazyByteString :: ByteString -> Builder
fromLazyByteString = Leaf . B.lazyByteString

fromText :: T.Text -> Builder
fromText t = Leaf $ B.byteString $ T.encodeUtf8 t

parens :: Builder -> Builder
parens b = "(" <>  b <> ")"

infixl 9 <+>
(<+>) :: Builder -> Builder -> Builder
x <+> y = x <> " " <> y

parenSeqs :: [Builder] -> Builder
parenSeqs = parens . seqs

key :: Builder -> Builder -> Builder
key k b = parenSeqs [k, b]

key2 :: Builder -> Builder -> Builder -> Builder
key2 k b1 b2 = parenSeqs [k, b1, b2]

key3 :: Builder -> Builder -> Builder -> Builder ->  Builder
key3 k b1 b2 b3 = parenSeqs [k, b1, b2, b3]

seqs :: [Builder] -> Builder
seqs = fold . L.intersperse " "

bShow :: Show a => a -> Builder
bShow = fromString . show

bFloat :: RealFloat a => a -> Builder
bFloat d = fromString (Numeric.showFFloat Nothing d "")

bb :: ByteString -> Builder
bb = fromLazyByteString
