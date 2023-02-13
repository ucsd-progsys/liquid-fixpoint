{-# LANGUAGE OverloadedStrings    #-}

-- | Wrapper around `Data.Text.Builder` that exports some useful combinators

module Language.Fixpoint.Utils.Builder
  ( Builder
  , B.lazyByteString
  , fromText
  , fromString
  , B.toLazyByteString
  , parens
  , (<+>)
  , parenSeqs
  , seqs
  , key
  , key2
  , key3
  , bShow
  , bFloat
  ) where

import           Data.Foldable (fold)
import           Data.String
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Data.List              as L
import qualified Numeric


fromText :: T.Text -> Builder
fromText t = B.byteString $ T.encodeUtf8 t

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
