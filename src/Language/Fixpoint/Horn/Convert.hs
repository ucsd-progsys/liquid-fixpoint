-- | This module contains the code for converting Horn format Constraints
--   into the classic FQ format that is then used for solving.


module Language.Fixpoint.Horn.Convert (convert) where

import Language.Fixpoint.Types       as F
import Language.Fixpoint.Horn.Types  as H

convert :: H.Horn a -> F.SInfo a
convert = error "_fixme_HornConvert"
