{-# LANGUAGE MultilineStrings #-}

module SimplifyKVarTests (tests) where

import Control.Monad (when)
import Language.Fixpoint.Parse
import qualified Language.Fixpoint.Types as F
import qualified Language.Fixpoint.Solver as F
import Test.Tasty
import Test.Tasty.HUnit


tests :: TestTree
tests =
  testGroup "simplifyKVar" $ map simplificationTest
    [ SimplificationTest
        { name = "single elimination"
        , expected = """
            exists [y : int] . P C y
          """
        , input = """
            exists [x : int, y : int] . x == C && P x y
          """
        }

    , SimplificationTest
        { name = "full elimination"
        , expected = """
            P C D
          """
        , input = """
            exists [x : int, y : int] . x == C && P x y && y == D
          """
        }

    , SimplificationTest
        { name = "alpha equivalence"
        , expected = """
            (exists [w : int, z : int] . Q w z) &&
            (exists [w : int, z : int] . P w z)
          """
        , input = """
            (exists [w : int, z : int] . Q w z) &&
            (exists [w : int, z : int] . P w z) &&
            (exists [x : int, y : int] . P x y)
          """
        }

    , SimplificationTest
        { name = "floating"
        , expected = """
            (exists [x : int, y : int] . P x y) && A == C
          """
        , input = """
            exists [x : int, y : int] . A == C && P x y
          """
        }

    , SimplificationTest
        { name = "inner floating"
        , expected = """
            (exists [x : int] . P x && Q x) && (exists [y : int] . P y)
          """
        , input = """
            exists [x : int] . P x && (exists [ y : int] . P y && Q x)
          """
        }

    , largeSimplificationTest
    ]

data SimplificationTest = SimplificationTest
  { input :: String
  , expected :: String
  , name :: String
  }

simplificationTest :: SimplificationTest -> TestTree
simplificationTest test =
  testCase (name test) $ do
    let actual = F.simplifyKVar (doParse'' True predP (name test) (input test))
        expectedE = doParse'' True predP (name test) (expected test)
    when (not (F.alphaEq actual expectedE)) $ do
      assertFailure $ unlines
        [ "output is not as expected"
        , "Expected:"
        , expected test
        , ""
        , "Actual:"
        , F.showpp actual
        ]

largeSimplificationTest :: SimplificationTest
largeSimplificationTest =
  SimplificationTest
    { name = "large simplification"
    , expected = """
        exists [w : int] . Test.gt0xy w i##aS7
      """
    , input = """
        exists [VV##1821##k_ : int,
                i##aS7##k_ : int,
                lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                lq_tmpx##1823##k_ : int,
                lq_tmpx##1824##k_ : int]
              . (exists [w : int,
                         w2 : int,
                         x : int,
                         y : Tuple0,
                         VV##F##13 : int]
                   . VV##1821##k_ == VV##F##13
                     && i##aS7##k_ == i##aS7
                     && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                     && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                     && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                     && lq_tmpx##1823##k_ == w
                     && lq_tmpx##1824##k_ == x
                     && (exists [VV##1829 : int,
                                 lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                 lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                                 VV##1805##k_ : int,
                                 lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                 i##aS7##k_ : int]
                           . VV##1829 == w
                             && VV##1805##k_ == w
                             && i##aS7##k_ == i##aS7
                             && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                             && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                             && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                             && (exists [VV##F##3 : int]
                                   . Test.gt0xy VV##F##3 i##aS7
                                     && VV##1805##k_ == VV##F##3
                                     && i##aS7##k_ == i##aS7
                                     && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                     && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                     && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc))
                     && (exists [VV##1830 : int,
                                 lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                 lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                 i##aS7##k_ : int,
                                 lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                                 VV##1807##k_ : int]
                           . VV##1830 == w2
                             && VV##1807##k_ == w2
                             && i##aS7##k_ == i##aS7
                             && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                             && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                             && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                             && (exists [w : int,
                                         VV##F##14 : int,
                                         lq_rnmx##255 : Tuple0]
                                   . (exists [VV##1828 : int,
                                              lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                              lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                                              VV##1805##k_ : int,
                                              lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                              i##aS7##k_ : int]
                                        . VV##1828 == w
                                          && VV##1805##k_ == w
                                          && i##aS7##k_ == i##aS7
                                          && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                          && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                          && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                                          && (exists [VV##F##3 : int]
                                                . Test.gt0xy VV##F##3 i##aS7
                                                  && VV##1805##k_ == VV##F##3
                                                  && i##aS7##k_ == i##aS7
                                                  && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                  && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                  && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc))
                                     && (exists [lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                                 lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                                 i##aS7##k_ : int,
                                                 lq_tmpx##1812##k_ : Tuple0,
                                                 VV##1809##k_ : int,
                                                 lq_tmpx##1811##k_ : int,
                                                 lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int)]
                                           . VV##1809##k_ == VV##F##14
                                             && i##aS7##k_ == i##aS7
                                             && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                             && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                             && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                                             && lq_tmpx##1811##k_ == w
                                             && lq_tmpx##1812##k_ == lq_rnmx##255
                                             && (exists [VV##F##12 : int,
                                                         lq_tmpx##1811 : int,
                                                         lq_tmpx##1812 : Tuple0,
                                                         lq_tmpdb##43 : int,
                                                         lq_tmpdb##44 : Tuple0]
                                                   . VV##F##12 == i##aS7
                                                     && VV##1809##k_ == VV##F##12
                                                     && i##aS7##k_ == i##aS7
                                                     && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                     && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                     && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                                                     && lq_tmpx##1811##k_ == lq_tmpx##1811
                                                     && lq_tmpx##1812##k_ == lq_tmpx##1812))
                                     && (exists [i##aS7##k_ : int,
                                                 lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                                 lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                                                 lq_tmpx##1801 : Tuple0,
                                                 lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                                 VV##1799##k_ : Tuple0]
                                           . VV##1799##k_ == lq_rnmx##255
                                             && i##aS7##k_ == i##aS7
                                             && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                             && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                             && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                                             && lq_tmpx##1801 == lq_rnmx##255
                                             && (exists [VV##F##10 : Tuple0]
                                                   . VV##1799##k_ == VV##F##10
                                                     && i##aS7##k_ == i##aS7
                                                     && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                     && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                     && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc))
                                     && VV##1807##k_ == VV##F##14
                                     && i##aS7##k_ == i##aS7
                                     && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                     && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                     && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc))
                     && (exists [i##aS7##k_ : int,
                                 lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                 lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                                 lq_tmpx##1801 : Tuple0,
                                 lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                 VV##1799##k_ : Tuple0]
                           . VV##1799##k_ == y
                             && i##aS7##k_ == i##aS7
                             && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                             && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                             && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                             && lq_tmpx##1801 == y
                             && (exists [VV##F##10 : Tuple0]
                                   . VV##1799##k_ == VV##F##10
                                     && i##aS7##k_ == i##aS7
                                     && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                     && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                     && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc))
                     && (exists [i##aS7##k_ : int,
                                 lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                 lq_tmpx##1804 : int,
                                 lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                                 lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                 VV##1802##k_ : int]
                           . VV##1802##k_ == x
                             && i##aS7##k_ == i##aS7
                             && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                             && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                             && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                             && lq_tmpx##1804 == x
                             && (exists [VV##F##6 : int]
                                   . (exists [i##aS7##k_ : int,
                                              lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                              lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                              VV##1796##k_ : int]
                                        . VV##1796##k_ == VV##F##6
                                          && i##aS7##k_ == i##aS7
                                          && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                          && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                          && (exists [VV##F##7 : int]
                                                . (exists [lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                                           lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                                           i##aS7##k_ : int,
                                                           lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                                                           VV##1807##k_ : int]
                                                     . VV##1807##k_ == VV##F##7
                                                       && i##aS7##k_ == i##aS7
                                                       && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                       && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                       && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                                                       && (exists [w : int,
                                                                   VV##F##14 : int,
                                                                   lq_rnmx##255 : Tuple0]
                                                             . (exists [VV##1828 : int,
                                                                        lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                                                        lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                                                                        VV##1805##k_ : int,
                                                                        lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                                                        i##aS7##k_ : int]
                                                                  . VV##1828 == w
                                                                    && VV##1805##k_ == w
                                                                    && i##aS7##k_ == i##aS7
                                                                    && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                                    && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                                    && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                                                                    && (exists [VV##F##3 : int]
                                                                          . Test.gt0xy VV##F##3 i##aS7
                                                                            && VV##1805##k_ == VV##F##3
                                                                            && i##aS7##k_ == i##aS7
                                                                            && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                                            && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                                            && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc))
                                                               && (exists [lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                                                           lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                                                           i##aS7##k_ : int,
                                                                           lq_tmpx##1812##k_ : Tuple0,
                                                                           VV##1809##k_ : int,
                                                                           lq_tmpx##1811##k_ : int,
                                                                           lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int)]
                                                                     . VV##1809##k_ == VV##F##14
                                                                       && i##aS7##k_ == i##aS7
                                                                       && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                                       && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                                       && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                                                                       && lq_tmpx##1811##k_ == w
                                                                       && lq_tmpx##1812##k_ == lq_rnmx##255
                                                                       && (exists [VV##F##12 : int,
                                                                                   lq_tmpx##1811 : int,
                                                                                   lq_tmpx##1812 : Tuple0,
                                                                                   lq_tmpdb##43 : int,
                                                                                   lq_tmpdb##44 : Tuple0]
                                                                             . VV##F##12 == i##aS7
                                                                               && VV##1809##k_ == VV##F##12
                                                                               && i##aS7##k_ == i##aS7
                                                                               && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                                               && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                                               && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                                                                               && lq_tmpx##1811##k_ == lq_tmpx##1811
                                                                               && lq_tmpx##1812##k_ == lq_tmpx##1812))
                                                               && (exists [i##aS7##k_ : int,
                                                                           lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                                                           lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                                                                           lq_tmpx##1801 : Tuple0,
                                                                           lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                                                           VV##1799##k_ : Tuple0]
                                                                     . VV##1799##k_ == lq_rnmx##255
                                                                       && i##aS7##k_ == i##aS7
                                                                       && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                                       && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                                       && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                                                                       && lq_tmpx##1801 == lq_rnmx##255
                                                                       && (exists [VV##F##10 : Tuple0]
                                                                             . VV##1799##k_ == VV##F##10
                                                                               && i##aS7##k_ == i##aS7
                                                                               && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                                               && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                                               && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc))
                                                               && VV##1807##k_ == VV##F##14
                                                               && i##aS7##k_ == i##aS7
                                                               && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                               && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                                               && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc))
                                                  && VV##1796##k_ == VV##F##7
                                                  && i##aS7##k_ == i##aS7
                                                  && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                  && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb))
                                     && (exists [i##aS7##k_ : int,
                                                 lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                                 lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                                 VV##1793##k_ : int]
                                           . VV##1793##k_ == VV##F##6
                                             && i##aS7##k_ == i##aS7
                                             && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                             && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                             && (exists [VV##F##5 : int]
                                                   . VV##1793##k_ == VV##F##5
                                                     && i##aS7##k_ == i##aS7
                                                     && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                                     && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb))
                                     && VV##1802##k_ == VV##F##6
                                     && i##aS7##k_ == i##aS7
                                     && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                     && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                     && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc))
                     && (exists [lq_anf##7205759403792798200##d1cc##k_ : (Test.State int int),
                                 lq_tmpx##1818##k_ : int,
                                 lq_anf##7205759403792798198##d1ca##k_ : (GHC.Internal.Base.Monad (Test.State int)),
                                 lq_tmpx##1817##k_ : int,
                                 VV##1815##k_ : int,
                                 lq_anf##7205759403792798199##d1cb##k_ : (Test.State int Tuple0),
                                 i##aS7##k_ : int]
                           . VV##1815##k_ == VV##F##13
                             && i##aS7##k_ == i##aS7
                             && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                             && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                             && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                             && lq_tmpx##1817##k_ == w2
                             && lq_tmpx##1818##k_ == x
                             && (exists [VV##F##8 : int,
                                         lq_tmpx##1817 : int,
                                         lq_tmpx##1818 : int,
                                         lq_tmpdb##47 : int,
                                         lq_tmpdb##48 : int]
                                   . lq_tmpx##1818 == VV##F##8
                                     && VV##F##8 == lq_tmpx##1817
                                     && VV##1815##k_ == VV##F##8
                                     && i##aS7##k_ == i##aS7
                                     && lq_anf##7205759403792798198##d1ca##k_ == lq_anf##7205759403792798198##d1ca
                                     && lq_anf##7205759403792798199##d1cb##k_ == lq_anf##7205759403792798199##d1cb
                                     && lq_anf##7205759403792798200##d1cc##k_ == lq_anf##7205759403792798200##d1cc
                                     && lq_tmpx##1817##k_ == lq_tmpx##1817
                                     && lq_tmpx##1818##k_ == lq_tmpx##1818)))
        """
    }
