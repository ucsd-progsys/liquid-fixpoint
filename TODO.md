# TODO

## HORN-SMTLIB

class ToHornSMT a where
  toSMTLIB :: a -> String

parse

- Just change parser?
- Fix tests.

(define adder ((x int)  (y int)) int ((+  x  y)))





(datatype (Vec 1)
 ((VNil ())
  (VCons ((head (@(0)))  (tail ((Vec  @(0))))))))