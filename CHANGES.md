# CHANGES

## NEXT

- Fixed the polymorphism-related crash caused by a restrictive Set theory encoding [#688](https://github.com/ucsd-progsys/liquid-fixpoint/pull/688).

## 0.9.6.3 (2024-01-29)

- For now we stopped folding constants that contain NaN [#670](https://github.com/ucsd-progsys/liquid-fixpoint/pull/670)

## 0.9.4.7

- Support GHC 9.6 tuples with `--extensionality` [#666](https://github.com/ucsd-progsys/liquid-fixpoint/issues/641) [#667](https://github.com/ucsd-progsys/liquid-fixpoint/issues/641)

## 0.9.2.5

- Adopt smtlib-backends for interactions with the SMT solvers [#641](https://github.com/ucsd-progsys/liquid-fixpoint/issues/641)

## 0.9.0.2

- Simplified the equalities dumped by PLE [#569](https://github.com/ucsd-progsys/liquid-fixpoint/issues/569) [#605](https://github.com/ucsd-progsys/liquid-fixpoint/issues/605)
- Add PLE implementation based on interpreting expressions [#502](https://github.com/ucsd-progsys/liquid-fixpoint/pull/502)

## 0.8.10.2

- Dump equalities discovered by PLE [#491](https://github.com/ucsd-progsys/liquid-fixpoint/pull/491) [#569](https://github.com/ucsd-progsys/liquid-fixpoint/issues/569)
- Dump prettified version of constraints [#473](https://github.com/ucsd-progsys/liquid-fixpoint/pull/473)
- Constraints now indicate the source code location that originated them [#471](https://github.com/ucsd-progsys/liquid-fixpoint/pull/471)
- Add support for term rewriting to PLE [#428](https://github.com/ucsd-progsys/liquid-fixpoint/pull/428)

## 0.8.6.4

- Fix bugs in PLE
- Move to GHC 8.6.4
- Add `fuel` parameter to debug unfolding in PLE

## 0.8.0.1

- Support for HORN-NNF format clauses, see `tests/horn/{pos,neg}/*.smt2`
- Support for "existential binders", see `tests/pos/ebind-*.fq` for example.
  This only works with `--eliminate`.
- Move to GHC 8.4.3

## 0.7.0.0

- New `eliminate` based solver (see ICFP 2017 paper for algorithm)
- Proof by Logical Evaluation see `tests/proof`
- SMTLIB2 ADTs to make data constructors injective
- Uniformly support polymorphic functions via `apply` and elaborate

## 0.3.0.0

- Make interpreted mul and div the default, when `solver = z3`
- Use `higherorder` flag to allow higher order binders into the environment

## 0.2.2.0

- Added support for theory of Arrays `Map_t`, `Map_select`, `Map_store`

- Added support for theory of Bitvectors -- see `Language.Fixpoint.Smt.Bitvector`

- Added support for string literals

## 0.2.1.0

- Pre-compiled binaries of the underlying ocaml solver are now
  provided for Linux, Mac OSX, and Windows.

  No more need to install Ocaml!

## 0.2.0.0

- Parsing has been improved to require *much* fewer parentheses.

- Experimental support for Z3's theory of real numbers with the `--real` flag.
