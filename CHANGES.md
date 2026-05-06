# CHANGES

## NEXT

## 0.9.6.3.6 (2026-05-06)

- Drop dependency on lens-family [#841](http://github.com/ucsd-progsys/liquid-fixpoint/pull/841)
- Add sort-compatibility check for measure application in PLE [#840](https://github.com/ucsd-progsys/liquid-fixpoint/pull/840)
- Eagerly expand the body of eta-expanded terms [#838](https://github.com/ucsd-progsys/liquid-fixpoint/pull/838)
- Add polymorphic kvar type variable substitution [#837](https://github.com/ucsd-progsys/liquid-fixpoint/pull/837)
- Show wf constraints in prettified fixpoint queries [#836](https://github.com/ucsd-progsys/liquid-fixpoint/pull/836)
- Unapply solutions in fqout files for readability [#835](https://github.com/ucsd-progsys/liquid-fixpoint/pull/835)
- Wildcard literals in qualifiers in horn constraints. [#834](https://github.com/ucsd-progsys/liquid-fixpoint/pull/834)
- Produce a fqout file before PLE [#835](https://github.com/ucsd-progsys/liquid-fixpoint/pull/835)
- Add `--save-dir` flag to specify output directory for generated files [#832](https://github.com/ucsd-progsys/liquid-fixpoint/pull/832)
- Add `--save-bfq-on-error` flag to collect .bfq files on verification failure [#831](https://github.com/ucsd-progsys/liquid-fixpoint/pull/831)
- Escape SMT string literals [#828](https://github.com/ucsd-progsys/liquid-fixpoint/pull/828)
- Add `cut` support to Horn query parser [#827](https://github.com/ucsd-progsys/liquid-fixpoint/pull/827)
- Fix name captures in substitution of PExist and PAll [#826](https://github.com/ucsd-progsys/liquid-fixpoint/pull/826)
- Don't require a space after the comment start [#825](https://github.com/ucsd-progsys/liquid-fixpoint/pull/825)
- Add support for fractional literals [#822](https://github.com/ucsd-progsys/liquid-fixpoint/pull/822) [#824](https://github.com/ucsd-progsys/liquid-fixpoint/pull/824)
- Generalize expressions and substitutions over binding types [#763](https://github.com/ucsd-progsys/liquid-fixpoint/pull/763)

## 0.9.6.3.5 (2026-01-14)

- Implement `--sortedsolution` to keep elaborated sorts in fqout/solution [#821](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/821)
- Retire old parser for horn queries [#820](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/820)
- Stop the parser from simplifying expressions during parsing [#819](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/819)
- Add `--explicitKvars` option and generalize horn syntax to accept expressions for kvars arguments [#818](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/818)
- Provide more comments in the SMT queries to relate them to the source code [#814](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/814)
- Remove `--no-lazy-ple` [#813](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/813)
- Allow PLE to unfold in kvar solutions  [#811](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/811)
- Remove redundant question marks from expression [#807](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/807)
- Remove distinction of predicates and expressions in the parser [#805](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/805)
- Shrink kvar solutions [#799](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/799) [#809](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/809) [#821](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/821)
- Disable the progress bar when not on a terminal [#798](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/798)
- Retire existential binds [#797](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/797)
- Provide stack traces for more crashes [#794](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/794)
- Support string operators [#793](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/793)
- Apply kvar solutions to constraints before PLE [#792](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/792)
- Retire implementation of gradual refinement types [#789](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/789)
- Retire old PLE variations [#788](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/788)
- Add set cardinality support when using cvc5 [#774](https:://github.com/ucsd-progsys/liquid-fixpoint/pull/774)
- Have `--scrape` consider global constants and ADTs [#772](https://github.com/ucsd-progsys/liquid-fixpoint/pull/772)
- Shorten flags of a few flags and add git-version [#762](https://github.com/ucsd-progsys/liquid-fixpoint/pull/762)
- Add support/conversions for Bitv8 and Bitv16 [#759](https://github.com/ucsd-progsys/liquid-fixpoint/pull/759)
- Support the finite field theory of CVC5 [#755](https://github.com/ucsd-progsys/liquid-fixpoint/pull/755)
- Fix SMT crashes on reflected functions on polymorphic data types [#753](https://github.com/ucsd-progsys/liquid-fixpoint/pull/753)
- Allow function names to start with prefix mod [#751](https://github.com/ucsd-progsys/liquid-fixpoint/pull/751)
- Implement let bindings for Horn queries [#748](https://github.com/ucsd-progsys/liquid-fixpoint/pull/748)
- Fix elaboration of `define_fun` declarations [#747](https://github.com/ucsd-progsys/liquid-fixpoint/pull/747) [#749](https://github.com/ucsd-progsys/liquid-fixpoint/pull/749)

## 0.9.6.3.3 (2025-03-22)

- Add support for GHC HEAD (9.13) [#745](https://github.com/ucsd-progsys/liquid-fixpoint/pull/745).
- Expose SMTLIB define-fun to users of liquid-fixpoint [#744](https://github.com/ucsd-progsys/liquid-fixpoint/pull/744).
- Check that expressions in refinements are Bool-sorted [#743](https://github.com/ucsd-progsys/liquid-fixpoint/pull/743).
- Fix crashes when a datatype is declared with a `Map_t` field [#738](https://github.com/ucsd-progsys/liquid-fixpoint/issues/738).
- Simplify expressions in fqout files [#741](https://github.com/ucsd-progsys/liquid-fixpoint/pull/741).

## 0.9.6.3.2 (2025-03-06)

- Expose relatedSymbols from EnvironmentReduction. Needed for improving error
  messages in LH
  [#2346](https://github.com/ucsd-progsys/liquidhaskell/issues/2346).
- Support extensionality in PLE [#704](https://github.com/ucsd-progsys/liquid-fixpoint/pull/704)
- Add a new flag `--etabeta` to reason with lambdas in PLE [#705](https://github.com/ucsd-progsys/liquid-fixpoint/pull/705)
- Add support for reflected lambdas in PLE [#725](https://github.com/ucsd-progsys/liquid-fixpoint/pull/725)
- Implement Bags and Maps reasoning with Arrays [#703](https://github.com/ucsd-progsys/liquid-fixpoint/pull/703)
- Support conditional elaboration of theories for cvc5 [#734](https://github.com/ucsd-progsys/liquid-fixpoint/pull/734)
- Generate smt2 files only when using `--save` [#712](https://github.com/ucsd-progsys/liquid-fixpoint/pull/712)
- Parameterize Expr and Reft by the variable type [#708](https://github.com/ucsd-progsys/liquid-fixpoint/pull/721)
- Preserve location of operators in the parser [#721](https://github.com/ucsd-progsys/liquid-fixpoint/pull/721)
- Optimize elaboration [#736](https://github.com/ucsd-progsys/liquid-fixpoint/pull/736)

## 0.9.6.3.1 (2024-08-21)

- Added support for ghc-9.10.1
- Use `;` for comments in SMTParse (as done in SMTLIB) [#700](https://github.com/ucsd-progsys/liquid-fixpoint/pull/700).
- Extend SMTParser to support lits e.g. for bitvec [#698](https://github.com/ucsd-progsys/liquid-fixpoint/pull/698).
- refactor `Set->Array` elaboration [#696](https://github.com/ucsd-progsys/liquid-fixpoint/pull/696).
- Fixed the polymorphism-related crash caused by a restrictive Set theory encoding [#688](https://github.com/ucsd-progsys/liquid-fixpoint/pull/688).
- Do not constant-fold div by zero [#686](https://github.com/ucsd-progsys/liquid-fixpoint/issue/686).
- Copy over the HOF configuraration options in hornFInfo [#684](https://github.com/ucsd-progsys/liquid-fixpoint/pull/684).
- Use SMTLIB style serialization/deserialization for Horn queries [#683](https://github.com/ucsd-progsys/liquid-fixpoint/pull/683).
- Print SMT preamble to the logfile when constructing context [#681](https://github.com/ucsd-progsys/liquid-fixpoint/pull/681).
- Allow reading/saving horn queries from/to JSON [#680](https://github.com/ucsd-progsys/liquid-fixpoint/pull/680).
- Extend parser to allow boolean function arguments [#678](https://github.com/ucsd-progsys/liquid-fixpoint/pull/678).

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
