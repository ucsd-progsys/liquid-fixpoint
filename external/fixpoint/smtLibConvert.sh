#!/bin/bash

FIXPOINT=/home/rjhala/research/liquid/liquid-fixpoint/external/fixpoint/fixpoint.native
Z3=/home/rjhala/research/liquid/liquid-fixpoint/external/z3/bin/z3

for f in $1/*.fq; do 
  echo "Processing $f ..."; 
  $FIXPOINT -smtlib $f -out $f.smt2 
  perl -pi -e 's/#/_/g' $f.smt2
  $Z3 $f.smt2
done

