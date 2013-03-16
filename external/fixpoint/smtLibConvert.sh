#!/bin/bash

FIXPOINT=/home/rjhala/research/liquid/liquid-fixpoint/external/fixpoint/fixpoint.native
Z3=/home/rjhala/research/liquid/liquid-fixpoint/external/z3/bin/z3

for f in $1/*.fq; do 
  echo "Processing $f ..."; 
  $FIXPOINT -smtlib $f -out $f.out 
  for g in $f.*.smt2; do
    perl -pi -e 's/#/_/g' $g
    $Z3 $g
  done
done

