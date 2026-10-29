; A kvar can have no parameters. $k is a loop invariant, so it must be
; solved with qualifiers. Nothing flows into it, so it is solved to false.

(var $k ())

(constraint
  (and
    (forall ((u Int) ($k))
      ($k))
    (forall ((x Int) ($k))
      ((> x 0)))))
