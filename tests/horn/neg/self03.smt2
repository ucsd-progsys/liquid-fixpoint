; A kvar can have no parameters. $k is a loop invariant that holds initially,
; so it is solved to true.

(var $k ())

(constraint
  (and
    ($k)
    (forall ((u Int) ($k))
      ($k))
    (forall ((x Int) ($k))
      ((> x 0)))))
