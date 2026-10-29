; A kvar without parameters that is not on a cycle is eliminated. It only
; records that it is reachable, so it does not prove x > 0.

(var $k ())

(constraint
  (and
    (forall ((x Int) ((= x 0)))
      ($k))
    (forall ((x Int) ($k))
      ((> x 0)))))
