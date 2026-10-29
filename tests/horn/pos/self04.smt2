; A kvar without parameters that is not on a cycle is eliminated. It is only
; reachable under a contradictory hypothesis, so it proves anything.

(var $k ())

(constraint
  (and
    (forall ((x Int) ((= x 0)))
      (forall ((y Int) ((= x 1)))
        ($k)))
    (forall ((z Int) ($k))
      ((> z 0)))))
