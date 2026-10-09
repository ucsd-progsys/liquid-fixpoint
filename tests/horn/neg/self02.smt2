; A kvar without self arguments can only be solved to true or false, so the
; qualifier is never instantiated for it. $k is a loop invariant, so it must
; be solved with qualifiers.

(qualif Pos ((v Int)) (> v 0))

(var $k (Int) :self 0)

(constraint
  (and
    (forall ((x Int) ((= x 1)))
      ($k x))
    (forall ((x Int) ($k x))
      (and
        (forall ((y Int) ((= y (+ x 1))))
          ($k y))
        ((> x 0))))))
