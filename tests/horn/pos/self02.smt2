; A kvar without self arguments can be solved to false. $k is a loop
; invariant, so it must be solved with qualifiers. There are none, so with a
; self argument $k could only be solved to true.

(var $k (Int) :self 0)

(constraint
  (forall ((x Int) ($k x))
    (and
      (forall ((y Int) ((= y (+ x 1))))
        ($k y))
      ((> x 0)))))
