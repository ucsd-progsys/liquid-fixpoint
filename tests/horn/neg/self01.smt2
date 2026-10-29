; Like pos/self01.smt2, but only the first argument of $k is a self argument,
; so the qualifier cannot be instantiated with b as its self parameter.

(qualif Le ((s Int) (x Int)) (<= s x))

(var $k (Int Int Int))

(constraint
  (and
    (forall ((a Int) (true))
      (forall ((b Int) ((<= b a)))
        (forall ((c Int) ((<= b c)))
          ($k a b c))))
    (forall ((a Int) (true))
      (forall ((b Int) (true))
        (forall ((c Int) ($k a b c))
          (and
            (forall ((a1 Int) ((= a1 (+ a 1))))
              (forall ((c1 Int) ((= c1 (+ c 1))))
                ($k a1 b c1)))
            ((<= b c))))))))
