; The second self argument of $k can instantiate the self parameter of a
; qualifier, and the other parameters can be instantiated with the first.
; $k is a loop invariant, so it must be solved with qualifiers.

(qualif Le ((s Int) (x Int)) (<= s x))

(var $k (Int Int Int) :self 2)

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
            ((<= b c))
            ((<= b a))))))))
