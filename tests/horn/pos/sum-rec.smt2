(qualif Bar ((v int)) (v >= 0))

(var $k1 ((int) (int)))

(constraint
  (and
      (forall ((n int) (true))
       (forall ((cond bool) (cond <=> n <= 0))
        (and
         (forall ((tmp bool) (cond))
          (forall ((VV int) (VV == 0))
           (($k1 VV n))))
         (forall ((tmp bool) (not cond))
          (forall ((n1 int) (n1 == n - 1))
           (forall ((t1 int) ($k1 t1 n1))
            (forall ((v int) (v == n + t1))
             (($k1 v n1)))))))))
      (forall ((y int) (true))
       (forall ((r int) ($k1 r y))
        (forall ((ok1 bool) (ok1 <=> 0 <= r))
           (forall ((v bool) (and (v <=> 0 <= r) (v == ok1)))
            ((v))))))))
