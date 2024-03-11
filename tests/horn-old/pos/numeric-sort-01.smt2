
(numeric Apple)

(qualif Bar ((v a)) (v >= 0))

(var $k1 ((Apple)))


(constraint
  (and
      (forall ((n Apple) (true))
       (forall ((cond bool) (cond <=> n <= 0))
        (and
         (forall ((grd bool) (cond))
          (forall ((VV Apple) (VV == 0))
           (($k1 VV))))
         (forall ((grd bool) (not cond))
          (forall ((n1 Apple) (n1 == n - 1))
           (forall ((t1 Apple) ($k1 t1))
            (forall ((v Apple) (v == n + t1))
             (($k1 v)))))))))
      (forall ((y Apple) (true))
       (forall ((r Apple) ($k1 r))
        (forall ((ok1 bool) (ok1 <=> 0 <= r))
           (forall ((v bool) (and (v <=> 0 <= r) (v == ok1)))
            ((v))))))))
