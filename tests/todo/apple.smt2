
(numeric apple)

(qualif Bar ((v 'apple)) (v >= 0))

(var $k1 ((apple)))


(constraint

  (forall ((apple num) (true))
  (and
      (forall ((n apple) (true))
       (forall ((cond bool) (cond <=> n <= 0))
        (and
         (forall ((grd bool) (cond))
          (forall ((VV apple) (VV == 0))
           (($k1 VV))))
         (forall ((grd bool) (not cond))
          (forall ((n1 apple) (n1 == n - 1))
           (forall ((t1 apple) ($k1 t1))
            (forall ((v apple) (v == n + t1))
             (($k1 v)))))))))
      (forall ((y apple) (true))
       (forall ((r apple) ($k1 r))
        (forall ((ok1 bool) (ok1 <=> 0 <= r))
           (forall ((v bool) (and (v <=> 0 <= r) (v == ok1)))
            ((v)))))))))