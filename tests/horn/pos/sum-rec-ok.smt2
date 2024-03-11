 
 
(qualif Bar ((v Int)) (>= v 0))
 
(var $k_##1 (Int))
 
 
 
 
 
(constraint
  (and
    (and
      (forall ((n Int) (true))
        (forall ((cond bool) ((<=> cond (<= n 0))))
          (and
            (forall ((lq_tmp$grd##4 bool) (cond))
              (forall ((VV Int) ((= VV 0)))
                ($k_##1 VV)))
            (forall ((lq_tmp$grd##4 bool) ((not cond)))
              (forall ((n1 Int) ((= n1 (- n 1))))
                (forall ((t1 Int) ($k_##1 t1))
                  (forall ((v Int) ((= v (+ n t1))))
                    ($k_##1 v))))))))
      (forall ((y Int) (true))
        (forall ((r Int) ($k_##1 r))
          (forall ((ok1 bool) ((<=> ok1 (<= 0 r))))
            (forall ((v bool) (and ((<=> v (<= 0 r))) ((= v ok1))))
              (v))))))))