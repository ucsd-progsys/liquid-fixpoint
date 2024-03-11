 
 
(qualif Bar ((v Int)) (>= v 0))
 
(var $k1 (Int Int))
 
 
 
 
 
(constraint
  (and
    (and
      (forall ((n Int) (true))
        (forall ((cond bool) ((<=> cond (<= n 0))))
          (and
            (forall ((tmp bool) (cond))
              (forall ((VV Int) ((= VV 0)))
                ($k1 VV n)))
            (forall ((tmp bool) ((not cond)))
              (forall ((n1 Int) ((= n1 (- n 1))))
                (forall ((t1 Int) ($k1 t1 n1))
                  (forall ((v Int) ((= v (+ n t1))))
                    ($k1 v n1))))))))
      (forall ((y Int) (true))
        (forall ((r Int) ($k1 r y))
          (forall ((ok1 bool) ((<=> ok1 (<= 0 r))))
            (forall ((v bool) (and ((<=> v (<= 0 r))) ((= v ok1))))
              (v))))))))