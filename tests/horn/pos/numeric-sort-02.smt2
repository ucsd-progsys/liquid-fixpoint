 
(numeric Apple)
 
(qualif Bar ((v @(0)) (z @(1))) (>= v z))
 
(var $k1 (Apple Int))
 
 
 
 
 
(constraint
  (and
    (forall ((zero Int) ((= zero 0)))
      (and
        (forall ((n Apple) (true))
          (forall ((cond bool) ((<=> cond (<= n zero))))
            (and
              (forall ((grd bool) (cond))
                (forall ((VV Apple) ((= VV zero)))
                  ($k1 VV zero)))
              (forall ((grd bool) ((not cond)))
                (forall ((n1 Apple) ((= n1 (- n 1))))
                  (forall ((t1 Apple) ($k1 t1 zero))
                    (forall ((v Apple) ((= v (+ n t1))))
                      ($k1 v zero))))))))
        (forall ((y Apple) (true))
          (forall ((r Apple) ($k1 r zero))
            (forall ((ok1 bool) ((<=> ok1 (<= zero r))))
              (forall ((v bool) (and ((<=> v (<= zero r))) ((= v ok1))))
                (v)))))))))