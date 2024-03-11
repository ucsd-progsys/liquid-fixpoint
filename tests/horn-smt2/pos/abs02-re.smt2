 
 
 
 
 
 
 
 
(constraint
  (and
    (and
      (forall ((x Int) (true))
        (forall ((VV Int) ((= VV 10)))
          ((>= VV 0))))
      (forall ((z Int) (true))
        (and
          (forall ((r Int) ((>= r 0)))
            (forall ((v Int) ((and (= v r) (>= v 0))))
              ((>= v 0))))
          (forall ((_t1 Int) ((>= _t1 0)))
            (forall ((v Int) ((>= v 0)))
              ((>= v 0)))))))))