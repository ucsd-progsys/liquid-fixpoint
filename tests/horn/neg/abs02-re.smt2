(fixpoint "--eliminate=horn")
 
 
 
(var $k_##1 (Int Int))
(var $k_##3 (Int Int))
 
 
 
 
 
(constraint
  (and
    (and
      (forall ((x Int) (true))
        (forall ((pos bool) ((<=> pos (>= x 0))))
          (and
            (forall ((lq_tmp$grd##3 bool) (pos))
              (forall ((VV Int) ((= VV x)))
                ($k_##1 VV x)))
            (forall ((lq_tmp$grd##3 bool) ((not pos)))
              (forall ((v Int) ((= v (- 0 x))))
                ($k_##1 v x))))))
      (forall ((z Int) (true))
        (and
          (forall ((r Int) ((>= r 0)))
            (forall ((v Int) ((= v (+ r 1))))
              ($k_##3 v z)))
          (and
            (forall ((_t1 Int) ((>= _t1 0)))
              (forall ((VV##0 Int) ($k_##1 VV##0 _t1))
                ((>= VV##0 0))))
            (forall ((res Int) ($k_##3 res z))
              (forall ((ok bool) ((<=> ok (<= 6660 res))))
                (forall ((v bool) ((and (<=> v (<= 6660 res)) (= v ok))))
                  (v))))))))))