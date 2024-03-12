 
 
 
 
 
 
 
 
(constraint
  (and
    (forall ((x Int) ((> x 0)))
      (forall ((y Int) ((> y x)))
        (forall ((b bool) ((not (<=> b (or (<= x 0) (<= y 0))))))
          (b))))
    (forall ((x Int) ((> x 0)))
      (forall ((y Int) ((> y x)))
        (forall ((b bool) ((not (<=> b (or (<= x 0) (<= y 0))))))
          (b))))))