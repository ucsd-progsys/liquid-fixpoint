 
 
(qualif Foo ((v Int)) (> v 0))
 
(var $k0 (Int))
 
 
 
 
 
(constraint
  (and
    (and
      (forall ((x Int) ((> x 0)))
        (forall ((v Int) ((= v x)))
          ($k0 v)))
      (forall ((y Int) ($k0 y))
        (forall ((v Int) ((= v (+ y 1))))
          ($k0 v)))
      (forall ((z Int) ($k0 z))
        ((> z 0))))))