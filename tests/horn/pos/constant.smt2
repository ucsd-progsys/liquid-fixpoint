 
 
(qualif Foo ((v Int)) (> v 100))
 
(var $k0 (Int))
 
(constant f (func 0 (Int) Int))
 
 
 
 
(constraint
  (and
    (forall ((x Int) ((> x 0)))
      (and
        (forall ((v Int) ((= v (f x))))
          ($k0 v))
        (forall ((z Int) ($k0 z))
          ((= z (f x))))))))