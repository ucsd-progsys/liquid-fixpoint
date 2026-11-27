(fixpoint "--eliminate=horn")

(qualif Foo ((v Int)) (> v 0))

(var $k0 ((Int)))
(var $k1 ((Int) (Int)))

(constraint
  (and
    (forall ((yyy Int) (true))  
      (forall ((vvv Int) ((= vvv (+ yyy 1)))) 
        ($k1 vvv yyy)))
    (forall ((x Int) ((> x 0)))
      (forall ((v Int) ((= v x)))
        ($k0 v)))
    (forall ((y Int) ($k0 y))
      (forall ((v Int) ($k1 v y))
        ($k0 v)))
    (forall ((z Int) ($k0 z))
      ((> z 0)))))
