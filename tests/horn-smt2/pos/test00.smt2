(fixpoint  --eliminate=horn)
 
(qualif Foo ((v (int)) (x (int))) ((==  v  x)))
(qualif Bar ((v (int)) (x (int))) ((>  v  x)))
 
(var $k1 ((int) (int) (int)))
(var $k2 ((int) (int) (int)))
(var $k3 ((int) (int) (int)))
 
 
 
 
(constraint
   (and
      (forall ((x (int)) ((>  x  0)))
         (forall ((y (int)) ((>  y  x)))
            (forall ((v (int)) ((==  v  (+  x  y))))
               ((>  v  0)))))))