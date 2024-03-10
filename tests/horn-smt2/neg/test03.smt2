(fixpoint  --eliminate=horn)
 
(qualif Foo ((v (int))) ((>  v  10)))
 
(var $k0 ((int)))
 
 
 
 
(constraint
   (and
      (and
         (forall ((x (int)) ((>  x  0)))
            (forall ((v (int)) ((==  v  x)))
               (($k0  v))))
         (forall ((y (int)) (($k0  y)))
            (forall ((v (int)) ((==  v  (+  y  1))))
               (($k0  v))))
         (forall ((z (int)) (($k0  z)))
            ((>  z  0))))))