(qualif Foo ((v (int))) ((>  v  100)))
 
(var $k0 ((int)))
 
(constant f ((func 0 ((int)) int)))
 
 
 
(constraint
   (and
      (forall ((x (int)) ((>  x  0)))
         (and
            (forall ((v (int)) ((==  v  (f  x))))
               (($k0  v)))
            (forall ((z (int)) (($k0  z)))
               ((==  z  (f  x))))))))