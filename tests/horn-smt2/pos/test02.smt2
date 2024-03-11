(fixpoint  "--eliminate=horn")
 
(qualif Foo ((v (int))) ((>  v  100)))
 
(var $k0 ((int)))
 
 
 
 
 
(constraint
   (and
      (forall ((x int) ((>  x  0)))
         (and
            (forall ((y int) ((>  y  (+  x  100))))
               (forall ((v int) ((==  v  (+  x  y))))
                  (($k0  v))))
            (forall ((z int) (($k0  z)))
               (forall ((v int) ((==  v  (+  x  z))))
                  ((>  v  100))))))))