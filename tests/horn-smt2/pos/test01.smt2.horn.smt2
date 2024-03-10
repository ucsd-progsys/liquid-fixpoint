 
 
 
 
 
(constraint
   (and
      (forall ((x (int)) ((>  x  0)))
         (and
            (forall ((y (int)) ((>  y  x)))
               (forall ((v (int)) ((==  v  (+  x  y))))
                  ((>  v  0))))
            (forall ((z (int)) ((>  z  100)))
               (forall ((v (int)) ((==  v  (+  x  z))))
                  ((>  v  100))))))))