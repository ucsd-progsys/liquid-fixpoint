(fixpoint  --eliminate=horn)
 
 
(var $k ((int)))
 
 
 
 
(constraint
   (and
      (forall ((x (int)) ((>=  x  0)))
         (and
            (forall ((v (int)) ((==  v  (-  x  1))))
               (($k  v)))
            (forall ((y (int)) (($k  y)))
               (forall ((v (int)) ((==  v  (+  y  1))))
                  ((>=  v  0))))))))