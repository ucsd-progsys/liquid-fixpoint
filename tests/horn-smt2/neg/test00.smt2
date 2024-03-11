(fixpoint  "--eliminate=horn")
 
 
 
 
 
 
 
(constraint
   (and
      (forall ((x int) ((>  x  0)))
         (forall ((y int) ((>  y  x)))
            (forall ((v int) ((==  v  (+  x  y))))
               ((>  v  10)))))))