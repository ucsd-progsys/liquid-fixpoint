 
 
 
 
 
 
(constraint
   (and
      (forall ((m (int)) (true))
         (exists ((x1 (int)) (true))
            (and
               (forall ((v (int)) ((==  v  (+  m  1))))
                  ((==  v  x1)))
               (forall ((v (int)) ((==  v  (+  x1  1))))
                  ((==  v  (+  2  m)))))))))