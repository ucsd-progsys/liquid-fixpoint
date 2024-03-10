 
 
(var $ka ((int)))
(var $kb ((int)))
 
 
 
 
(constraint
   (and
      (and
         (exists ((x1 (int)) (true))
            (and
               (forall ((v (int)) ((==  v  1)))
                  ((==  v  x1)))
               (forall ((v (int)) ((==  v  (+  x1  1))))
                  (($ka  v)))))
         (exists ((x2 (int)) (true))
            (and
               (forall ((v (int)) (($ka  v)))
                  ((==  v  x2)))
               (forall ((v (int)) ((==  v  (+  x2  1))))
                  (($kb  v)))))
         (forall ((v (int)) (($kb  v)))
            ((==  v  3))))))